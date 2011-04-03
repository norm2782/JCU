{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Prolog where

import Data.Char (isUpper, isSpace)
import Data.List
import Debug.Trace
import System.IO
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils

type Ident  =  String

data Term   =  Con Int
            |  Var Ident
            |  Fun Ident [Term]
            deriving Eq

data Rule   =  Term   :<-: [Term]
data Trace  =  Trace  { goal   :: Term
                      , unif   :: Rule
                      , env    :: Env
                      , terms  :: [Term] }

type Env       = [(Ident, Term)]
type EnvTrace  = (Env, [Trace])

instance Show Term where
  show (Con  i)      =  show  i
  show (Var  i)      =        i
  show (Fun  i  [])  =        i
  show (Fun  i  ts)  =  i ++ "(" ++ showCommas ts ++ ")"

instance Show Rule where
  show (t :<-: []) = show t ++ "."
  show (t :<-: ts) = show t ++ ":-" ++ showCommas ts ++ "."

instance Show Trace where
    show (Trace t r e ts) =  display "goal                  : " t   ++
                             display "unifies with head of  : " r   ++
                             display "new environment       : " e   ++
                             display "new goals             : " ts  ++ "\n"
                        where display str val = str ++ show val ++ "\n"

showCommas :: Show a => [a] -> String
showCommas l = intercalate ", " (map show l)

lookUp :: Term -> Env -> Term
lookUp  (Var x)  e   =  case lookup x e of
                            Nothing   -> Var x
                            Just res  -> lookUp res e
lookUp  t        _   =  t

class Taggable a where
  tag :: Int -> a -> a

instance Taggable Term where
  tag n (Con  x)     = Con  x
  tag n (Var  x)     = Var  (x ++ show n)
  tag n (Fun  x xs)  = Fun  x (map (tag n) xs)

instance Taggable Rule where
  tag n (c :<-: cs) = tag n c :<-: map (tag n) cs

unify :: (Term, Term) -> Maybe Env -> Maybe Env
unify _       Nothing       = Nothing
unify (t, u)  env@(Just e)  = trace  ("unifying: " ++ show t ++ " " ++ show u ++ "\n")
                                     (uni (lookUp t e) (lookUp u e))
  where  uni (Var x) y          =  Just ((x, y): e)
         uni x (Var y)          =  Just ((y, x): e)
         uni (Con x) (Con y)    =  if x == y then env else Nothing
         uni (Fun x xs) (Fun y ys)
           | x == y && length xs == length ys  = foldr unify env (zip xs ys)
           | otherwise                         = Nothing
         uni _ _                =  Nothing

solve :: [Rule] -> [Term] -> Env -> Int -> [EnvTrace]
solve rules []      e _  = [(e, [])]
solve rules (t:ts)  e n  =
    [  (sol, trc:trace)
    |  tm@(c :<-: cs)   <- map (tag n) rules
    ,  Just r           <- [unify (t, c) (Just e)]
    ,  let trc = Trace t tm r (cs ++ ts)
    ,  (sol, trace)     <- solve rules (cs ++ ts) r (n+1)
    ]

-- Solving individual rules:
-- We need the right-hand side of the rule, since the left-hand side would bring
-- us back to where we were.
-- In order to solve the right-hand side, we need the environment with variables
-- which map terms like X0 and Y0 to concrete values.
-- Actually, we probably don't even want to show X0 and Y0, but concrete values
-- instead?
-- We need to take the environments into account.

pRules :: Parser [Rule]
pRules = pList pRule

pRule :: Parser Rule
pRule = (:<-:) <$> pFun  <*> ((pSpaces *> pToken ":-" <* pSpaces *> pTerms) `opt` [])
                         <* pSpaces <* pDot

pTerm, pCon, pVar, pFun :: Parser Term
pTerm  =  pCon  <|>  pVar <|> pFun
pCon   =  Con   <$>  pNatural <* pSpaces
pVar   =  Var   <$>  pList1 pUpper <* pSpaces
pFun   =  Fun   <$>  (pIdentifier <* pSpaces) <*> (pParens pTerms `opt` [])

pTerms :: Parser [Term]
pTerms = pListSep pComma (pSpaces *> pTerm <* pSpaces)

startParse :: Parser a -> String -> (a, [Error LineColPos])
startParse p inp = parse ((,) <$> p <*> pEnd) $ createStr (LineColPos 0 0 0) inp

pIdentifier :: Parser String
pIdentifier = (:) <$> pLower <*> pList (pLower <|> pUpper <|> pDigit) <* pSpaces

showBdg :: Env -> (Ident, Term) -> String
showBdg bs (x, t)  | isUpper (head x) && length x == 1  = x ++ " = " ++ showTerm t ++ "\n"
                   | otherwise                          = ""
  where showTerm :: Term -> String
        showTerm (Con n)     = show n
        showTerm t@(Var _)   = showTerm (lookUp t bs) 
        showTerm (Fun f [])  = f
        showTerm (Fun f ts)  = f ++ "(" ++ intercalate ", " (map showTerm ts) ++ ")"
