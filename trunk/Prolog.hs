{-# LANGUAGE Rank2Types, FlexibleContexts #-}

-- TODO: Do not strip whitespace, but just deal with it. Otherwise the error
-- messages are useless!
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

data Rule   =  Term :<-: [Term]
data Trace  =  Trace Term Rule Env [Term]

type Env       = [(Ident, Term)]
type EnvTrace  = (Env, [Trace])


instance Show Term where
  show (Con i)     =  show  i
  show (Var i)     =        i
  show (Fun i [])  =        i
  show (Fun i ts)  =  i ++ "(" ++ showCommas ts ++ ")"

instance Show Rule where
  show (t :<-: ts) = show t ++ ":-" ++ showCommas ts ++ "."

instance Show Trace where
    show (Trace t r e ts) = display "goal                  : " t   ++
                            display "unifies with head of  : " r   ++
                            display "new environment       : " e   ++
                            display "new goals             : " ts  ++ "\n"
                        where display string value = string ++ show value ++ "\n"

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

-- TODO: Grab the thing in the trace function somehow and extract it, so we can
-- do stuff with it
unify ::  (Term, Term) ->  Maybe Env -> Maybe Env                         
unify  _       Nothing       =  Nothing
unify  (t, u)  env@(Just e)  =  trace  ("unifying: " ++ show t ++ " " ++ show u ++ "\n")
                                       (uni (lookUp t e) (lookUp u e))
  where  uni (Var x) y          =  Just ((x, y): e)
         uni x (Var y)          =  Just ((y, x): e)
         uni (Con x) (Con y)    =  if  x == y then env else Nothing
         uni (Fun x xs) (Fun y ys) 
           | x == y && length xs == length ys  = foldr unify env (zip xs ys)
           | otherwise                         = Nothing
         uni _ _                =  Nothing

solve :: [Rule] -> [Term] -> Env -> Int -> [EnvTrace]
solve rules []         e  _  =  [(e, [])]
solve rules lt@(t:ts)  e  n  = 
    [  (sol, trc:trace)  
    |  tm@(c :<-: cs)   <- map (tag n) rules
    ,  Just r           <- [unify (t, c) (Just e)]
    ,  let trc = Trace t tm r (cs ++ ts)
    ,  (sol, trace)     <- solve rules (cs ++ ts) r (n+1)
    ]

pRules :: Parser [Rule]
pRules = pList pRule

pRule :: Parser Rule
pRule = (:<-:)  <$> pFun <*> ((pToken ":-" *> pTerms) `opt` []) <* pDot

pTerm, pCon, pVar, pFun :: Parser Term
pTerm  =  pCon  <|>  pVar <|> pFun
pCon   =  Con   <$>  pNatural
pVar   =  Var   <$>  pList1 pUpper
pFun   =  Fun   <$>  pIdentifier <*> (pParens pTerms `opt` [])

pTerms :: Parser [Term]
pTerms =  pListSep pComma pTerm

startParse :: Parser a -> String -> (a, [Error LineColPos])
startParse p inp = parse ((,) <$> p <*> pEnd)  $  createStr (LineColPos 0 0 0) 
                                               .  filter (not . isSpace) $ inp 
             
pIdentifier :: Parser String
pIdentifier = (:) <$> pLower <*> pList (pLower <|> pUpper <|> pDigit)

showBdg :: Env -> (Ident, Term) -> String
showBdg bs (x, t)  |  isUpper (head x) && length x == 1 =  x ++ " = " ++ showTerm bs t ++ "\n"
                   |  otherwise = ""

showTerm :: Env -> Term -> String
showTerm _   (Con n)     = show n 
showTerm bs  t@(Var _)   = showTerm bs (lookUp t bs) 
showTerm _   (Fun f [])  = f 
showTerm bs  (Fun f ts)  = f ++ "(" ++ intercalate ", " (map (showTerm bs) ts) ++ ")"
