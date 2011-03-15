{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Main where

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

data Rule   =   Term :<-: [Term]

type Env       = [(Ident, Term)]
type Trace     = [String]
type EnvTrace  = (Env, Trace)


instance Show Term where
  show (Con i)     =  show  i
  show (Var i)     =        i
  show (Fun i [])  =        i
  show (Fun i ts)  =  i ++ "(" ++ showCommas ts ++ ")"

instance Show Rule where
  show (t :<-: ts) = show t ++ ":-" ++ showCommas ts ++"."

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

unify ::  (Term, Term) ->  Maybe Env -> Maybe Env                         
unify  _       Nothing       =  Nothing
unify  (t, u)  env@(Just e)  =  trace  ("unifying: " ++ show t ++ " " ++ show u ++ "\n")
                                       (uni (lookUp t e) (lookUp u e))
  where  uni (Var x) y          =  Just ((x, y): e)
         uni x (Var y)          =  Just ((y, x): e)
         uni (Con x) (Con y)    =  if  x == y then env else Nothing
         uni (Fun x xs) (Fun y ys) 
           | x == y && length xs == length ys  =  foldr unify env (zip xs ys)
           | otherwise                         =  Nothing
         uni _ _                =  Nothing

solve :: [Rule] -> [Term] -> Env -> Int -> [EnvTrace]
solve rules []         e  _  =  [(e, [])]
solve rules lt@(t:ts)  e  n  = 
    [  (sol, msg:trace)  
    |  (c :<-: cs)  <- map (tag n) rules
    ,  Just r       <- [unify (t, c) (Just e)]
    ,  let msg =  display "goal                  : " t            ++
                  display "unifies with head of  : " (c :<-: cs)  ++
                  display "new environment       : " r            ++
                  display "new goals             : " (cs ++ ts)   ++ "\n"
    ,  (sol, trace) <- solve rules (cs ++ ts) r (n+1)]

display :: Show a => String -> a -> String
display string value = string ++ show value ++ "\n"

main :: IO ()          
main = 
 do  hSetBuffering stdin LineBuffering
     putStr "File with rules? "
     fn  <- getLine
     s   <- readFile fn
     let (rules, errors) = start (pList pRule) s
     if null errors 
         then  do  mapM_ print rules
                   loop rules  
         else  do  putStrLn "No rules parsed"
                   mapM_ print errors
                   main

loop :: [Rule] -> IO ()
loop rules =  do  putStr "term? "
                  s <- getLine
                  unless (s == "stop") $
                      do  let (goal, errors) = start pFun s 
                          if null errors
                              then  printsolutions (solve rules [goal] [] 0)
                              else  do  putStrLn "A term was expected:"
                                        mapM_ print errors
                          loop rules

pRule :: Parser Rule
pRule = (:<-:)  <$> pFun <*> ((pToken ":-" *> pTerms) `opt` []) <* pDot

pTerm, pCon, pVar, pFun :: Parser Term
pTerm  =  pCon  <|>  pVar <|> pFun
pCon   =  Con   <$>  pNatural
pVar   =  Var   <$>  pList1 pUpper
pFun   =  Fun   <$>  pIdentifier <*> (pParens pTerms `opt` [])

pTerms :: Parser [Term]
pTerms =  pListSep pComma pTerm

start :: Parser a -> String -> (a, [Error LineColPos])
start p inp = parse ((,) <$> p <*> pEnd)  $  createStr (LineColPos 0 0 0) 
                                          .  filter (not . isSpace) $ inp 
             
pIdentifier :: Parser String
pIdentifier = (:) <$> pLower <*> pList (pLower <|> pUpper <|> pDigit)

printsolutions :: [EnvTrace] -> IO () 
printsolutions sols = sequence_ [ printGetLn bs | bs <- sols]
    where printGetLn bs = do  printsolution bs
                              getLine

printsolution :: EnvTrace -> IO ()
printsolution (bs, trace) = do  mapM_ putStr trace
                                putStr (concatMap showBdg bs) 
 where  showBdg (x, t)  |  isUpper (head x) && length x == 1 =  x ++ " = " ++ showTerm t ++ "\n"
                        |  otherwise = ""  
        showTerm (Con n)     = show n 
        showTerm t@(Var _)   = showTerm (lookUp t bs) 
        showTerm (Fun f [])  = f 
        showTerm (Fun f ts)  = f ++ "(" ++ intercalate ", " (map showTerm ts) ++ ")"

