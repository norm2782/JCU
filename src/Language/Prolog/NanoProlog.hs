{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Prolog.NanoProlog where

import            Data.Maybe
import            Data.ListLike.Base (ListLike)
import            Data.List (intercalate,lookup)
import            Data.Tree (Tree(..))
import            Text.ParserCombinators.UU
import            Text.ParserCombinators.UU.BasicInstances
import            Text.ParserCombinators.UU.Utils
import            Data.Tree (Tree(..))
import            System.IO
import            Debug.Trace


-- * Basic Data Types
type UpperCase  = String
type LowerCase  = String

data Term  =  Var UpperCase
           |  Fun LowerCase [Term]
           deriving (Eq, Ord)

data Rule  =  Term :<-: [Term]deriving Eq

class Taggable a where
  tag :: Int -> a -> a

instance Taggable Term where
  tag n  (Var  x)     = Var  (x ++ show n)
  tag n  (Fun  x xs)  = Fun  x (map (tag n) xs)

instance Taggable Rule where
  tag n (c :<-: cs) = tag n c :<-: map (tag n) cs

type Env = [(UpperCase, Term)]

emptyEnv :: Maybe Env
emptyEnv = Just []

-- * The Prolog machinery

subst :: Env -> Term -> Term
subst env (Var x)     = maybe (Var x) (subst env) (lookup x env)
subst env (Fun x cs)  = Fun x (map (subst env) cs)

unify :: (Term, Term) -> Maybe Env -> Maybe Env
unify _       Nothing       = Nothing
unify (t, u)  env@(Just e)  = uni (subst e t) (subst e u)
  where  uni (Var x) y          =  Just ((x, y): e)
         uni x       (Var y)    =  Just ((y, x): e)
         uni (Fun x xs) (Fun y ys)
           | x == y && length xs == length ys  = foldr unify env (zip xs ys)
           | otherwise                         = Nothing
         uni _ _              =  Nothing

solve :: [Rule] -> Maybe Env -> Int -> [Term] -> [Env]
solve _      Nothing  _  _       =  []
solve _      e        _  []      =  [fromJust e]
solve rules  e        n  (t:ts)  =
  [  sol |  c :<-: cs   <- map (tag n) rules
         ,  sol         <- solve rules (unify (t, c) e) (n+1) (cs ++ ts) 
  ]

-- * Running the Interpreter, reading data base and printing solutions
main :: IO ()          
main = 
 do  hSetBuffering stdin LineBuffering
     putStr "File with rules? "
     fn <- getLine
     s <- readFile fn
     let (rules, errors) = startParse (pList pRule)  s
     if Prelude.null errors then  do  mapM_ (putStrLn.show) rules
                                      loop rules  
                            else  do  putStrLn "No rules parsed"
                                      mapM_ (putStrLn.show) errors
                                      main

loop rules =  do  putStr "term? "
                  s <- getLine
                  if  (s == "quit") then return ()
                   else do  let (goal, errors) = startParse pFun s 
                            if null errors then 
                                 do print goal
                                    printsolutions (solve rules  emptyEnv 0 [goal])
                               else do  putStrLn "A term was expected:"
                                        mapM_ (putStrLn.show) errors
                            loop rules

-- ** Printing the solutions

printsolutions :: [Env] -> IO () 
printsolutions sols = sequence_ [ do {printsolution bs; getLine} |bs <- sols]

printsolution bs =  putStr (concat . map  showBdg $ bs) 
 where  showBdg (    x,t)  | isGlobVar x =  x ++ " = "++ showTerm t ++ "\n" 
                           | otherwise = ""   
        showTerm t@(Var _)  = showTerm (subst bs t) 
        showTerm (Fun f []) = f 
        showTerm (Fun f ts) = f ++"("++ (intercalate ", " (map showTerm ts)) ++ ")"
        isGlobVar x = head x `elem` ['A'..'Z'] && last x `notElem` ['0'..'9']   

instance Show Term where
  show (Var  i)      = i
  show (Fun  i [] )  = i
  show (Fun  i ts )  = i ++ "(" ++ showCommas ts ++ ")"

instance Show Rule where
  show (t :<-: [] ) = show t ++ "."
  show (t :<-: ts ) = show t ++ ":-" ++ showCommas ts ++ "."


showCommas :: Show a => [a] -> String
showCommas l = intercalate ", " (map show l)

-- ** Parsing Rules and Terms
startParse :: (ListLike s b, Show b)  => P (Str b s LineColPos) a -> s
                                      -> (a, [Error LineColPos])
startParse p inp  =  parse ((,) <$> p <*> pEnd)
                  $  createStr (LineColPos 0 0 0) inp

pTerm, pVar, pFun :: Parser Term
pTerm  = pVar  <|>  pFun
pVar   = Var   <$>  lexeme (pList1 pUpper)
pFun   = Fun   <$>  pLowerCase <*> (pParens pTerms `opt` [])
         where pLowerCase :: Parser String
               pLowerCase = (:) <$> pLower 
                                <*> lexeme (pList (pLetter <|> pDigit))

pRule  :: Parser Rule
pRule  =  (:<-:) <$> pFun <*> (pSymbol ":-" *> pTerms `opt` []) <* pDot

pTerms :: Parser [Term]
pTerms =  pListSep pComma pTerm


