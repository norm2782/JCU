module Main where

import Control.Monad (unless)
import Data.Char (isUpper)
import Data.List
import Prolog
import System.IO

main :: IO () 
main = 
 do  hSetBuffering stdin LineBuffering
     putStr "File with rules? "
     fn  <- getLine
     s   <- readFile fn
     let (rules, errors) = startParse pRules s
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
                      do  let (goal, errors) = startParse pFun s 
                          if null errors
                              then  printsolutions (solve rules [goal] [] 0)
                              else  do  putStrLn "A term was expected:"
                                        mapM_ print errors
                          loop rules

printsolutions :: [EnvTrace] -> IO () 
printsolutions sols = sequence_ [ printGetLn etr | etr <- sols]
    where printGetLn (bs, trace) = do  mapM_ (putStr . show) trace
                                       putStr (concatMap (showBdg bs) bs)
                                       getLine

