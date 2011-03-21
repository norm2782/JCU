module Main where

import Data.Char (isUpper)
import Data.List (intercalate)
import Graphics.UI.WX
import Prolog

main :: IO ()
main = start gui

gui :: IO ()
gui = do -- Application frame 
    frame   <- frame [text := "Prolog in Haskell"]
    rules   <- textCtrl  frame  []
    query   <- textCtrl  frame  []
    output  <- textCtrl  frame  []
    run     <- button    frame  [  text := "Run!"
                                ,  on command ::= onRun rules query output
                                ]
    clear   <- button    frame  [  text := "Clear"
                                ,  on command ::= onClear rules query output
                                ]
    set frame  [  layout := column 5  [ boxed "" (grid 5 5 [
                                             [label "Rules:",   hfill $ widget rules]
                                          ,  [label "Query:",   hfill $ widget query]
                                          ,  [label "Output:",  hfill $ widget output]
                                          ,  [widget run]
                                          ,  [widget clear]
                                          ])
                                      ]
               ,  clientSize := sz 800 600
               ]

onRun :: (Textual a, Textual b, Textual c) => a -> b -> c -> d -> IO ()
onRun rules query output _ = do
    set output [ text := "Running..." ]
    rs <- get rules text
    let (rules, rerr) = startParse pRules rs
    if null rerr
        then  do qs <- get query text
                 let (goal, ferr) = startParse pFun qs
                 if null ferr
                     then  do  append output "Done!"
                               showSolutions output $ solve rules [goal] [] 0
                     else  append output $ "Invalid query: " ++ qs
        else  append output $ "Errors in parsing rules!" ++ show rerr

append :: Textual a => a -> String -> IO ()
append t s = do
    txt <- get t text
    set t [ text := txt ++ '\n':s ]

onClear :: (Textual a, Textual b, Textual c) => a -> b -> c -> d -> IO ()
onClear rules query output _ = do
    set rules   [ text := "" ]
    set query   [ text := "" ]
    set output  [ text := "" ]

showSolutions :: Textual a => a -> [EnvTrace] -> IO ()
showSolutions t es = sequence_ [ showSolution t bs | bs <- es]

showSolution :: Textual a => a -> EnvTrace -> IO ()
showSolution t (bs, trace) = do  mapM_ (append t) trace
                                 append t $ concatMap showBdg bs
 where  showBdg (x, t)  |  isUpper (head x) && length x == 1 =  x ++ " = " ++ showTerm t ++ "\n"
                        |  otherwise = ""  
        showTerm (Con n)     = show n 
        showTerm t@(Var _)   = showTerm (lookUp t bs) 
        showTerm (Fun f [])  = f 
        showTerm (Fun f ts)  = f ++ "(" ++ intercalate ", " (map showTerm ts) ++ ")"
