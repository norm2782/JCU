module Main where

import Data.Char (isUpper)
import Data.List (intercalate)
import Graphics.UI.WX
import Prolog

main :: IO ()
main = start gui

gui :: IO ()
gui = do -- Application frame 
    frame    <- frame [text := "Prolog in Haskell"]
    cvas     <- panel frame  [  on paint    := onPaint
                             ,  clientSize  := sz 200 200
                             ]
    rules    <- textCtrl  frame  []
    query    <- textCtrl  frame  []
    output   <- textCtrl  frame  []
    file     <- menuPane  [text := "&File"]
    mopen    <- menuItem  file   [  text        := "&Open"
                                 ,  help        := "Open a Prolog file"
                                 ,  on command  := onOpen frame rules
                                 ]
    msave    <- menuItem  file   [  text        := "&Save"
                                 ,  help        := "Save a Prolog file"
                                 ,  on command  := onSave frame rules
                                 ]
    msaveas  <- menuItem  file   [  text        := "&Save As"
                                 ,  help        := "Save As a Prolog file"
                                 ,  on command  := onSaveAs frame rules
                                 ]
    mquit    <- menuItem  file   [  text        := "&Quit"
                                 ,  help        := "Quit the program"
                                 ,  on command  := close frame
                                 ]
    run      <- button    frame  [  text := "Run!"
                                 ,  on command ::= onRun rules query output
                                 ]
    clear    <- button    frame  [  text := "Clear"
                                 ,  on command ::= onClear rules query output
                                 ]
    set frame  [  menuBar  := [file]
               ,  layout   := column 5  [ boxed "Enter rules and queries, press Run and be amazed!"
                                            (grid 5 5 [
                                               [label "Canvas:",  hfill $ widget cvas] 
                                            ,  [label "Rules:",   hfill $ widget rules]
                                            ,  [label "Query:",   hfill $ widget query]
                                            ,  [label "Output:",  hfill $ widget output]
                                            ,  [widget run]
                                            ,  [widget clear]
                                            ])
                                        ]
               ,  clientSize := sz 800 600
               ]

fileFilter :: [(String, [String])]
fileFilter = [("Prolog files (*.pro, *.pl)", ["*.pro", "*.pl"])]

runDiag diag frame hdr = diag frame True True hdr fileFilter "" ""

onOpen :: Textual w => Window a -> w -> IO ()
onOpen frame rules = do
    diag <- runDiag fileOpenDialog frame "Select Prolog file"
    case diag of
        Nothing  -> return () -- TODO: Nice error handling
        Just f   -> do  cnts <- readFile f
                        set rules [ text := cnts ]
    
onSave :: Textual w => Window a -> w -> IO ()
onSave frame rules = do
    diag <- runDiag fileSaveDialog frame "Save Prolog file"
    case diag of
        Nothing  -> return () -- TODO: Nice handling
        Just f   -> do  rs <- get rules text
                        writeFile f rs

onSaveAs :: Textual w => Window a -> w -> IO ()
onSaveAs = onSave

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
        else  append output $ "Errors in parsing rules! " ++ show rerr

onPaint cvas area = undefined

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
showSolutions t es = sequence_ [ showSolution t etr | etr <- es]
    where showSolution t (bs, trace) = do  mapM_ (append t . show) trace
                                           append t $ concatMap (showBdg bs) bs
