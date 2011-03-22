module Main where

import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Char (isUpper)
import Data.List (intercalate)
import Graphics.UI.WX
import Prolog

data AppState = AppState { currentFile :: String }

main :: IO ()
main = start gui

gui :: IO ()
gui = do -- Application frame 
    f        <- frame [text := "Prolog in Haskell"]
    vlogic   <- variable   [  value := [] ]
    cvas     <- panel f    [  on paint    := draw vlogic
                           ,  clientSize  := sz 200 200 ]
    rules    <- textCtrl   f  []
    query    <- textEntry  f  []
    output   <- textCtrl   f  []
    mfile    <- menuPane   [text := "&File"]
    mopen    <- menuItem   mfile  [  text        := "&Open"
                                  ,  help        := "Open a Prolog file"
                                  ,  on command  := onOpen f rules ]
    msave    <- menuItem   mfile  [  text        := "&Save"
                                  ,  help        := "Save a Prolog file"
                                  ,  on command  := onSave f rules ]
    msaveas  <- menuItem   mfile  [  text        := "&Save As"
                                  ,  help        := "Save As a Prolog file"
                                  ,  on command  := onSaveAs f rules ]
    mquit    <- menuQuit   mfile  [  text        := "&Quit"
                                  ,  help        := "Quit the program"
                                  ,  on command  := close f ]
    run      <- button     f  [  text := "Run!"
                              ,  on command ::= onRun cvas vlogic rules query output ]
    set f  [  menuBar  := [mfile]
           ,  layout   := column 5 [ boxed "Enter rules and queries, press Run and be amazed!"
                                       (grid 5 5 [
                                              [label "Canvas:",  hfill $ widget cvas] 
                                           ,  [label "Rules:",   hfill $ widget rules]
                                           ,  [label "Query:",   hfill $ widget query]
                                           ,  [label "Output:",  hfill $ widget output]
                                           ,  [widget run]
                                           ])
                                       ]
           ,  clientSize := sz 800 600 ]

runDiag diag f hdr =  diag f True True hdr
                          [("Prolog files (*.pro, *.pl)", ["*.pro", "*.pl"])]
                          "" ""

onOpen :: Textual w => Window a -> w -> IO ()
onOpen f rules = do
    diag <- runDiag fileOpenDialog f "Select Prolog file"
    case diag of
        Nothing  -> return () -- TODO: Nice error handling
        Just f   -> do  cnts <- readFile f
                        set rules [ text := cnts ]
    
onSave :: Textual w => Window a -> w -> IO ()
onSave f rules = do
    diag <- runDiag fileSaveDialog f "Save Prolog file"
    case diag of
        Nothing  -> return () -- TODO: Nice handling
        Just f   -> do  rs <- get rules text
                        writeFile f rs

onSaveAs :: Textual w => Window a -> w -> IO ()
onSaveAs = onSave

onRun cvas vlogic rules query output _ = do
    set output [ text := "Running..." ]
    rs <- get rules text
    let (rules, rerr) = startParse pRules rs
    if null rerr
        then  do qs <- get query text
                 let (goal, ferr) = startParse pFun qs
                 if null ferr
                     then  do  append output "Done!"
                               let sol = solve rules [goal] [] 0
                               set vlogic [ value := sol ]
                               showSolutions output sol
                               repaint cvas
                     else  append output $ "Invalid query: " ++ qs
        else  append output $ "Errors in parsing rules! " ++ show rerr

draw :: TVar [a] -> DC b -> t -> IO ()
draw tv dc va = do
    vl <- atomically $ readTVar tv
    circle dc (pt 200 200) 20 [penKind := PenDash DashDot]
    arc dc (pt 100 100) 20 90 230 [color := red, penWidth :~ (+1), penKind := PenSolid]
    if null vl -- vl contains the solve results! honestly!
        then ellipticArc dc (rect  (pt 20  20) (sz 60 30)) 90 230 [color := blue, penWidth :~ (*2)]
        else ellipticArc dc (rect  (pt 20  20) (sz 60 30)) 90 230 [color := black, penWidth :~ (*2)]

    c <- get dc color
    set dc [fontFace := "Courier New", fontSize := 16, fontWeight := WeightBold ]
    drawText dc (show c) (pt 50 50) []
    rotatedText dc "rotated text" (pt 80 160) 45 [textColor := green]

append :: Textual a => a -> String -> IO ()
append t s = appendText t $ '\n':s

showSolutions :: Textual a => a -> [EnvTrace] -> IO ()
showSolutions t es = sequence_ [ showSolution t etr | etr <- es]
    where showSolution t (bs, trace) = do  mapM_ (append t . show) trace
                                           append t $ concatMap (showBdg bs) bs
