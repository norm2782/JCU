module Main where

import Control.Monad
import Data.Char (isUpper)
import Data.List (intercalate)
import Graphics.UI.WX
import Prolog

main :: IO ()
main = start gui

gui :: IO ()
gui = do -- Application frame 
    f        <- frame [text := "Prolog in Haskell"]
    vlogic   <- variable   [  value := [] ]
    rules    <- textCtrl   f  []
    query    <- textEntry  f  []
    output   <- textCtrl   f  []
    cvas     <- panel f    [  on paint    := draw vlogic query
                           ,  clientSize  := sz 800 500
                           ]
    mfile    <- menuPane   [text := "&File"]
    mopen    <- menuItem   mfile  [  text        := "&Open\tCtrl+O"
                                  ,  help        := "Open a Prolog file"
                                  ,  on command  := onOpen f rules ]
    msave    <- menuItem   mfile  [  text        := "&Save\tCtrl+S"
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
           ,  clientSize := sz 1000 700 ]

runDiag :: (t1 -> Bool -> Bool -> t2 -> [(String, [String])] -> String
        -> String -> t) -> t1 -> t2 -> t
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

onRun :: (Textual a, Textual w1, Textual w2, Valued w, Paint w3)
      => w3 -> w [EnvTrace] -> w1 -> w2 -> a -> t -> IO ()
onRun cvas vlogic rules query output _ = do
    set output  [  text   := "Running..." ]
    set vlogic  [  value  := [] ]
    repaint cvas
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

{-
ouder(X,ama):


    ma(max, ama):-.                pa(alex, ama):-.
    ---------------                ----------------
        ma(X0, Y0)                    pa(X0, Y0)
--------------------------    --------------------------
ouder(X0, Y0):-ma(X0, Y0).    ouder(X0, Y0):-pa(X0, Y0).
--------------------------------------------------------
                    ouder(X,ama)
-}


-- TODO: Clean up and improve drawing code
draw :: (Textual b, Valued w) => w [EnvTrace] -> b -> DC a1 -> t -> IO ()
draw tv query dc va = do
    vl  <- get tv value
    q   <- get query text
    set dc [  fontFace  := "Courier New"
           ,  fontSize  := 14 ]
    if null vl
        then  drawText dc "No solutions yet!" (pt 50 50) []
        else  do  drawText    dc "We have a solution!" (pt 50 50) []
                  drawTraces  dc vl 350

drawTraces :: DC a -> [EnvTrace] -> Int -> IO ()
drawTraces dc trs y = foldM_ drawTraces' 50 trs
    where drawTraces' x (_,tr) = do drawTrace dc tr x
                                    return $ x+300

drawTrace :: DC a -> [Trace] -> Int -> IO ()
drawTrace dc trs x = foldM_ drawTrace' 350 trs
    where drawTrace' y t = do drawText dc (show $ goal t) (pt x y) []
                              line dc (pt 0 (y+15)) (pt 700 (y+15)) []
                              drawText dc (show $ unif t) (pt x (y-20)) []
                              line dc (pt 0 (y-5)) (pt 700 (y-5)) []
                              return $ y-40

append :: Textual a => a -> String -> IO ()
append t s = appendText t $ '\n':s

showSolutions :: Textual a => a -> [EnvTrace] -> IO ()
showSolutions t es = sequence_ [ showSolution t etr | etr <- es]
    where showSolution t (bs, trace) = do  mapM_ (append t . show) trace
                                           append t $ concatMap (showBdg bs) bs
