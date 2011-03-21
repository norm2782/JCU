import Graphics.UI.WX
import Graphics.UI.WXCore 

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
                                             [label "Rules:",  hfill $ widget rules]
                                          ,  [label "Query:",  hfill $ widget query]
                                          ,  [label "Output:", hfill $ widget output]
                                          ,  [widget run]
                                          ,  [widget clear]
                                          ])
                                      ]
               ,  clientSize := sz 800 600
               ]

onRun rules query output b = do
    rs  <- get  rules   text
    qs  <- get  query   text
    os  <- get  output  text
    return ()

onClear rules query output b = do
    set rules   [ text := "" ]
    set query   [ text := "" ]
    set output  [ text := "" ]
    return ()
