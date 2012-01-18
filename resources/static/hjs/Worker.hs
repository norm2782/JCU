module Main where

import Language.UHC.JScript.WebWorker 

import Language.UHC.JScript.Prelude
import Language.Prolog.NanoProlog.NanoProlog

import Prolog

main = do self <- getSelf
          setOnMessage self doCheck 
          return ()

doCheck :: JSPtr a -> IO ()
doCheck obj = do (proof, rules) <- getAttr "data" obj 
                 -- let (proof, rules) = (read :: String -> (Proof, [Rule])) str
                 self <- getSelf
                 postMessage self $ checkProof rules proof

foreign import jscript "JSON.parse(%1)"
  jsonParse :: JSString -> IO a