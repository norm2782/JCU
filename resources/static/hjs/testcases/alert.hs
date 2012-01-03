module Main where

import Language.UHC.JScript.Assorted (alert)
import Language.UHC.JScript.Types (fromJS)

import Language.UHC.JScript.ECMA.String (JSString)

foreign import jscript "window.location.href"
  windowHref :: JSString

main :: IO ()
main = alert (fromJS windowHref)
