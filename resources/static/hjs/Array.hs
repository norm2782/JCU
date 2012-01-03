module Array where

import Language.UHC.JScript.Primitives

data JSArrayPtr a
type JSArray a = JSPtr (JSArrayPtr a) 

