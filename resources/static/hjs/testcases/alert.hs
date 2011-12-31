module Main where
  
import Language.UHC.JScript.Assorted (alert , _alert)
import Language.UHC.JScript.Types

import Language.UHC.JScript.ECMA.String

  
foreign import jscript "window.location.host"
  windowLocationHost :: JSString

main :: IO ()
main = do _alert windowLocationHost
          alert (fromJS windowLocationHost)
          
        