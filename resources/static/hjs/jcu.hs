{-# LANGUAGE EmptyDataDecls #-}
module JCU where

import Language.UHC.JScript.Types (toJS)
import Language.UHC.JScript.Primitives
import Language.UHC.JScript.JQuery.JQuery
import Language.UHC.JScript.W3C.HTML5 as HTML5

import Language.UHC.JScript.Assorted (alert)

import Language.UHC.JScript.JQuery.Ajax
import qualified Language.UHC.JScript.JQuery.AjaxQueue as AQ
----
--  App
----

import Templates
import Models

ajaxQ :: String -> JSFunPtr (JSPtr a -> IO()) -> JSFunPtr (JSPtr a -> IO()) -> IO ()
ajaxQ url onSuccess onFail = do
  AQ.ajaxQ "jcu_app" $  AjaxOptions { ao_url         = url,
                                      ao_requestType = "POST",
                                      ao_contentType = "application/json",
                                      ao_dataType    = "json",
                                      ao_success     = onSuccess,
                                      ao_failure     = onFail
                                    }
  alert "meh"
  
    
register_events :: [(String, JEventType, JEventHandler)] -> IO ()    
register_events = mapM_ (\ (e, event, eh) -> do elem <- jQuery e
                                                bind elem
                                                     event 
                                                     eh)
    
-- main :: IO ()
-- main = do init    <- ioWrap initialize
--           loadJS' <- ioWrap $ loadJS init
-- 
--           onWindowLoad $ loadJS'
--           init   <- ioWrap initialize
--           
-- foreign import jscript "window.onload" 
--   onWindowLoad :: JSFunPtr (IO ()) -> IO ()
-- 
-- loadJS :: JSFunPtr (IO ()) -> IO ()          
-- loadJS f = do alert "hi"
--               dynLoad "../brunch/src/vendor/jquery-1.6.2.js"
--               alert "bye"
--               onDocumentReady f

main :: IO ()
main = do init <- ioWrap initialize
          onDocumentReady init
          
initialize :: IO () 
initialize = do -- Dynamiccaly include the necessary files
                -- Events loading
                hiAlert <- eventWrap (\x -> do alert "Hi!"
                                               return True)
                register_events [("#button", "click", hiAlert)]
                button <- jQuery "#button"
                click button hiAlert
                
                ajaxQ "/rules/stored" noop noop

-- addRules :: JSPtr [Rule] -> IO ()
-- addRules = undefined                


foreign import jscript "jQuery.noop()"
  noop :: JSFunPtr (JSPtr a -> IO ())
  
foreign import jscript "wrapper"
  eventWrap :: (JQuery -> IO Bool)-> IO (JSFunPtr (JQuery -> IO Bool))
  
foreign import jscript "wrapper"
  ioWrap :: IO () -> IO (JSFunPtr (IO ()))
  
dynLoad :: String -> IO ()  
dynLoad src = do doc <- HTML5.document
                 node     <- documentCreateElement "script"
                 elementSetAttribute node "src"   src
                 elementSetAttribute node "type"  "text/javascript"
                 -- Append the tag
                 headTags <- documentGetElementsByTagName doc "head"
                 headTag  <- nodeListItem headTags 0
                 elementAppendChild headTag node