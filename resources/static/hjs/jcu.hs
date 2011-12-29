{-# LANGUAGE EmptyDataDecls #-}
module JCU where

import Control.Monad (liftM)

import Data.List



import Language.UHC.JScript.Types (JS, toJS, fromJS)
import Language.UHC.JScript.Primitives
import Language.UHC.JScript.JQuery.JQuery
import Language.UHC.JScript.W3C.HTML5 as HTML5

import Language.UHC.JScript.ECMA.String

import Language.UHC.JScript.Assorted (alert , _alert)

import Language.UHC.JScript.JQuery.Ajax as Ajax
import qualified Language.UHC.JScript.JQuery.AjaxQueue as AQ
----
--  App
----
-- import Language.UHC.JScript.ECMA.Array

import Array

import Templates
import Models

ajaxQ :: JS r => String -> AjaxCallback r -> AjaxCallback r -> IO ()
ajaxQ url onSuccess onFail = do
  AQ.ajaxQ "jcu_app" 
           (AjaxOptions { ao_url         = url,
                          ao_requestType = Ajax.GET,
                          ao_contentType = "application/json",
                          ao_dataType    = "json"
                        })
           onSuccess
           onFail
    
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
initialize = do -- Rendering
                bd <- jQuery "#bd"
                setHTML bd Templates.home          
                wrapInner bd "<div id=\"home-view\"/>"
                -- Proof tree
                
                -- Rules list
                ajaxQ "/rules/stored" addRules noop
  where noop :: AjaxCallback (JSPtr a)
        noop = (\x y z -> return ())


addRules :: AjaxCallback (JSArray JSRule)
addRules obj str obj2 = do -- slet rules  = (Data.List.map fromJS . elems . jsArrayToArray) obj
                           f <- mkEachIterator (\ idx e -> do let ruleElem = jsRule2Rule e
                                                              _alert (rule ruleElem)
                                                              alert (jsStringToString $ rule ruleElem)

                                                              return ())

                           alert "rules!"
                           each' obj f
                           return ()                
  where ruleF :: Int -> JSRule -> IO ()
        ruleF idx e = do let ruleElem = fromJS e 
                         (alert . fromJS . rule) ruleElem

foreign import jscript "jQuery.noop()"
  noop :: IO (JSFunPtr (JSPtr a -> String -> JSPtr b -> IO()))
  
foreign import jscript "wrapper"
  eventWrap :: (JQuery -> IO Bool)-> IO (JSFunPtr (JQuery -> IO Bool))

foreign import jscript "wrapper"
  ioWrap :: IO () -> IO (JSFunPtr (IO ()))