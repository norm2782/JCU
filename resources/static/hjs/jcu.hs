{-# LANGUAGE EmptyDataDecls #-}
module JCU where

import Control.Monad (liftM)

import Data.List



import Language.UHC.JScript.Types -- (JS, toJS, fromJS, FromJS)
import Language.UHC.JScript.Primitives
import Language.UHC.JScript.JQuery.JQuery
import Language.UHC.JScript.W3C.HTML5 as HTML5

import Language.UHC.JScript.ECMA.Bool
import Language.UHC.JScript.ECMA.String


import Language.UHC.JScript.Assorted (alert , _alert)

import Language.UHC.JScript.JQuery.Ajax as Ajax
import qualified Language.UHC.JScript.JQuery.AjaxQueue as AQ
import Language.UHC.JScript.JQuery.Draggable

----
--  App
----
-- import Language.UHC.JScript.ECMA.Array

import Array

import Templates
import Models


foreign import jscript "typeof(%1)"
  typeof :: a -> JSString


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
addRules obj str obj2 = do
  -- slet rules  = (Data.List.map fromJS . elems . jsArrayToArray) obj
  f <- mkEachIterator (\idx e -> do
    let rt = (rules_list_item .fromJS . rule . jsRule2Rule) e
    rules_list_div <- jQuery "#rules-list-div"
    rules_list_ul  <- jQuery "<ul id=\"rules-list-view\"/>"
    append rules_list_div rules_list_ul
    appendString rules_list_ul ("<li>" ++ rt ++ "</li>")    
    return ())
  each' obj f
  
  onStart <- mkJUIEventHandler (\x y -> do focus <- jQuery ":focus"
                                           doBlur focus
                                           return False)
  
  draggables <- jQuery ".draggable"
  draggable draggables $ Draggable (toJS True) (toJS "document") (toJS True) 100 50 onStart
  
  return ()

foreign import jscript "jQuery.noop()"
  noop :: IO (JSFunPtr (JSPtr a -> String -> JSPtr b -> IO()))
  
foreign import jscript "wrapper"
  eventWrap :: (JQuery -> IO Bool)-> IO (JSFunPtr (JQuery -> IO Bool))

foreign import jscript "wrapper"
  ioWrap :: IO () -> IO (JSFunPtr (IO ()))

alertType :: a -> IO ()
alertType = _alert . typeof

defaultDragOptions = Draggable 