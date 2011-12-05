{-# LANGUAGE EmptyDataDecls #-}
module JCU where

import Language.UHC.JScript.Types (toJS)
import Language.UHC.JScript.Primitives
import Language.UHC.JScript.JQuery.JQuery


import Language.UHC.JScript.Assorted (alert)
----
--  App
----

import Templates
import Models


    
register_events :: [(String, JEventType, JEventHandler)] -> IO ()    
register_events = mapM_ (\ (e, event, eh) -> do elem <- jQuery e
                                                bind elem
                                                     event 
                                                     eh)
    
main :: IO ()
main = do alert "Hello!"
          init <- ioWrap initialize
          onDocumentReady init
          
          
initialize :: IO () 
initialize = do hiAlert <- eventWrap (\x -> do alert "Hi!"
                                               return True)
                register_events [("#button", "click", hiAlert)]
                button <- jQuery "#button"
                click button hiAlert

  
foreign import jscript "wrapper"
  eventWrap :: (JQuery -> IO Bool)-> IO (JSFunPtr (JQuery -> IO Bool))
  
foreign import jscript "wrapper"
  ioWrap :: IO () -> IO (JSFunPtr (IO ()))