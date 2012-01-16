{-# LANGUAGE EmptyDataDecls #-}
module JCU where

import Control.Monad (liftM, foldM)

import Data.List
import Data.Tree as T



import Language.UHC.JScript.Types -- (JS, toJS, fromJS, FromJS)
import Language.UHC.JScript.Primitives
import Language.UHC.JScript.JQuery.JQuery
import Language.UHC.JScript.W3C.HTML5 as HTML5

import Language.UHC.JScript.ECMA.Bool
import Language.UHC.JScript.ECMA.String as JSString


import Language.UHC.JScript.Assorted (alert , _alert)

import Language.UHC.JScript.JQuery.Ajax as Ajax
import qualified Language.UHC.JScript.JQuery.AjaxQueue as AQ
import Language.UHC.JScript.JQuery.Draggable
import Language.UHC.JScript.JQuery.Droppable

import Language.Prolog.NanoProlog.NanoProlog
import Language.Prolog.NanoProlog.ParserUUTC

----
--  App
----

import Prolog

-- import Language.UHC.JScript.ECMA.Array

import Array

import Templates
import Models

ajaxQ :: (JS r, JS v) => AjaxRequestType -> String -> v -> AjaxCallback r -> AjaxCallback r -> IO ()
ajaxQ rt url vals onSuccess onFail = do
  AQ.ajaxQ "jcu_app"
           (AjaxOptions { ao_url         = url,
                          ao_requestType = rt,
                          ao_contentType = "application/json",
                          ao_dataType    = "json"
                        })
           vals
           onSuccess
           onFail

registerEvents :: [(String, JEventType, EventHandler)] -> IO ()
registerEvents = mapM_ (\ (e, event, eh) -> do elem <- jQuery e
                                               bind elem
                                                    event 
                                                    eh)

main :: IO ()
main = do init <- ioWrap initialize
          onDocumentReady init
          
ruleTreeId = "ul#proof-tree-view.tree"

initialize :: IO ()
initialize = do -- Rendering
                bd <- jQuery "#bd"
                setHTML bd Templates.home
                wrapInner bd "<div id=\"home-view\"/>"
                -- Proof tree
                
                -- Rules list
                obj <- mkAnonObj
                ajaxQ GET "/rules/stored" obj addRules noop
                
                addRuleTree
                
                registerEvents $ [("#btnCheck"  , "click"   , noevent)
                                 ,("#btnAddRule", "click"   , addRuleEvent)
                                 ,("#btnReset"  , "click"   , noevent)
                                 ,("#txtAddRule", "keypress", noevent)
                                 ,("#txtAddRule", "blur"    , noevent)
                                 ,("#btnSubst"  , "click"   , noevent)
                                 ]
  where noop :: AjaxCallback (JSPtr a)
        noop = (\x y z -> return ())
        noevent :: EventHandler
        noevent x = return False

addRuleTree :: IO ()
addRuleTree = do
  ruleTreeDiv <- jQuery "#proof-tree-div"
  ruleTreeUL  <- buildRuleUl $ T.Node (Var "") []
  append ruleTreeDiv ruleTreeUL
  
buildRuleUl :: Proof -> IO JQuery
buildRuleUl node =
  do topUL <- jQuery "<ul id=\"proof-tree-view\" class=\"tree\"/>"
     restUL <- build' [0] node node False
     append topUL restUL
     inputField <- findSelector restUL "input"
     eh  <- mkJThisEventHandler fCheck
     eh' <- wrappedJQueryEvent eh
     _bind inputField (toJS "blur") eh'
     return topUL
  where
    f :: [Int] -> Proof -> (JQuery, Int) -> Proof -> IO (JQuery, Int)
    f lvl wp (jq, n) node = do li' <- build' (lvl ++ [n]) wp node True
                               append jq li'
                               return (jq, n + 1)
    dropje :: Proof -> [Int] -> Proof -> UIThisEventHandler
    dropje wp lvl node this _ ui = do
      elemVal <- findSelector this "input[type='text']:first" >>= valString
      
      jsRuleText <- (getAttr "draggable" ui >>= getAttr "context" >>= getAttr "innerText") :: IO JSString
      let ruleText = fromJS jsRuleText :: String
      if length elemVal == 0 then
          alert "There needs to be a term in the text field!" 
        else
          case tryParseRule ruleText of
            Nothing  -> alert "This should not happen. Dropping an invalid rule here."
            (Just t) -> case dropUnify wp lvl t of
                          (DropRes False _) -> alert "I could not unify this."
                          (DropRes True  p) -> replaceRuleTree p
      
      return True

    
    build' :: [Int] -> Proof -> Proof -> Bool -> IO JQuery
    build' lvl wp n@(T.Node term childTerms) disabled =
      do li <- jQuery "<li/>"
         appendString li  $ proof_tree_item (show term) (intercalate "." $ map show lvl) disabled

         dropzones <- findSelector li ".dropzone"
         
         drop'   <- mkJUIThisEventHandler (dropje wp lvl n) 
         drop''  <- wrappedJQueryUIEvent drop'
         droppable dropzones $ Droppable (toJS "dropHover") drop''
         startUl <- jQuery "<ul/>"
         (res,_) <- foldM (f lvl wp) (startUl, 1) childTerms
         append li res
         return li
         
    fCheck :: ThisEventHandler
    fCheck this _ = do  elemVal <- valString this
                        let term = fromJS elemVal :: String
                        case tryParseTerm term of
                          (Just t) -> replaceRuleTree $ T.Node t []
                          _        -> addClass this "blueField"
                        return False
         
replaceRuleTree :: Proof -> IO ()
replaceRuleTree p = do
  oldUL <- jQuery ruleTreeId
  newUL <- buildRuleUl p
  replaceWith oldUL newUL


addRules :: AjaxCallback (JSArray JSRule)
addRules obj str obj2 = do
  -- slet rules  = (Data.List.map fromJS . elems . jsArrayToArray) obj
  rules_list_div <- jQuery "#rules-list-div"
  rules_list_ul  <- jQuery "<ul id=\"rules-list-view\"/>"
  append rules_list_div rules_list_ul
  f <- mkEachIterator (\idx e -> do
    (Rule id _ rule') <- jsRule2Rule e
    listItem <- createRuleLi (fromJS rule') id
    append rules_list_ul listItem
    return ())
  each' obj f
  
  onStart <- mkJUIEventHandler (\x y -> do focus <- jQuery ":focus"
                                           doBlur focus
                                           return False)
  
  draggables <- jQuery ".draggable"
  draggable draggables $ Draggable (toJS True) (toJS "document") (toJS True) 100 50 onStart
  
  return ()

addRuleEvent :: EventHandler
addRuleEvent event = do
  rule  <- jQuery "#txtAddRule" >>= valString
  let str = JSString.concat (toJS "{\"rule\":\"") $ JSString.concat rule (toJS "\"}")
  ajaxQ POST "/rules/stored" str (onSuccess (fromJS rule)) onFail
  return True
  where onSuccess :: String -> AjaxCallback Int
        onSuccess r id _ _ = do ul   <- jQuery "ul#rules-list-view"
                                item <- createRuleLi r id 
                                append ul item
        onFail _ _ _ = alert "faal"
        
createRuleLi :: String -> Int -> IO JQuery
createRuleLi rule id = do item <- jQuery $ "<li>" ++ rules_list_item rule ++ "</li>"
                          delButton <- findSelector item "button.btnDeleteList"
                          click delButton (deleteRule item id)
                          return item
        
foreign import jscript "jQuery.noop()"
  noop :: IO (JSFunPtr (JSPtr a -> String -> JSPtr b -> IO()))
  
foreign import jscript "wrapper"
  eventWrap :: (JQuery -> IO Bool)-> IO (JSFunPtr (JQuery -> IO Bool))

foreign import jscript "wrapper"
  ioWrap :: IO () -> IO (JSFunPtr (IO ()))

deleteRule :: JQuery -> Int -> EventHandler
deleteRule jq i _ = do ajaxQ DELETE ("/rules/stored/"++show i) i removeLi noop
                       return False
  where removeLi :: AjaxCallback ()
        removeLi _ _ _ = remove jq
        noop :: AjaxCallback ()
        noop _ _ _ = return ()
