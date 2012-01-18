module JCU where

import Control.Monad (liftM, foldM)

import Data.Array (elems)
import Data.List
import Data.Map   (fromList)
import Data.Maybe (fromJust)
import Data.Tree as T


import Language.UHC.JScript.Prelude
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

import Language.UHC.JScript.ECMA.Array as ECMAArray (JSArray, jsArrayToArray)

import Array

import Templates
import Models

showError = alert

ajaxQ :: (JS r, JS v) => AjaxRequestType -> String -> v -> AjaxCallback r -> AjaxCallback r -> IO ()
ajaxQ rt url =
  AQ.ajaxQ "jcu_app"
           AjaxOptions { ao_url         = url,
                         ao_requestType = rt,
                         ao_contentType = "application/json",
                         ao_dataType    = "json"
                       }

registerEvents :: [(String, JEventType, EventHandler)] -> IO ()
registerEvents = mapM_ (\ (e, event, eh) -> do elem <- jQuery e
                                               bind elem
                                                    event 
                                                    eh)

main :: IO ()
main = do init <- wrapIO initialize
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
                
                registerEvents [("#btnCheck"  , "click"   , toggleClue)
                               ,("#btnAddRule", "click"   , addRuleEvent)
                               ,("#btnReset"  , "click"   , resetTree)
                               ,("#txtAddRule", "keypress", noevent)
                               ,("#txtAddRule", "blur"    , checkTermSyntax)
                               ]
  where noop :: AjaxCallback (JSPtr a)
        noop _ _ _ = return ()
        noevent :: EventHandler
        noevent x = return False
        toggleClue :: EventHandler 
        toggleClue _ = do toggleClassString "#proof-tree-div" "noClue"
                          return True
        checkTermSyntax _ = do inp   <- jQuery "#txtAddRule"
                               input <- valString inp
                               case tryParseRule input of
                                 Nothing -> markInvalidTerm inp
                                 _       -> return ()
                               return True
        resetTree _ = do replaceRuleTree emptyProof
                         return True
                
                                               

emptyProof :: Proof
emptyProof = T.Node (Var "") []

addRuleTree :: IO ()
addRuleTree = do
  status      <- checkProof emptyProof
  ruleTreeDiv <- jQuery "#proof-tree-div"
  ruleTreeUL  <- buildRuleUl emptyProof status
  append ruleTreeDiv ruleTreeUL
  
buildRuleUl :: Proof -> PCheck -> IO JQuery
buildRuleUl node status =
  do topUL <- jQuery "<ul id=\"proof-tree-view\" class=\"tree\"/>"
     restUL <- build' [0] node (node, status) False
     append topUL restUL
     inputField <- findSelector restUL "input"
     eh  <- mkJThisEventHandler fCheck
     eh' <- wrappedJQueryEvent eh
     _bind inputField (toJS "blur") eh'
     return topUL
  where
    f :: [Int] -> Proof -> (JQuery, Int) -> (Proof, PCheck) -> IO (JQuery, Int)
    f lvl wp (jq, n) (node,status) = do li' <- build' (lvl ++ [n]) wp (node,status) True
                                        append jq li'
                                        return (jq, n + 1)
    dropje :: Proof -> [Int] -> Proof -> UIThisEventHandler
    dropje wp lvl node this _ ui = do
      elemVal <- findSelector this "input[type='text']:first" >>= valString
      
      jsRuleText <- (getAttr "draggable" ui >>= getAttr "context" >>= getAttr "innerText") :: IO JSString
      let ruleText = fromJS jsRuleText :: String
      if null elemVal then
          showError "There needs to be a term in the text field!" 
        else
          case tryParseRule ruleText of
            Nothing  -> showError "This should not happen. Dropping an invalid rule here."
            (Just t) -> case dropUnify wp lvl t of
                          (DropRes False _) -> showError "I could not unify this."
                          (DropRes True  p) -> replaceRuleTree p
      
      return True

    
    build' :: [Int] -> Proof -> (Proof, PCheck) -> Bool -> IO JQuery
    build' lvl wp (n@(T.Node term childTerms), T.Node status childStatus) disabled =
      do li <- jQuery "<li/>"
         appendString li  $ proof_tree_item (show term) (intercalate "." $ map show lvl) disabled status

         dropzones <- findSelector li ".dropzone"
         
         drop'   <- mkJUIThisEventHandler (dropje wp lvl n) 
         drop''  <- wrappedJQueryUIEvent drop'
         droppable dropzones $ Droppable (toJS "dropHover") drop''
         startUl <- jQuery "<ul/>"
         (res,_) <- foldM (f lvl wp) (startUl, 1) (zip childTerms childStatus)
         append li res
         return li
         
    fCheck :: ThisEventHandler
    fCheck this _ = do  term <- valString this
                        case tryParseTerm term of
                          (Just t) -> replaceRuleTree $ T.Node t []
                          _        -> markInvalidTerm this
                        return False
         
replaceRuleTree :: Proof -> IO ()
replaceRuleTree p = do
  status <- checkProof p
  oldUL <- jQuery ruleTreeId
  newUL <- buildRuleUl p status
  
  -- Store new proof in the subst funct
  registerEvents [("#btnSubst", "click", doSubst p)]
  -- Draw the new ruleTree
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
  rule  <- jQuery "#txtAddRule" >>= valJSString
  
  case tryParseRule (fromJS rule) of
    Nothing   -> showError "Invalid rule, not adding to rule list."
    (Just _)  -> do let str = JSString.concat (toJS "{\"rule\":\"") $ JSString.concat rule (toJS "\"}")
                    ajaxQ POST "/rules/stored" str (onSuccess (fromJS rule)) onFail
  return True
  where onSuccess :: String -> AjaxCallback Int
        onSuccess r id _ _ = do ul   <- jQuery "ul#rules-list-view" 
                                item <- createRuleLi r id 
                                append ul item
        onFail _ _ _ = showError "faal"
        
createRuleLi :: String -> Int -> IO JQuery
createRuleLi rule id = do item <- jQuery $ "<li>" ++ rules_list_item rule ++ "</li>"
                          delButton <- findSelector item "button.btnDeleteList"
                          click delButton (deleteRule item id)
                          return item
                          
checkProof :: Proof -> IO PCheck
checkProof p = do rules  <- jQuery ".rule-list-item" >>= jQueryToArray
                  rules' <- (mapM f . elems . jsArrayToArray) rules
                  return $ Prolog.checkProof rules' p
  where f x =    getAttr "innerText" x 
            >>=  return . fromJust . tryParseRule . (fromJS :: JSString -> String)
                    
doSubst :: Proof -> EventHandler
doSubst p _ = do sub <- jQuery "#txtSubstSub" >>= valString
                 for <- jQuery "#txtSubstFor" >>= valString
                 case tryParseTerm sub of
                   Nothing  -> return False
                   (Just t) -> do let newP = subst (Env $ fromList [(for, t)]) p
                                  replaceRuleTree newP
                                  return True
                                  
clearClasses :: JQuery -> IO ()
clearClasses = flip removeClass "blueField yellowField redField whiteField greenField"

markInvalidTerm :: JQuery -> IO ()
markInvalidTerm jq = do clearClasses jq
                        addClass jq "blueField"
        
deleteRule :: JQuery -> Int -> EventHandler
deleteRule jq i _ = do ajaxQ DELETE ("/rules/stored/"++show i) i removeLi noop
                       return False
  where removeLi :: AjaxCallback ()
        removeLi _ _ _ = remove jq
        noop :: AjaxCallback ()
        noop _ _ _ = return ()
