module Models where

import Language.UHC.JScript.ECMA.String
import Language.UHC.JScript.Primitives
import Language.UHC.JScript.Types

import Data.List (find)

import ParseLib.Abstract
import Language.Prolog.NanoProlog.NanoProlog as NP
import Language.Prolog.NanoProlog.ParserUUTC


type ProofResult = String -- I want to make an enum of this

data ProofTreeNode = Node {
  term :: String,
  mcid :: String,
  childTerms   :: [ProofTreeNode],
  proofResult  :: ProofResult
}

data Rule = Rule {
  id   :: Int,
  ro   :: Int,
  rule :: JSString
}

data JSRulePtr
type JSRule = JSPtr JSRulePtr

instance FromJS JSRule (IO Rule) where
  fromJS = jsRule2Rule

jsRule2Rule :: JSRule -> IO Rule
jsRule2Rule ptr = do id   <- getAttr "id"   ptr 
                     ro   <- getAttr "ro"   ptr
                     rule <- getAttr "rule" ptr 
                     return $ Rule id ro rule

proofTreeNode = Node "" "" [] ""

foreign import jscript "%1.rule"
  getRule :: JSRule -> JSString
  
hasValidTermSyntax :: String -> Bool
hasValidTermSyntax term = 
  maybe False (const True) (tryParseTerm term)
  
  
tryParseTerm :: String -> Maybe Term
tryParseTerm = run pTerm


hasValidRuleSyntax :: String -> Bool
hasValidRuleSyntax rule = 
  maybe False (const True) (tryParseRule rule)

tryParseRule :: String -> Maybe NP.Rule
tryParseRule = run pRule
  
    
run :: Parser a b -> [a] -> Maybe b    
run p as = fmap fst . find (null . snd) $ startParse p as 

-- class exports.ProofTreeNode extends Backbone.Model
--   # Available attributes:
--   # term :: String
--   # mcid :: String
--   # childTerms :: BackBone.Collection
--   # proofResult :: String, can be either Correct, Incomplete or Invalid
-- 
--   initialize: =>
--     @set { childTerms: new Backbone.Collection()
--          , mcid: @cid }
-- 
--   term: =>
--     @get('term')
-- 
--   proofResult: =>
--     @get('proofResult')
-- 
--   isProved: =>
--     f = (acc, nd) -> nd.isProved() && acc
--     (@proofResult() == "Correct") && @childTerms().reduce f, true
-- 
--   setTerm: (tm) =>
--     @set({term: tm})
-- 
--   childTerms: =>
--     @get('childTerms')
-- 
--   setValidSyntax: (flag) =>
--     @set({validSyntax: flag})
-- 
--   hasValidSyntax: =>
--     @get('validSyntax')
-- 
--   isValid: =>
--     f = (acc, nd) -> nd.isValid() && acc
--     @hasValidSyntax() && @childTerms().reduce f, true
-- 
--   setProofResult: (data) =>
--     @set({proofResult: data.proofCheckResult})
--     @trigger('proof')
--     i = 0
--     f = (x) ->
--       x.setProofResult data.proofCheckChildren[i]
--       i++
--     @childTerms().each f
-- 
--   reset: =>
--      @childTerms().reset new Array()
-- 
--   setUnified: (tr) =>
--     @setTerm(tr.term)
--     childNo = tr.childTerms.length
--     newChildren = new Array()
--     if childNo > 0
--       for i in [1..childNo]
--         trLvl = @get('treeLvl').slice(0)
--         trLvl.push(i)
-- 
--         nd = new ProofTreeNode({ term: tr.childTerms[i-1].term
--                                , treeLvl: trLvl
--                                , treeLbl: @get('treeLbl') + "." + i
--                                , validSyntax: true
--                                , disabled: true })
-- 
--         nd.setUnified tr.childTerms[i-1]
--         newChildren.push(nd)
--     @childTerms().reset(newChildren)
