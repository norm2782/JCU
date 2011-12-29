module Models where

import Language.UHC.JScript.ECMA.String
import Language.UHC.JScript.Primitives
import Language.UHC.JScript.Types


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

instance FromJS JSRule Rule where
  fromJS = jsRule2Rule

jsRule2Rule :: JSRule -> Rule
jsRule2Rule ptr = Rule {
                    id   = getAttr "id"   ptr, 
                    ro   = getAttr "ro"   ptr,
                    rule = getAttr "rule" ptr
                  }

proofTreeNode = Node "" "" [] ""

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
