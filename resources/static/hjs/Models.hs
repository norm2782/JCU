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