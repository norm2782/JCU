{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module JCU.Parser where

import            Data.Aeson
import            Data.ListLike.Base (ListLike)
import            Data.Tree (Tree(..))
import            JCU.Types
import            Text.ParserCombinators.UU
import            Text.ParserCombinators.UU.BasicInstances
import            Text.ParserCombinators.UU.Utils

pRules :: Parser [Rule]
pRules = pList pRule

-- TODO: Add support for rules with more pFuns before the :-
pRule :: Parser Rule
pRule = (:<-:) <$> pFun <*> (pSymbol ":-" *> pTerms `opt` []) <* pDot

pTerm, pCon, pVar, pFun :: Parser Term
pTerm  =  pCon  <|>  pVar <|> pFun
pCon   =  Con   <$>  pNatural
pVar   =  Var   <$>  lexeme (pList1 pUpper)
pFun   =  Fun   <$>  pIdentifier <*> (pParens pTerms `opt` [])

pTerms :: Parser [Term]
pTerms = pListSep pComma pTerm

startParse :: (ListLike s b, Show b)  => P (Str b s LineColPos) a -> s
                                      -> (a, [Error LineColPos])
startParse p inp  =  parse ((,) <$> p <*> pEnd)
                  $  createStr (LineColPos 0 0 0) inp

pIdentifier :: Parser String
pIdentifier = (:) <$> pLower <*> lexeme (pList (pLower <|> pUpper <|> pDigit))

instance FromJSON DropReq where
  parseJSON (Object o) = mkDropReq <$> o .: "term" <*> o .: "rule"

mkDropReq :: String -> String -> DropReq
mkDropReq t r = DropReq (mkTerm t) (mkRule r)
  where mkRule = fst . startParse pRule

instance ToJSON DropRes where
  toJSON (b, i) = object  [  "unified"   .= b
                          ,  "children"  .= i]

instance ToJSON PCheck where
  toJSON (Node st cs) = object  [  "status"    .= show st
                                ,  "children"  .= toJSON cs ]

instance ToJSON Rule where
  toJSON t = object [ "rule" .= show t ]

instance FromJSON Proof where
  parseJSON (Object o) = mkProofTree <$> o .: "term" <*> o .: "childTerms"

instance ToJSON Proof where
  toJSON (Node t ps) = object  [  "term"        .= show t
                               ,  "childTerms"  .= toJSON ps ]

mkProofTree :: String -> Value -> Proof
mkProofTree r rts = Node (mkTerm r) mkProofTrees
  where mkProofTrees = case fromJSON rts :: Result [Proof] of
                         (Success a)  -> a
                         _            -> error "failed!"

-- TODO: Something with errors
mkTerm :: String -> Term
mkTerm = fst . startParse pTerm
