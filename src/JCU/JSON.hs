{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module JCU.JSON where

import            Control.Applicative
import            Data.Aeson
import            Data.Tree (Tree(..))
import            JCU.Types
import            Language.Prolog.NanoProlog

instance FromJSON DropReq where
  parseJSON (Object o) = mkJSONDropReq <$> o .: "term" <*> o .: "rule"

mkJSONDropReq :: String -> String -> DropReq
mkJSONDropReq t r = DropReq (mkJSONTerm t) (mkJSONRule r)

instance ToJSON DropRes where
  toJSON (DropRes b i ts uts) = object  [  "unified"   .= b
                                        ,  "children"  .= i
                                        ,  "rhss"      .= map show ts
                                        ,  "urhss"     .= map show uts ]

instance ToJSON PCheck where
  toJSON (Node st cs) = object  [  "status"    .= show st
                                ,  "children"  .= toJSON cs ]

instance ToJSON Rule where
  toJSON t = object [ "rule" .= show t ]

instance FromJSON Rule where
  parseJSON (Object o) = mkJSONRule <$> o .: "rule"

-- TODO: Errors
mkJSONRule :: String -> Rule
mkJSONRule = fst . startParse pRule

instance FromJSON Proof where
  parseJSON (Object o) = mkJSONProofTree <$> o .: "term" <*> o .: "childTerms"

instance ToJSON Proof where
  toJSON (Node t ps) = object  [  "term"        .= show t
                               ,  "childTerms"  .= toJSON ps ]

mkJSONProofTree :: String -> Value -> Proof
mkJSONProofTree r rts = Node (mkJSONTerm r) mkProofTrees
  where mkProofTrees = case fromJSON rts :: Result [Proof] of
                         (Success a)  -> a
                         _            -> error "failed!"

-- TODO: Something with errors
mkJSONTerm :: String -> Term
mkJSONTerm = fst . startParse pTerm
