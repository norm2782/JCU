{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JCU.Types where

import            Control.Applicative
import            Data.Aeson as AE
import            Data.ByteString (ByteString)
import            Data.Tree (Tree(..))
import            Language.Prolog.NanoProlog.Lib
import            Snap.Auth (AuthUser)

data User     = User  {  authUser     :: AuthUser
                      ,  storedRules  :: [ByteString] }
              deriving Show

data DropReq  = DropReq Term Rule
              deriving Show

data DropRes  = DropRes Bool Int [Term] [Term]
              deriving Show

data Status   = Correct
              | Incomplete
              | Invalid
              deriving Show

type Proof    = Tree Term
type PCheck   = Tree Status
type Cid      = String

instance FromJSON DropReq where
  parseJSON (Object o) = mkJSONDropReq <$> o .: "term" <*> o .: "rule"
  parseJSON _          = fail "No parseJSON for DropReq"

mkJSONDropReq :: String -> String -> DropReq
mkJSONDropReq t r = DropReq (mkJSONTerm t) (mkJSONRule r)

instance ToJSON DropRes where
  toJSON (DropRes b i ts uts) = object  [  "unified"   .= b
                                        ,  "children"  .= i
                                        ,  "rhss"      .= map show ts
                                        ,  "urhss"     .= map show uts ]

instance ToJSON PCheck where
  toJSON (Node st cs) = object  [  "proofCheckResult"    .= show st
                                ,  "proofCheckChildren"  .= toJSON cs ]

instance ToJSON Rule where
  toJSON t = object [ "rule" .= show t ]

instance FromJSON Rule where
  parseJSON (Object o) = mkJSONRule <$> o .: "rule"
  parseJSON _          = fail "No parseJSON for Rule"

-- TODO: Errors
mkJSONRule :: String -> Rule
mkJSONRule = fst . startParse pRule

instance FromJSON Proof where
  parseJSON (Object o) = mkJSONProofTree <$> o .: "term" <*> o .: "childTerms"
  parseJSON _          = fail "No parseJSON for Proof"

instance ToJSON Proof where
  toJSON (Node t ps) = object  [  "term"        .= show t
                               ,  "childTerms"  .= toJSON ps ]

mkJSONProofTree :: String -> Value -> Proof
mkJSONProofTree r rts = Node (mkJSONTerm r) mkProofTrees
  where mkProofTrees = case fromJSON rts :: AE.Result [Proof] of
                         (Success a)  -> a
                         _            -> error "failed!"

-- TODO: Something with errors
mkJSONTerm :: String -> Term
mkJSONTerm = fst . startParse pTerm
