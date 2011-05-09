{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings  #-}

module JCU.Web.Types where

import            Control.Applicative
import            Control.Monad
import            Data.Aeson
import            Data.ByteString (ByteString)
import            Snap.Auth (AuthUser)
import            JCU.Prolog.Types (Rule(..), Term(..))
import            JCU.Prolog.Parser


-- TODO: Get prolog field out of user? Though it can't hurt too much;
-- there's not a lot of data we want to store anyway.
data User = User  {  authUser     :: AuthUser
                  ,  storedRules  :: [ByteString]
                  ,  inuseRules   :: [ByteString] }
          deriving Show

data Proof = Proof Term [Proof]
           deriving Show

instance ToJSON Rule where
  toJSON t = object [ "rule" .= show t ]

-- TODO: Do we still need this?
{- instance FromJSON Rule where-}
{-   parseJSON (Object o)  = mkRule <$> o .: "rule"-}
{-   parseJSON _           = mzero-}

instance FromJSON Proof where
  parseJSON (Object o) = mkProofTree <$> o .: "term" <*> o .: "childTerms"

instance ToJSON Proof where
  toJSON (Proof t ps) = object  [  "term"        .= show t
                                ,  "childTerms"  .= toJSON ps ]

mkProofTree :: String -> Value -> Proof
mkProofTree r rts = Proof (mkTerm r) mkProofTrees
  where mkProofTrees = case fromJSON rts :: Result [Proof] of
                         (Success a)  -> a
                         _            -> error "failed!"

-- TODO: Something with errors
mkTerm :: String -> Term
mkTerm r = a
  where (a, e) = startParse pTerm r
