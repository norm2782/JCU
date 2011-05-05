{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings  #-}

module JCU.Web.Types where

import            Control.Applicative
import            Control.Monad
import            Data.Aeson
import            Data.ByteString (ByteString)
import            Snap.Auth (AuthUser)
import            JCU.Prolog.Types (Rule(..))
import            JCU.Prolog.Parser


-- TODO: Get prolog field out of user? Though it can't hurt too much;
-- there's not a lot of data we want to store anyway.
data User = User  {  authUser     :: AuthUser
                  ,  storedRules  :: [ByteString]
                  ,  inuseRules   :: [ByteString] }
          deriving Show

data RuleTree = RuleTree Rule [RuleTree]
              deriving Show

instance ToJSON Rule where
  toJSON t = object [ "rule" .= show t ]

-- TODO: Do we still need this?
instance FromJSON Rule where
  parseJSON (Object o)  = mkRule <$> o .: "rule"
  parseJSON _           = mzero

instance FromJSON RuleTree where
  parseJSON (Object o) = mkRuleTree <$> o .: "rule" <*> o .: "childRules"

mkRuleTree :: String -> Value -> RuleTree
mkRuleTree r rts = RuleTree (mkRule r) mkRuleTrees
  where mkRuleTrees = case fromJSON rts :: Result [RuleTree] of
                        (Success a) -> a
                        _           -> error "failed!"

-- TODO: Something with errors
mkRule :: String -> Rule
mkRule r = a
  where (a, e) = startParse pRule r
