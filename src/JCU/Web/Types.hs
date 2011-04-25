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

instance ToJSON Rule where
  toJSON t = let  txt = show t
             in   object  [  "rule"  .= txt
                          ,  "id"    .= txt]

instance FromJSON Rule where
  parseJSON (Object o)  = mkRule <$> (o .: "id" <|> pure "") -- id is optional
                                 <*> o .: "rule"
  parseJSON _           = mzero

-- TODO: Something with errors
mkRule :: String -> String -> Rule
mkRule _ r = a
  where (a, e) = startParse pRule r
