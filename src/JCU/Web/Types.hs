{-# LANGUAGE OverloadedStrings  #-}

module JCU.Web.Types where

import            Data.Aeson
import            Data.ByteString (ByteString)
import            Snap.Auth (AuthUser)
import            JCU.Prolog.Types (Rule(..))

-- TODO: Get prolog field out of user? Though it can't hurt too much;
-- there's not a lot of data we want to store anyway.
data User = User  {  authUser     :: AuthUser
                  ,  storedRules  :: [ByteString]
                  ,  inuseRules   :: [ByteString] }

instance ToJSON Rule where
  toJSON t = let  txt = show t
             in   object  [  "rule"  .= txt
                          ,  "id"    .= txt]
