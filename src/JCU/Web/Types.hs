{-# LANGUAGE OverloadedStrings  #-}

module JCU.Web.Types where

import            Snap.Auth (AuthUser)
import            Data.ByteString (ByteString)

-- TODO: Get prolog field out of user? Though it can't hurt too much;
-- there's not a lot of data we want to store anyway.
data User = User  {  authUser     :: AuthUser
                  ,  storedRules  :: ByteString }

