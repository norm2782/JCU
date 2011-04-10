{-# LANGUAGE OverloadedStrings  #-}

module JCU.Web.Types where

import            Data.ByteString (ByteString)
import            Snap.Auth (AuthUser)

-- TODO: Get prolog field out of user? Though it can't hurt too much;
-- there's not a lot of data we want to store anyway.
data User = User  {  authUser     :: AuthUser
                  ,  storedRules  :: ByteString }

