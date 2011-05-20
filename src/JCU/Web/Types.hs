{-# LANGUAGE OverloadedStrings #-}

module JCU.Web.Types where

import            Data.ByteString (ByteString)
import            JCU.Prolog.Types
import            Snap.Auth (AuthUser)

data User     =  User  {  authUser     :: AuthUser
                       ,  storedRules  :: [ByteString] }
              deriving Show

data DropReq  = DropReq Term Rule  deriving Show

type DropRes  = (Bool, Int)

