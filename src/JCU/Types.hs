module JCU.Types where

import            Data.ByteString (ByteString)
import            Data.Tree (Tree(..))
import            Language.Prolog.NanoProlog
import            Snap.Auth (AuthUser)

data User     = User  {  authUser     :: AuthUser
                      ,  storedRules  :: [ByteString] }
              deriving Show

data DropReq  = DropReq Term Rule
              deriving Show

type DropRes  = (Bool, Int)

type Proof    = Tree Term
type PCheck   = Tree Status

data Status   = Correct
              | Incomplete
              | Invalid
              deriving Show
