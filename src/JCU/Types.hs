{-# LANGUAGE OverloadedStrings #-}

module JCU.Types where

import            Data.ByteString (ByteString)
import            Data.List (intercalate)
import            Data.Tree (Tree(..))
import            Snap.Auth (AuthUser)

type Ident  =  String

data Term   =  Con Int
            |  Var Ident
            |  Fun Ident [Term]
            deriving Eq

data Rule   =  Term   :<-: [Term]
            deriving Eq

type Env    = [(Ident, Term)]

instance Show Term where
  show (Con  i)     = show i
  show (Var  i)     = i
  show (Fun  i [])  = i
  show (Fun  i ts)  = i ++ "(" ++ showCommas ts ++ ")"

instance Show Rule where
  show (t :<-: []) = show t ++ "."
  show (t :<-: ts) = show t ++ ":-" ++ showCommas ts ++ "."

showCommas :: Show a => [a] -> String
showCommas l = intercalate ", " (map show l)

class Taggable a where
  tag :: Int -> a -> a

instance Taggable Term where
  tag _ (Con  x)     = Con  x
  tag n (Var  x)     = Var  (x ++ show n)
  tag n (Fun  x xs)  = Fun  x (map (tag n) xs)

instance Taggable Rule where
  tag n (c :<-: cs) = tag n c :<-: map (tag n) cs

-- there's not a lot of data we want to store anyway.
data User = User  {  authUser     :: AuthUser
                  ,  storedRules  :: [ByteString]
                  ,  inuseRules   :: [ByteString] }
          deriving Show

type Proof   = Tree [Term]
type PCheck  = Tree Bool
