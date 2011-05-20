module JCU.Prolog.Types where

import            Data.List (intercalate)
import            Data.Tree (Tree(..))

type Env = [(UpperCase, Term)]
type UpperCase  = String
type LowerCase  = String
type Proof    = Tree Term
type PCheck   = Tree Status

data Term  =  Var UpperCase
           |  Fun LowerCase [Term]
           deriving (Eq, Ord)

data Rule  =  Term :<-: [Term]
           deriving Eq

data Status  =  Correct
             |  Incomplete
             |  Invalid
             deriving Show

instance Show Term where
  show (Var  i)      = i
  show (Fun  i [] )  = i
  show (Fun  i ts )  = i ++ "(" ++ showCommas ts ++ ")"

instance Show Rule where
  show (t :<-: [] ) = show t ++ "."
  show (t :<-: ts ) = show t ++ ":-" ++ showCommas ts ++ "."

showCommas :: Show a => [a] -> String
showCommas l = intercalate ", " (map show l)

class Taggable a where
  tag :: Int -> a -> a

instance Taggable Term where
  tag n  (Var  x)     = Var  (x ++ show n)
  tag n  (Fun  x xs)  = Fun  x (map (tag n) xs)

instance Taggable Rule where
  tag n (c :<-: cs) = tag n c :<-: map (tag n) cs
