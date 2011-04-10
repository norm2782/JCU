{-# LANGUAGE OverloadedStrings #-}

module JCU.Prolog.Types where

import Data.Aeson
import Data.List (intercalate)

type Ident  =  String

data Term   =  Con Int
            |  Var Ident
            |  Fun Ident [Term]
            deriving Eq

data Rule   =  Term   :<-: [Term]
data Trace  =  Trace  { goal   :: Term
                      , unif   :: Rule
                      , env    :: Env
                      , terms  :: [Term] }

type Env       = [(Ident, Term)]
type EnvTrace  = (Env, [Trace])

instance Show Term where
  show (Con  i)     = show  i
  show (Var  i)     =       i
  show (Fun  i [])  =       i
  show (Fun  i ts)  = i ++ "(" ++ showCommas ts ++ ")"

instance Show Rule where
  show (t :<-: []) = show t ++ "."
  show (t :<-: ts) = show t ++ ":-" ++ showCommas ts ++ "."

instance Show Trace where
  show (Trace t r e ts) =  display "goal                  : " t   ++
                           display "unifies with head of  : " r   ++
                           display "new environment       : " e   ++
                           display "new goals             : " ts  ++ "\n"
                      where display str val = str ++ show val ++ "\n"

showCommas :: Show a => [a] -> String
showCommas l = intercalate ", " (map show l)

class Taggable a where
  tag :: Int -> a -> a

instance Taggable Term where
  tag n (Con  x)     = Con  x
  tag n (Var  x)     = Var  (x ++ show n)
  tag n (Fun  x xs)  = Fun  x (map (tag n) xs)

instance Taggable Rule where
  tag n (c :<-: cs) = tag n c :<-: map (tag n) cs

instance ToJSON Term where
  toJSON (Con number)       = object  [  "number"  .= number]
  toJSON (Var ident)        = object  [  "ident"   .= ident]
  toJSON (Fun ident terms)  = object  [  "ident"   .= ident
                                      ,  "terms"   .= map toJSON terms ]

instance ToJSON Rule where
  toJSON (term :<-: terms) = object  [  "term"   .= toJSON term
                                     ,  "terms"  .= map toJSON terms ]

instance ToJSON Trace where
  toJSON (Trace g u e ts) = object  [  "goal"   .= toJSON g
                                    ,  "unif"   .= toJSON u
                                    ,  "env"    .= toJSON e
                                    ,  "terms"  .= map toJSON ts ]
