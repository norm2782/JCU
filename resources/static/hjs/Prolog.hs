module Prolog where

import Data.List  
import Data.Tree (Tree(..))


import            Language.Prolog.NanoProlog.NanoProlog
import            Language.Prolog.NanoProlog.ParserUUTC

type Proof     =  Tree Term

data DropRes   =  DropRes Bool Proof
               deriving Show
  
dropUnify :: Proof -> [Int] -> Rule -> DropRes
dropUnify prf []          _                               = DropRes False prf
dropUnify prf tns@(_:ns)  (t :<-: ts)  |  isNothing tmnd  = DropRes False prf
                                       |  otherwise       = drprs
  where  tmnd   =  getNode prf ns
         drprs  =  let  (Just (Node tm _)) = tmnd
                        ntg         = intersperse '.' $ concatMap show tns
                        ncs         = map (flip Node []) (tag ntg ts)
                        mkPrf' env  = subst env (insertNode prf ns ncs)
                   in   case unify (tag ntg t, tm) emptyEnv of
                          Nothing   -> DropRes False  prf
                          Just env  -> DropRes True   (mkPrf' env)

getNode :: Proof -> [Int] -> Maybe Proof
getNode (Node _ [])  (_:_)   =  Nothing
getNode (Node _ ys)  (x:xs)  |  length ys >= x  = getNode (ys !! (x-1)) xs
                             |  otherwise       = Nothing
getNode node         []      =  Just node

insertNode :: Proof -> [Int] -> [Proof] -> Proof
insertNode (Node t ys)  (x:xs)  cs = Node t (take (x-1) ys ++ insertNode (ys !! (x-1)) xs cs : drop x ys)
insertNode (Node t _)   []      cs = Node t cs

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False