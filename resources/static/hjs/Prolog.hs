-- | This code was scavenged from the JCU server-side Prolog code. 
--   So if that changes you should change it here too otherwise you won't see
--   the effects of your changes. :)
module Prolog where

import Data.List  
import Data.Tree (Tree(..))


import            Language.Prolog.NanoProlog.NanoProlog
import            Language.Prolog.NanoProlog.ParserUUTC

type Proof     =  Tree Term
type PCheck    =  Tree Status

data Status    =  Correct
               |  Incomplete
               |  Invalid
               deriving Show
               
dummyProof :: Proof -> PCheck
-- dummyProof = fmap (const Incomplete)
dummyProof (Node _ xs) = Node Incomplete (map dummyProof xs)

-- | Check if the proof provided by the client is correct, incomplete or
-- incorrect. It returns a @PCheck@: a @Tree Status@. Each node is assigned
-- an indiviual status. The status is determined by examining a node's child
-- nodes (containing terms) and see if they unify. If they do, that particular
-- node is Correct. If a node does not have any more children, but has not
-- reached a fact yet, it is Incomplete. If the child term is a non-unifyable
-- term, it is Incorrect.
checkProof :: [Rule] -> Proof -> PCheck
checkProof rls (Node tm cs)
  | rlsMatch   =  if hasVars tm
                    then  mkNode Incomplete
                    else  mkNode Correct
  | otherwise  =  if null cs
                    then  mkNode Incomplete
                    else  mkNode Invalid
  where  rlsMatch   = any (tryRule tm (map rootLabel cs)) rls
         mkNode st  = Node st (map (checkProof rls) cs)

hasVars :: Term -> Bool
hasVars (Var _)     = True
hasVars (Fun _ [])  = False
hasVars (Fun _ xs)  = any hasVars xs

tryRule ::  Term -> [Term] -> Rule -> Bool
tryRule tm cs (lhs :<-: rhs) =
  case matches (lhs, tm) emptyEnv of
    Nothing  ->  False
    env      ->  length cs == length rhs &&
                 isJust (foldr matches env (zip rhs cs))

instance Subst (Tree Term) where
  subst env (Node tm cs) = Node (subst env tm) (subst env cs)
  
  
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

isJust :: Maybe a -> Bool
isJust = not . isNothing