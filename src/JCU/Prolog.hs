{-# LANGUAGE TypeSynonymInstances #-}
module JCU.Prolog where

import            Data.List (intersperse)
import            Data.Maybe (isJust, isNothing)
import            Data.Set (Set)
import qualified  Data.Set as S (unions, singleton, null)
import            Data.Tree (Tree(..))
import            JCU.Types
import            Language.Prolog.NanoProlog.NanoProlog

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
hasVars (Var v)     = True
hasVars (Fun _ [])  = False
hasVars (Fun _ xs)  = any hasVars xs

tryRule ::  Term -> [Term] -> Rule -> Bool
tryRule tm cs (lhs :<-: rhs) =
  case matches (lhs, tm) emptyEnv of
    Nothing  -> False
    env      -> length cs == length rhs && isJust (foldr matches env (zip rhs cs))

instance Subst Proof where
  subst env (Node tm cs) = Node (subst env tm) (subst env cs)


dropUnify :: Proof -> [Int] -> Rule -> DropRes
dropUnify prf []          _                               = DropRes False prf
dropUnify prf tns@(_:ns)  (t :<-: ts)  |  isNothing tmnd  = DropRes False prf
                                       |  otherwise       = drprs
  where  tmnd   =  getNode prf ns
         drprs  =  let  (Just (Node tm _)) = tmnd
                        ntg         = intersperse '.' $ concatMap show tns
                        ncs         = map (flip Node []) (tag ntg ts)
                        mkPrf' env  = subst env (insertNode prf ns ncs)
                   in   case unify (tm, tag ntg t) emptyEnv of
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

cnst :: LowerCase -> Term
cnst s = Fun s []

fact :: LowerCase -> [Term] -> Rule
fact fn ts = Fun fn ts :<-: []

exampleData :: [Rule]
exampleData =
  [
  -- List
     fact "append"  [  cnst "nil", Var "X", Var "Y"]
  ,  Fun "append"   [  Fun "cons" [Var "A", Var "X"]
                    ,  Var "Y", Fun "cons" [Var "A", Var "Z"]]
                    :<-: [Fun "append" [Var "X", Var "Y", Var "Z"]]

  -- List lookup
  ,  fact "elem"  [Var "X", Fun "cons" [Var "X", Var "Y"]]
  ,  Fun "elem"   [Var "X", Fun "cons" [Var "Z", Var "Y"]]
                  :<-: [Fun "elem" [Var "X", Var "Y"]]

  -- Natural numbers
  ,  fact "plus"  [cnst "zero", Var "X", Var "X"]
  ,  Fun "plus"   [Fun "succ" [Var "X"], Var "Y", Fun "succ" [Var "Z"]]
                  :<-: [Fun "plus" [Var "X", Var "Y", Var "Z"]]

  -- Dutch Royal family
  ,  Fun "ouder"  [Var "X",  Var "Y"] :<-:  [  Fun "pa"     [Var "X",  Var "Y"] ]
  ,  Fun "ouder"  [Var "X",  Var "Y"] :<-:  [  Fun "ma"     [Var "X",  Var "Y"] ]
  ,  Fun "voor"   [Var "X",  Var "Y"] :<-:  [  Fun "ouder"  [Var "X",  Var "Y"] ]
  ,  Fun "voor"   [Var "X",  Var "Y"] :<-:  [  Fun "ouder"  [Var "X",  Var "Z"]
                                            ,  Fun "voor"   [Var "Z",  Var "Y"] ]
  ,  Fun "oma"    [Var "X",  Var "Z"] :<-:  [  Fun "ma"     [Var "X",  Var "Y"]
                                            ,  Fun "ouder"  [Var "Y",  Var "Z"] ]
  ,  Fun "man"    [Var "X"] :<-: [Fun "elem"  [Var "X",       Fun "cons"
                                              [cnst "claus",  Fun "cons"
                                              [cnst "alex",   Fun "cons"
                                              [cnst "con",    Fun "cons"
                                              [cnst "fri",    cnst "empty" ]]]]]]
  ,  fact "ma"    [cnst "mien",  cnst "juul"]
  ,  fact "ma"    [cnst "juul",  cnst "bea"]
  ,  fact "ma"    [cnst "bea",   cnst "alex"]
  ,  fact "ma"    [cnst "bea",   cnst "con"]
  ,  fact "ma"    [cnst "bea",   cnst "fri"]
  ,  fact "ma"    [cnst "max",   cnst "ale"]
  ,  fact "ma"    [cnst "max",   cnst "ama"]
  ,  fact "ma"    [cnst "max",   cnst "ari"]
  ,  fact "pa"    [cnst "alex",  cnst "ale"]
  ,  fact "pa"    [cnst "alex",  cnst "ama"]
  ,  fact "pa"    [cnst "alex",  cnst "ari"]

  ]
