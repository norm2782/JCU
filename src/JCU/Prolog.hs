{-# LANGUAGE TypeSynonymInstances #-}
module JCU.Prolog where

import            Data.Tree (Tree(..))
import            Data.Maybe (isJust, listToMaybe, mapMaybe)
import            Data.Map (empty)
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
  | rlsMatch   =  Node Correct (map (checkProof rls) cs)
  | otherwise  =  if null cs
                    then  Node Incomplete []
                    else  Node Invalid (map (checkProof rls) cs)
  where rlsMatch = any (isJust . tryRule empty tm (map rootLabel cs)) rls

tryRule :: Env -> Term -> [Term] -> Rule -> Maybe Env
tryRule env tm cs (lhs :<-: rhs) =
  case unify (tm, lhs) (Just env) of
    Nothing  ->  Nothing
    Just s   ->  locateAll empty (subst s rhs) cs

locateAll :: Env -> [Term] -> [Term] -> Maybe Env
locateAll env  []      _   =  Just env
locateAll _    (_:_)   []  =  Nothing
locateAll _    (t:ts)  cs  =  listToMaybe located
  where located =  [ env'  |  (c, css)   <- split cs
                           ,  Just env   <- [unify (t, c) emptyEnv]
                           ,  Just env'  <- [locateAll env (subst env ts) css]
                   ]

split :: [a] -> [(a, [a])]
split xs = split' xs id
  where  split' (y:ys)  f  = (y, f ys) : split' ys (f.(y:))
         split' []      _  = []

instance Subst Proof where
  subst env (Node tm cs) = Node (subst env tm) (map (subst env) cs)

getRhss :: Proof -> Term -> Rule -> DropRes
getRhss prf tm (c :<-: cs) =
  case unify (tm, c) emptyEnv of
    Nothing   -> DropRes False 0 tm [] prf
    Just env  -> DropRes True  (length cs) (subst env tm) (map (subst env) cs)
                               (subst env prf)



-- Unify as before. Then subst the new env over the entire proof tree. Send
-- completely new tree back. On client side, persist new terms over the entire
-- tree, but without creating new objects. Just replace the existing values.









-- TODO:
-- This is wrong. It should return an entirely new tree. Other variables in the
-- tree may be affected by the resulting env.
-- Also, on the client side we don't have to add fake child nodes. It will all
-- be unified anyway.
-- So, try to unify with env obtained from checking tree, then apply a subst
-- with the env resulting from _that_ over the entire tree again.
{- getRhss :: Proof -> [Rule] -> Term -> Rule -> DropRes-}
{- getRhss prf rls tm (c :<-: cs) =-}
{-   let  prfRes    = checkProof' empty rls prf-}
{-        falseRes  = DropRes False 0 []-}
{-   in   case prfRes of-}
{-          Nothing  ->  falseRes-}
{-          justenv  ->  case unify (tm, c) justenv of-}
{-                          Nothing    -> falseRes-}
{-                          Just env'  -> DropRes True (length cs) (subst env' cs)-}

{- checkProof' :: Env -> [Rule] -> Proof -> Maybe Env-}
{- checkProof' env rls (Node tm cs) =-}
{-   let  envs = mapMaybe (tryRule env tm (map rootLabel cs)) rls-}
{-   in   case envs of-}
{-          []        -> Nothing-}
{-          (env':_)  -> listToMaybe (mapMaybe (checkProof' env' rls) cs)-}


{-
Approach for drag and drop unification:

Client-side:
- Clone tree using _.clone
- If non-fact, split rule up and add rhs as childnode(s), sans dot
- Send tree, together with rule and term server-side

Server-side:
- Execute checkUnify on cloned tree
- Checkunify does the same as checkProof, but substs the new term with the
  env first. It then proceeds with checkProof functionality. An invalid tree
  means unification failed. Anything else means sucess.

-}

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
