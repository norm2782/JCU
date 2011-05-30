{-# LANGUAGE TypeSynonymInstances #-}
module JCU.Prolog where

import            Data.List (nub)
import            Data.Maybe (isJust, listToMaybe, mapMaybe)
import qualified  Data.Map as M (empty, unions, null, insert)
import            Data.Set (Set)
import qualified  Data.Set as S (unions, singleton, toList)
import            Data.Tree (Tree(..))
import            JCU.Types
import            Language.Prolog.NanoProlog.NanoProlog
import Debug.Trace (trace)
-- | Check if the proof provided by the client is correct, incomplete or
-- incorrect. It returns a @PCheck@: a @Tree Status@. Each node is assigned
-- an indiviual status. The status is determined by examining a node's child
-- nodes (containing terms) and see if they unify. If they do, that particular
-- node is Correct. If a node does not have any more children, but has not
-- reached a fact yet, it is Incomplete. If the child term is a non-unifyable
-- term, it is Incorrect.
checkProof :: [Rule] -> Proof -> PCheck
checkProof  rls (Node tm cs)
  | rlsMatch   =  if hasVars tm
                    then  Node Incomplete cs'
                    else  Node Correct cs'
  | otherwise  =  if null cs
                    then  Node Incomplete []
                    else  Node Invalid cs'
  where  rlsMatch            = any (tryRule  tm (map rootLabel cs)) rls
         cs'                 = map (checkProof rls) cs

hasVars (Var _)     = True
hasVars (Fun _ [])  = False
hasVars (Fun _ xs)  = any hasVars xs

tryRule ::  Term -> [Term] -> Rule ->Bool
tryRule tm cs (lhs :<-: rhs) =
  case matches (lhs, tm) emptyEnv of
    Nothing  ->  False
    Just e   ->   length cs == length rhs && isJust (foldr matches (Just e) (zip rhs cs))


instance Subst Proof where
  subst env (Node tm cs) = Node (subst env tm) (map (subst env) cs)

instance Taggable Proof where
  tag n (Node tm cs) = Node (tag n tm) (map (tag (n ++ "1")) cs)

dropUnify :: Int -> Proof -> Term -> Rule -> DropRes
dropUnify n prf tm rl =
  let  (c :<-: cs) = tag (show n) rl
  in   case unify (tm, c) emptyEnv of
          Nothing   ->  DropRes False 0 [] prf
          Just env  ->  let  subcs  = subst env cs
                             newcs  = zipWith (\n c -> tag ("." ++ show n) c) [1..] subcs
                             vs xs  = S.toList . S.unions $ map vars xs
                             zippd  = foldr (\(x, y) e -> M.insert x (Var y) e) env (zip (vs subcs) (vs newcs))
                        in   DropRes True (length cs) (subst zippd newcs) (subst zippd prf)

vars :: Term -> Set String
vars (Fun _ ts) = S.unions $ map vars ts
vars (Var v)    = S.singleton v

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
