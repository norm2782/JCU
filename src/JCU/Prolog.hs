module JCU.Prolog where

import            Data.Tree (Tree(..))
import            JCU.Types
import            Language.Prolog.NanoProlog

checkProof :: [Rule] -> Proof -> PCheck
checkProof rls (Node tm cs)
  | rlsMatch   = Node Correct (map (checkProof rls) cs)
  | otherwise  = if null cs
                   then  Node Incomplete []
                   else  Node Invalid (map (checkProof rls) cs)
  where rlsMatch = any (tryRule tm [c | Node c _ <- cs]) rls

tryRule :: Term -> [Term] -> Rule -> Bool
tryRule tm cs (lhs :<-: rhs) =
  case unify (tm, lhs) (Just []) of
    Nothing  ->  False
    Just s   ->  let  newrhs = map (subst s) rhs
                 in   locateAll newrhs cs

locateAll :: [Term] -> [Term] -> Bool
locateAll []      _   = True
locateAll (_:_)   []  = False
locateAll (x:xs)  cs  = or  [  locateAll (map (subst e) xs) css
                            |  (c,css) <- split cs
                            ,  Just e  <- [unify (x,c) (Just [])] ]

split :: [a] -> [(a, [a])]
split xs = split' xs id
  where  split' (y:ys)  f  = (y, f ys) : split' ys (f.(y:))
         split' []      _  = []

getRhss :: Term -> Rule -> DropRes
getRhss t (c :<-: cs) =
  case unify (t, c) (Just []) of
    Nothing  -> DropRes False 0 [] []
    Just x   -> DropRes True (length cs) cs (map (subst x) cs)
