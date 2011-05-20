module JCU.Prolog.Prolog where

import            Data.Tree (Tree(..))
import            Data.Maybe
import            JCU.Prolog.Types

unify :: (Term, Term) -> Maybe Env -> Maybe Env
unify _       Nothing       = Nothing
unify (t, u)  env@(Just e)  = uni (subst e t) (subst e u)
  where  uni (Fun x xs) (Fun y ys)
           | x == y && length xs == length ys  = foldr unify env (zip xs ys)
           | otherwise                         = Nothing
         uni _ _              =  Nothing

solve :: [Rule] -> Maybe Env -> Int -> [Term] -> [Env]
solve _      Nothing  _  _       =  []
solve _      e        _  []      =  [fromJust e]
solve rules  e        n  (t:ts)  =
  [  sol | (c :<-: cs)  <- map (tag n) rules
  ,  sol                <- solve rules (unify (t, c) e) (n+1) (cs ++ ts) ]

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

subst :: Env -> Term -> Term
subst env (Var x)     = maybe (Var x) (subst env) (lookup x env)
subst env (Fun x cs)  = Fun x (map (subst env) cs)
