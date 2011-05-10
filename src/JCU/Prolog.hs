module JCU.Prolog where

import            JCU.Types
import            Data.Tree (Tree(..))

lookUp :: Term -> Env -> Term
lookUp (Var x)  e   = case lookup x e of
                        Nothing   -> Var x
                        Just res  -> lookUp res e
lookUp t        _   = t

unify :: (Term, Term) -> Maybe Env -> Maybe Env
unify _       Nothing       = Nothing
unify (t, u)  env@(Just e)  = uni (lookUp t e) (lookUp u e)
  where  uni (Var x) y        = Just ((x, y): e)
         uni x (Var y)        = Just ((y, x): e)
         uni (Con x) (Con y)  = if x == y then env else Nothing
         uni (Fun x xs) (Fun y ys)
           | x == y && length xs == length ys  = foldr unify env (zip xs ys)
           | otherwise                         = Nothing
         uni _ _              =  Nothing

solve :: [Rule] -> [Term] -> Env -> Int -> [Env]
solve _     []      e _  = [e]
solve rules (t:ts)  e n  =
    [  sol
    |  (c :<-: cs)  <- map (tag n) rules
    ,  Just r       <- [unify (t, c) (Just e)]
    ,  sol          <- solve rules (cs ++ ts) r (n+1)
    ]

checkProof :: [Rule] -> Env -> Int -> Proof -> PCheck
checkProof rules e n (Node lbl sub) = Node unified children
  where unified   = (not . null) children
        children  = case solve rules [lbl] e n of
                      []       -> []
                      trEnv:_  -> map (checkProof rules trEnv (n+1)) sub


testRight = checkProof testStoredRules [] 0 voorBeaAmaProof
testWrong = checkProof testStoredRules [] 0 voorBeaAmaWrong

{-
Need to unify ouder(alex, ama). with ouder(X,Y) :- pa(X,Y). somehow.
How about:
- Zip concrete data with variable names
- Input those as variable/value pairs in the environment
- Continue unification

Does the existing unify already do this? It seems so! :D
So now the only trick is to build the tree and we're done.

When this is working, we can expand the PCheck tree to include whole
environments which can be passed to the client.
-}

cnst s = Fun s []
testStoredRules :: [Rule]
testStoredRules =  [ Fun "ma"    [cnst "mien", cnst "juul"] :<-: []
                   , Fun "ma"    [cnst "juul", cnst "bea"]  :<-: []
                   , Fun "ma"    [cnst "bea" , cnst "alex"] :<-: []
                   , Fun "ma"    [cnst "bea" , cnst "cons"] :<-: []
                   , Fun "ma"    [cnst "max" , cnst "ale"]  :<-: []
                   , Fun "ma"    [cnst "max" , cnst "ama"]  :<-: []
                   , Fun "ma"    [cnst "max" , cnst "ari"]  :<-: []
                   , Fun "oma"   [Var  "X"   ,  Var "Z"]    :<-: [ Fun "ma"    [Var "X", Var "Y"]
                                                               , Fun "ouder" [Var "Y", Var "Z"] ]
                   , Fun "pa"    [cnst "alex", cnst "ale"]  :<-: []
                   , Fun "pa"    [cnst "alex", cnst "ama"]  :<-: []
                   , Fun "pa"    [cnst "alex", cnst "ari"]  :<-: []
                   , Fun "ouder" [Var "X",    Var "Y"]    :<-: [ Fun "pa"    [Var "X", Var "Y"] ]
                   , Fun "ouder" [Var "X",    Var "Y"]    :<-: [ Fun "ma"    [Var "X", Var "Y"] ]
                   , Fun "voor"  [Var "X",    Var "Y"]    :<-: [ Fun "ouder" [Var "X", Var "Y"] ]
                   , Fun "voor"  [Var "X",    Var "Y"]    :<-: [ Fun "ouder" [Var "X", Var "Z"]
                                                               , Fun "voor"  [Var "Z", Var "Y"] ] ]

testInUseRules :: [Rule]
testInUseRules = [ Fun "voor"  [cnst "bea",    cnst "ama"] :<-: []
                 , Fun "" []   :<-: [ Fun "pa" [Var "X", Var "Y"] ]
                 , Fun "pa"    [Var "X"    , cnst "ama"] :<-: []
                 , Fun "pa"    [cnst "alex", cnst "ama"] :<-: []
                 ]

voorBeaAmaProof :: Proof
voorBeaAmaProof = Node (Fun "voor" [cnst "bea",  cnst "ama"])
                    [ Node (Fun "ouder" [cnst "bea",  cnst "alex"]) [
                        Node (Fun "ma" [cnst "bea",  cnst "alex"]) []
                      ]
                    , Node (Fun "voor"  [cnst "alex", cnst "ama"]) [
                        Node (Fun "ouder" [cnst "alex", cnst "ama"]) [
                          Node (Fun "pa" [cnst "alex", cnst "ama"]) []
                        ]
                      ]
                    ]

voorBeaAmaWrong :: Proof
voorBeaAmaWrong = Node (Fun "voor" [cnst "bea",  cnst "ama"])
                    [ Node (Fun "ouder" [cnst "bea",  cnst "alex"]) [
                        Node (Fun "pa" [cnst "bea",  cnst "alex"]) []
                      ]
                    , Node (Fun "voor"  [cnst "alex", cnst "ama"]) [
                        Node (Fun "ouder" [cnst "alex", cnst "ama"]) [
                          Node (Fun "pa" [cnst "alex", cnst "ama"]) []
                        ]
                      ]
                    ]
