module JCU.Prolog where

import            Data.Tree (Tree(..))
import            JCU.Types

lookUp :: Term -> Env -> Term
lookUp (Var x)  e   = case lookup x e of
                        Nothing   -> Var x
                        Just res  -> lookUp res e
lookUp t        _   = t

subst :: Env -> Term  -> Term
subst env  (Var x)      = case lookup x env of
                            Nothing   -> Var x
                            Just res  -> subst env res
subst env  (Fun x cs)   = Fun x (map (subst env) cs)
subst _    con@(Con _)  = con


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

solve :: [Rule] -> Env -> Int -> [Term] -> [Env]
solve _     e _  []      = [e]
solve rules e n  (t:ts)  =
  [  sol
  |  (c :<-: cs)  <- tag n rules
  ,  Just r       <- [unify (t, c) (Just e)]
  ,  sol          <- solve rules r (n+1) (cs ++ ts)
  ]

{-
 pa(alex,ama).
----------------
ouder(alex,ama).
-}

{-
                         pa(alex,ama). (6)
                         -------------
ma(bea,alex). (4)     ouder(alex,ama). (5)
-------------         ----------------
ouder(bea,alex), (2)  voor(alex,ama). (3)
-------------------------------------
         voor(bea,ama). (1)
-}

-- TODO Client-side:
-- A rule from the list can be dragged onto a textfield which already has
-- content. Then application then checks whether the dragged rule and the
-- text in the textfield can be unified. If so, n child text fields appear,
-- where n is the number of terms in the right-hand side of the rule.
-- If a fact is unified this way, it will spawn one text field, containing
-- the fact.

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
                            ,  Just e  <- [unify (x,c) (Just [])]
                            ]

split :: [a] -> [(a, [a])]
split xs = split' xs id
    where  split' (y:ys)  f  = (y, f ys) : split' ys (f.(y:))
           split' []      _  = []

testSimpleRight :: PCheck
testSimpleRight  =  checkProof testStoredRules
                 $  Node (Fun "pa" [cnst "alex",  cnst "ama"]) []

testSimpleWrong :: PCheck
testSimpleWrong = checkProof testStoredRules $ Node (Fun "ma" [cnst "alex",  cnst "ama"]) []

testCorrect :: PCheck
testCorrect = checkProof testStoredRules voorBeaAmaProof
testInvalid :: PCheck
testInvalid = checkProof testStoredRules voorBeaAmaWrong
testIncomplete :: PCheck
testIncomplete = checkProof testStoredRules voorBeaAmaIncomplete

voorBeaAmaProof :: Proof
voorBeaAmaProof = Node (Fun "voor" [cnst "bea",  cnst "ama"])
                    [  Node (Fun "ouder" [cnst "bea",  cnst "alex"])
                         [ Node (Fun "ma" [cnst "bea",  cnst "alex"]) []]
                    ,  Node (Fun "voor"  [cnst "alex", cnst "ama"])
                         [ Node (Fun "ouder" [cnst "alex", cnst "ama"])
                             [ Node (Fun "pa" [cnst "alex", cnst "ama"]) []]] ]

voorBeaAmaIncomplete :: Proof
voorBeaAmaIncomplete = Node (Fun "voor" [cnst "bea",  cnst "ama"])
                         [  Node (Fun "ouder" [cnst "bea",  cnst "alex"]) []
                         ,  Node (Fun "voor"  [cnst "alex", cnst "ama"]) [] ]

voorBeaAmaWrong :: Proof
voorBeaAmaWrong = Node (Fun "voor" [cnst "bea",  cnst "ama"])
                    [  Node (Fun "ouder" [cnst "bea",  cnst "alex"])
                         [ Node (Fun "ma" [cnst "bea",  cnst "alex"]) []]
                    ,  Node (Fun "fout!"  [cnst "alex", cnst "ama"])
                         [ Node (Fun "ouder" [cnst "alex", cnst "ama"])
                             [ Node (Fun "pa" [cnst "alex", cnst "ama"]) []]] ]

cnst ::  Ident -> Term
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
