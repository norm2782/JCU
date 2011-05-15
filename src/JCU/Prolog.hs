module JCU.Prolog where

import            Data.List (find, permutations)
import            Data.Maybe (isJust, isNothing)
import            Data.Tree (Tree(..))
import            Debug.Trace
import            JCU.Types

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

solve :: [Rule] -> Env -> Int -> [Term] -> [Env]
solve _     e _  []      = [e]
solve rules e n  (t:ts)  =
  [  sol
  |  (c :<-: cs)  <- map (tag n) rules
  ,  Just r       <- [unify (t, c) (Just e)]
  ,  sol          <- solve rules r (n+1) (cs ++ ts)
  ]


check :: [Rule] -> Env -> Int -> Proof -> Bool
check _      e _  (Node _      [])    = True
check rules  e n  (Node terms  subs)  = bla && all (check rules e (n+1)) subs
  where  rhsss :: [[([Term], Env)]]
         rhsss = map (rhss rules e) terms -- | Gather all right-hand sides of all terms in the current node
  -- Nu moet er dus een [Term] in rhsss zijn die hetzelfde is als subs (misschien wel verschillende volgorde).
         iser asol = isNothing $ find (\(ts, env) -> tseq ts subs env) asol
         bla = or [iser a | a <- rhsss ]

tseq :: [Term] -> [Tree [Term]] -> Env -> Bool
tseq terms subs env = any (matches . rootLabel) subs
  where  matches :: [Term] -> Bool
         matches sts    = or [termperm `match` sts | termperm <- permutations terms]
         match :: [Term] -> [Term] -> Bool
         match ts1 ts2  = and [isJust $ unify tp (Just env) | tp <- zip ts1 ts2]

   -- any (subsEqs subs . snd) sols
{-  where  -- | Are there as many solutions as terms?
         sols     :: [[Term]]
          -- Do all terms lead to a solution? This eliminates terms for which there is no solution.
         sols     = concatMap (rhss rules e) terms
         bla :: [Tree [Term]]
         bla = subs-}

-- ergens in de subs moet een Node zitten waarvan de terms gelijk zijn aan een
-- van de unify'de resultaten uit rhss

-- subs moet unifyen met 1 [Term] uit de resultaten van rhss!
--
-- If the t and c unify, we know that there is a term in the rules with which
-- the current term under consideration unifies. As a result, we return the
-- right-hand side of the rule(s) with which the provided term unifies.
--
-- Each [Term] represents a branch in the proof tree. E.g., either ma or ouder.
rhss :: [Rule] -> Env -> Term -> [([Term], Env)]
rhss rules e t = [(cs, r)  |  (c :<-: cs)  <- rules
                           ,  Just r       <- [unify (t, c) (Just e)]]

{-
 pa(alex,ama).
----------------
ouder(alex,ama).
-}

{-
                        pa(alex,ama). (5)
                        -------------
ma(bea,alex). (3)    ouder(alex,ama). (4)
-------------        ----------------
ouder(bea,alex), (2a) voor(alex,ama). (2b)
-------------------------------------
         voor(bea,ama). (1)



If (1) does not unify with something in the environment, return a False tree.
If (1) unifies with something in the environment, then grab the corresponding
rule with which it unified and grab the right-hand side list of terms of this
rule. With this list, see if the list of terms at (2) matches. That is, all
left-hand side terms must be exactly present at (2). No more no less. In
addition, they all need to unify with the left-hand side of (1). This now
repeats for every branch of the provided proof-tree. In general, if the
left-hand side list is empty, it means that we have arrived at a fact and we're
done. If we do not end up at a fact, this should be communicated in some way as
well.

Alternatively, just solve for every level in the tree. Not fast, but might just
work and it is definitely simple.
-}

-- FIXME:
-- Approach: First check/solve for the current node. This will result in an
-- environment. In `solve` the recursive case is passed (cs ++ ts), which are
-- the next things to be considered. These are the exact same things the user
-- passed in the childnodes. (or should have at least). We now just have to
-- check whether these resulting terms are specified in the node's children.
-- This needs to be an exact match. We then repeat this for each of the
-- children. This means that all non-root and non-leaf nodes are checked twice,
-- in essence.
-- We might not even need the Env here...
checkProof :: [Rule] -> Env -> Int -> Proof -> PCheck
checkProof rules e n nd@(Node _ sub)
  | check rules e n nd  = Node True $ map (checkProof rules e (n+1)) sub
  | otherwise           = fmap (\_ -> False) nd

testSimpleRight :: [Env]
testSimpleRight = solve testStoredRules [] 0 [Fun "ma" [cnst "mien", cnst "juul"]]
testSimpleWrong :: [Env]
testSimpleWrong = solve testStoredRules [] 0 [Fun "ma" [cnst "miesn", cnst "juul"]]


testRight ::  PCheck
testRight = checkProof testStoredRules [] 0 voorBeaAmaProof
testWrong :: PCheck
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

voorBeaAmaProof :: Proof
voorBeaAmaProof = Node [Fun "voor" [cnst "bea",  cnst "ama"]]
                    [ Node [ Fun "ouder" [cnst "bea",  cnst "alex"]
                           , Fun "voor"  [cnst "alex", cnst "ama"]
                           ]
                        [ Node [Fun "ma" [cnst "bea",  cnst "alex"]] []
                        , Node [Fun "ouder" [cnst "alex", cnst "ama"]]
                            [ Node [Fun "pa" [cnst "alex", cnst "ama"]] []
                            ]
                        ]
                    ]
voorBeaAmaWrong :: Proof
voorBeaAmaWrong = Node [Fun "voor" [cnst "bea",  cnst "ama"]]
                    [ Node [ Fun "ouder" [cnst "bea",  cnst "alex"]
                           , Fun "voor"  [cnst "alex", cnst "ama"]
                           ]
                        [ Node [Fun "ma" [cnst "bea",  cnst "alex"]] []
                        , Node [Fun "ouder" [cnst "max", cnst "ama"]]
                            [ Node [Fun "pa" [cnst "alex", cnst "ama"]] []
                            ]
                        ]
                    ]
