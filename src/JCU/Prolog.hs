module JCU.Prolog where

import            Data.Tree (Tree(..))
import            Debug.Trace
import            JCU.Types
import            Language.Prolog.NanoProlog.Lib

checkProof :: [Rule] -> Proof -> PCheck
checkProof rls (Node tm cs)
  | rlsMatch   = Node Correct (map (checkProof rls) cs)
  | otherwise  = if null cs
                   then  Node Incomplete []
                   else  Node Invalid (map (checkProof rls) cs)
  where rlsMatch = any (tryRule tm [c | Node c _ <- cs]) rls

tryRule :: Term -> [Term] -> Rule -> Bool
tryRule tm cs (lhs :<-: rhs) =
  case unify (tm, lhs) emptyEnv of
    Nothing  ->  False
    Just s   ->  let  newrhs = map (subst s) rhs
                 in   locateAll newrhs cs

locateAll :: [Term] -> [Term] -> Bool
locateAll []      _   = True
locateAll (_:_)   []  = False
locateAll (x:xs)  cs  = or  [  locateAll (map (subst e) xs) css
                            |  (c,css) <- split cs
                            ,  Just e  <- [unify (x, c) emptyEnv] ]

split :: [a] -> [(a, [a])]
split xs = split' xs id
  where  split' (y:ys)  f  = (y, f ys) : split' ys (f.(y:))
         split' []      _  = []

getRhss :: Term -> Rule -> DropRes
getRhss tm rl =
  let (c :<-: cs) = tag 0 rl -- TODO: Actually tag the entire tree
  in  case unify (tm, c) emptyEnv of
        Nothing  -> DropRes False 0 [] []
        Just x   -> trace (show x) $ DropRes True (length cs) cs (subst x cs)


cnst :: LowerCase -> Term
cnst s = Fun s []

exampleData :: [Rule]
exampleData =
  -- Dutch Royal family
  [  Fun "ma"     [cnst "mien",  cnst "juul"]  :<-: []
  ,  Fun "ma"     [cnst "juul",  cnst "bea"]   :<-: []
  ,  Fun "ma"     [cnst "bea",   cnst "alex"]  :<-: []
  ,  Fun "ma"     [cnst "bea",   cnst "con"]   :<-: []
  ,  Fun "ma"     [cnst "bea",   cnst "fri"]   :<-: []
  ,  Fun "ma"     [cnst "max",   cnst "ale"]   :<-: []
  ,  Fun "ma"     [cnst "max",   cnst "ama"]   :<-: []
  ,  Fun "ma"     [cnst "max",   cnst "ari"]   :<-: []
  ,  Fun "pa"     [cnst "alex",  cnst "ale"]   :<-: []
  ,  Fun "pa"     [cnst "alex",  cnst "ama"]   :<-: []
  ,  Fun "pa"     [cnst "alex",  cnst "ari"]   :<-: []
  ,  Fun "ouder"  [Var "X",  Var "Y"] :<-:  [  Fun "pa"     [Var "X",  Var "Y"] ]
  ,  Fun "ouder"  [Var "X",  Var "Y"] :<-:  [  Fun "ma"     [Var "X",  Var "Y"] ]
  ,  Fun "voor"   [Var "X",  Var "Y"] :<-:  [  Fun "ouder"  [Var "X",  Var "Y"] ]
  ,  Fun "voor"   [Var "X",  Var "Y"] :<-:  [  Fun "ouder"  [Var "X",  Var "Z"]
                                            ,  Fun "voor"   [Var "Z",  Var "Y"] ]
  ,  Fun "oma"    [Var "X",  Var "Z"] :<-:  [  Fun "ma"     [Var "X",  Var "Y"]
                                            ,  Fun "ouder"  [Var "Y",  Var "Z"] ]
  -- List
  ,  Fun "append"  [  cnst "nil", Var "X", Var "Y"] :<-: []
  ,  Fun "append"  [  Fun "cons" [Var "A", Var "X"]
                   ,  Var "Y", Fun "cons" [Var "A", Var "Z"]]
                   :<-: [Fun "append" [Var "X", Var "Y", Var "Z"]]

  -- List lookup
  ,  Fun "elem"  [Var "X", Fun "cons" [Var "X", Var "Y"]] :<-: []
  ,  Fun "elem"  [Var "X", Fun "cons" [Var "Z", Var "Y"]]
                 :<-: [Fun "elem" [Var "X", Var "Y"]]

  -- Natural numbers
  ,  Fun "plus"  [cnst "zero", Var "X", Var "X"] :<-: []
  ,  Fun "plus"  [Fun "succ" [Var "X"], Var "Y", Fun "succ" [Var "Z"]]
                 :<-: [Fun "plus" [Var "X", Var "Y", Var "Z"]]
  ,  Fun "man"   [Var "X"] :<-: [Fun "elem"  [Var "X",       Fun "cons"
                                             [cnst "claus",  Fun "cons"
                                             [cnst "alex",   Fun "cons"
                                             [cnst "con",    Fun "cons"
                                             [cnst "fri",    Fun "empty" []]]]]]]
  ]

-- man(X) :- elem(X, cons(claus, cons(alex, cons(con, cons(fri, empty)))))
