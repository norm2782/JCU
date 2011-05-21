module JCU.Testing where

import            Data.Tree
import            JCU.Prolog
import            JCU.Types
import            Language.Prolog.NanoProlog

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

cnst :: LowerCase -> Term
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

testGetRhsss :: DropRes
testGetRhsss = getRhss t r
  where t = Fun "voor" [cnst "bea", cnst "ama"]
        r = Fun "voor" [Var "X",    Var "Y"] :<-: [ Fun "ouder" [Var "X", Var "Z"]
                                                  , Fun "voor"  [Var "Z", Var "Y"] ]

testGetRhsss' :: DropRes
testGetRhsss' = getRhss t r
  where t = Fun "voor" [cnst "bea", cnst "ama"]
        r = Fun "pa" [cnst "alex", cnst "ama"] :<-: []

