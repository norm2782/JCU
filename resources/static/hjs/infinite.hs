module Infinite where

import Data.Tree  

import Language.UHC.JScript.Assorted (alert)
import            Language.Prolog.NanoProlog.NanoProlog
import            Language.Prolog.NanoProlog.ParserUUTC
  
import Prolog

main = do alert "start"
          let p =Node (Fun "ouder" [Var "X",Fun "alex" []]) [Node (Fun "ma" [Var "X",Fun "alex" []]) []]
          let rules = [Fun "append" [Fun "nil" [],Var "X",Var "Y"] :<-: [],Fun "append" [Fun "cons" [Var "A",Var "X"],Var "Y",Fun "cons" [Var "A",Var "Z"]] :<-: [Fun "append" [Var "X",Var "Y",Var "Z"]],Fun "elem" [Var "X",Fun "cons" [Var "X",Var "Y"]] :<-: [],Fun "elem" [Var "X",Fun "cons" [Var "Z",Var "Y"]] :<-: [Fun "elem" [Var "X",Var "Y"]],Fun "plus" [Fun "zero" [],Var "X",Var "X"] :<-: [],Fun "plus" [Fun "succ" [Var "X"],Var "Y",Fun "succ" [Var "Z"]] :<-: [Fun "plus" [Var "X",Var "Y",Var "Z"]],Fun "ouder" [Var "X",Var "Y"] :<-: [Fun "pa" [Var "X",Var "Y"]],Fun "ouder" [Var "X",Var "Y"] :<-: [Fun "ma" [Var "X",Var "Y"]],Fun "voor" [Var "X",Var "Y"] :<-: [Fun "ouder" [Var "X",Var "Y"]],Fun "voor" [Var "X",Var "Y"] :<-: [Fun "ouder" [Var "X",Var "Z"],Fun "voor" [Var "Z",Var "Y"]],Fun "oma" [Var "X",Var "Z"] :<-: [Fun "ma" [Var "X",Var "Y"],Fun "ouder" [Var "Y",Var "Z"]],Fun "man" [Var "X"] :<-: [Fun "elem" [Var "X",Fun "cons" [Fun "claus" [],Fun "cons" [Fun "alex" [],Fun "cons" [Fun "con" [],Fun "cons" [Fun "fri" [],Fun "empty" []]]]]]],Fun "ma" [Fun "mien" [],Fun "juul" []] :<-: [],Fun "ma" [Fun "juul" [],Fun "bea" []] :<-: [],Fun "ma" [Fun "bea" [],Fun "alex" []] :<-: [],Fun "ma" [Fun "bea" [],Fun "con" []] :<-: [],Fun "ma" [Fun "bea" [],Fun "fri" []] :<-: [],Fun "ma" [Fun "max" [],Fun "ale" []] :<-: [],Fun "ma" [Fun "max" [],Fun "ama" []] :<-: [],Fun "ma" [Fun "max" [],Fun "ari" []] :<-: [],Fun "pa" [Fun "alex" [],Fun "ale" []] :<-: [],Fun "pa" [Fun "alex" [],Fun "ama" []] :<-: [],Fun "pa" [Fun "alex" [],Fun "ari" []] :<-: [],Fun "pa" [Fun "alex" [],Fun "moe" []] :<-: []]
          --Node ouder(X, alex) [Node ma(X, alex) []]
          let x = checkProof rules p
          alert (show x)
          alert "stop"
          