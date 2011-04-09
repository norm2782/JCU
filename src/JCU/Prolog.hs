module JCU.Prolog where

import Data.Char (isUpper)
import Data.List (intercalate)
import Debug.Trace (trace)
import JCU.Types

lookUp :: Term -> Env -> Term
lookUp  (Var x)  e   =  case lookup x e of
                          Nothing   -> Var x
                          Just res  -> lookUp res e
lookUp  t        _   =  t

unify :: (Term, Term) -> Maybe Env -> Maybe Env
unify _       Nothing       = Nothing
unify (t, u)  env@(Just e)  = trace  ("unifying: " ++ show t ++ " " ++ show u ++ "\n")
                                     (uni (lookUp t e) (lookUp u e))
  where  uni (Var x) y          =  Just ((x, y): e)
         uni x (Var y)          =  Just ((y, x): e)
         uni (Con x) (Con y)    =  if x == y then env else Nothing
         uni (Fun x xs) (Fun y ys)
           | x == y && length xs == length ys  = foldr unify env (zip xs ys)
           | otherwise                         = Nothing
         uni _ _                =  Nothing

solve :: [Rule] -> [Term] -> Env -> Int -> [EnvTrace]
solve rules []      e _  = [(e, [])]
solve rules (t:ts)  e n  =
    [  (sol, trc:trace)
    |  tm@(c :<-: cs)   <- map (tag n) rules
    ,  Just r           <- [unify (t, c) (Just e)]
    ,  let trc = Trace t tm r (cs ++ ts)
    ,  (sol, trace)     <- solve rules (cs ++ ts) r (n+1)
    ]

-- Solving individual rules:
-- We need the right-hand side of the rule, since the left-hand side would bring
-- us back to where we were.
-- In order to solve the right-hand side, we need the environment with variables
-- which map terms like X0 and Y0 to concrete values.
-- Actually, we probably don't even want to show X0 and Y0, but concrete values
-- instead?
-- We need to take the environments into account.

showBdg :: Env -> (Ident, Term) -> String
showBdg bs (x, t)  | isUpper (head x) && length x == 1  = x ++ " = " ++ showTerm t ++ "\n"
                   | otherwise                          = ""
  where showTerm :: Term -> String
        showTerm (Con n)     = show n
        showTerm t@(Var _)   = showTerm (lookUp t bs) 
        showTerm (Fun f [])  = f
        showTerm (Fun f ts)  = f ++ "(" ++ intercalate ", " (map showTerm ts) ++ ")"
