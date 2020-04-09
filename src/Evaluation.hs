module Evaluation where

import Expr
import qualified Assignment as A

eval :: A.Context -> Expr -> Maybe Expr
eval c (Variable n) = do (A.Variable _ e) <- A.get c n
                         eval c e
eval c (Function n e) = do x <- eval c e
                           (A.Function _ param fe) <- A.get c n
                           let tmp = A.update c (A.Variable param x)
                           eval tmp fe
eval c (Add e1 e2) = evalInfix c e1 e2 (+?)
eval c (Sub e1 e2) = evalInfix c e1 e2 (-?)
eval c (Mul e1 e2) = evalInfix c e1 e2 (*?)
eval c (Div e1 e2) = evalInfix c e1 e2 (/?)
eval c (Mod e1 e2) = evalInfix c e1 e2 (%?)
eval c (Exp e1 e2) = evalInfix c e1 e2 (^?)
eval c (Dot e1 e2) = evalInfix c e1 e2 (**?)
eval c x = Just x

evalInfix :: A.Context -> Expr -> Expr -> (Expr -> Expr -> Maybe Expr) -> Maybe Expr
evalInfix c e1 e2 f = do a <- eval c e1
                         b <- eval c e2
                         f a b
