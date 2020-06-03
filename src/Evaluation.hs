module Evaluation where

import           Data.Map as M

import           Expr     as E


type LabelMap a = Map String a
data Context = Context { variables :: LabelMap Expr
                       , functions :: LabelMap (String, Expr)
                       }

eval :: Context -> Expr -> Maybe Expr

eval c (Add e1 e2) = evalInfix c e1 e2 add
eval c (Sub e1 e2) = evalInfix c e1 e2 sub
eval c (Mul e1 e2) = evalInfix c e1 e2 mul
eval c (Div e1 e2) = evalInfix c e1 e2 E.div
eval c (Mod e1 e2) = evalInfix c e1 e2 E.mod
eval c (Exp e1 e2) = evalInfix c e1 e2 E.exp
eval c (Dot e1 e2) = evalInfix c e1 e2 dot

eval c (Variable name) = name `M.lookup` (variables c) >>= eval c

eval (Context vars funcs) (Function name e) =
    do arg <- eval (Context vars funcs) e
       (argName, functionExpr) <- name `M.lookup` funcs
       let localVars = insert argName arg vars
       eval (Context localVars funcs) functionExpr

eval c x = Just x


evalInfix :: Context -> Expr -> Expr -> (Expr -> Expr -> Maybe Expr) -> Maybe Expr
evalInfix c e1 e2 f = do a <- eval c e1
                         b <- eval c e2
                         f a b
