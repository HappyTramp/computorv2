module Expr where

import Atom


data Expr
    = EAtom Atom
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Mod Expr Expr
    | Exp Expr Expr
    -- | Dot Expr Expr


eval :: Expr -> Maybe Atom
eval (EAtom a) = Just a
eval (Add e1 e2) = evalBin e1 e2 (+?)
eval (Sub e1 e2) = evalBin e1 e2 (-?)
eval (Mul e1 e2) = evalBin e1 e2 (*?)
eval (Div e1 e2) = evalBin e1 e2 (/?)
eval (Mod e1 e2) = evalBin e1 e2 (%?)
eval (Exp e1 e2) = evalBin e1 e2 (^?)

evalBin :: Expr -> Expr -> (Atom -> Atom -> Maybe Atom) -> Maybe Atom
evalBin e1 e2 f = do a <- eval e1
                     b <- eval e2
                     f a b

instance Show Expr where
    show (EAtom a) = show a
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2) = show e1 ++ " - " ++ show e2
    show (Mul e1 e2) = show e1 ++ " * " ++ show e2
    show (Div e1 e2) = show e1 ++ " / " ++ show e2
    show (Mod e1 e2) = show e1 ++ " % " ++ show e2
    show (Exp e1 e2) = show e1 ++ " ^ " ++ show e2
