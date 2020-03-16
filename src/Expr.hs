module Expr where

import Data.List

data Atom
    = Rational Float
    | Imaginary Float
    | Matrix [[Expr]]
    deriving (Eq)

data Expr
    = EAtom Atom
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Mod Expr Expr
    | Exp Expr Expr
    | Dot Expr Expr
    | Variable String
    | Function String Expr
    deriving (Eq)

instance Show Expr where
    show (EAtom a) = show a
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2) = show e1 ++ " - " ++ show e2
    show (Mul e1 e2) = show e1 ++ " * " ++ show e2
    show (Div e1 e2) = show e1 ++ " / " ++ show e2
    show (Mod e1 e2) = show e1 ++ " % " ++ show e2
    show (Exp e1 e2) = show e1 ++ " ^ " ++ show e2
    show (Dot e1 e2) = show e1 ++ " ** " ++ show e2
    show (Variable name) = name
    show (Function name e) = name ++ "(" ++ show e ++ ")"

instance Show Atom where
    show (Rational r) = show r
    show (Imaginary i) = show i ++ "i"
    show (Matrix m) = intercalate "\n" (map showRow m)
        where showRow r = "[ " ++ intercalate ", " (map show r) ++ " ]"
