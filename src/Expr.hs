module Expr where

import Data.List


data Atom
    = Rational Float
    | Imaginary Float
    | Matrix [[Expr]]

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

eval :: Expr -> Maybe Atom
eval (EAtom a) = Just a
eval (Add e1 e2) = evalInfix e1 e2 (+?)
eval (Sub e1 e2) = evalInfix e1 e2 (-?)
eval (Mul e1 e2) = evalInfix e1 e2 (*?)
eval (Div e1 e2) = evalInfix e1 e2 (/?)
eval (Mod e1 e2) = evalInfix e1 e2 (%?)
eval (Exp e1 e2) = evalInfix e1 e2 (^?)
eval (Dot e1 e2) = evalInfix e1 e2 (**?)
eval _ = Nothing

evalInfix :: Expr -> Expr -> (Atom -> Atom -> Maybe Atom) -> Maybe Atom
evalInfix e1 e2 f = do a <- eval e1
                       b <- eval e2
                       f a b

infixl 6 +?
(+?) :: Atom -> Atom -> Maybe Atom
(Rational a) +? (Rational b) = Just $ Rational (a + b)
(Imaginary a) +? (Imaginary b) = Just $ Imaginary (a + b)
_ +? _ = Nothing

infixl 6 -?
(-?) :: Atom -> Atom -> Maybe Atom
(Rational a) -? (Rational b) = Just $ Rational (a - b)
(Imaginary a) -? (Imaginary b) = Just $ Imaginary (a - b)
_ -? _ = Nothing

infixl 7 *?
(*?) :: Atom -> Atom -> Maybe Atom
(Rational a) *? (Rational b) = Just $ Rational (a * b)
(Rational a) *? (Imaginary b) = Just $ Imaginary (a * b)
(Imaginary a) *? (Imaginary b) = (Imaginary (a * b)) ^? Rational 2
_ *? _ = Nothing

infixl 7 /?
(/?) :: Atom -> Atom -> Maybe Atom
_ /? (Rational 0) = Nothing
(Rational a) /? (Rational b) = Just $ Rational (a / b)
_ /? _ = Nothing

infixl 7 %?
(%?) :: Atom -> Atom -> Maybe Atom
_ %? _ = Nothing

infixr 8 ^?
(^?) :: Atom -> Atom -> Maybe Atom
(Rational a) ^? (Rational b) = Just $ Rational (a ** b)
(Imaginary a) ^? (Rational 0) = Just $ Rational a
(Imaginary a) ^? (Rational 1) = Just $ Imaginary a
(Imaginary a) ^? (Rational 2) = Just $ Rational (-a)
(Imaginary a) ^? (Rational 3) = Just $ Imaginary (-a)
(Imaginary a) ^? (Rational b) = Imaginary a ^? (Rational (b - 4))
_ ^? _ = Nothing

infixr 8 **?
(**?) :: Atom -> Atom -> Maybe Atom
_ **? _ = Nothing

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
