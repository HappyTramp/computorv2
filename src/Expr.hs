module Expr where

import Data.List

data Expr
    = Rational Float
    | Imaginary Float
    | Complex Float Float
    | Matrix [[Expr]]
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
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2) = show e1 ++ " - " ++ show e2
    show (Mul e1 e2) = show e1 ++ " * " ++ show e2
    show (Div e1 e2) = show e1 ++ " / " ++ show e2
    show (Mod e1 e2) = show e1 ++ " % " ++ show e2
    show (Exp e1 e2) = show e1 ++ " ^ " ++ show e2
    show (Dot e1 e2) = show e1 ++ " ** " ++ show e2
    show (Variable name) = name
    show (Function name e) = name ++ "(" ++ show e ++ ")"


-------------------------------------------------------------------------------
-- Operators
-------------------------------------------------------------------------------

builtinAdd :: Expr -> Expr -> Maybe Expr

builtinAdd (Rational a)    (Rational b)    = Just $ Rational (a + b)
builtinAdd (Rational a)    (Imaginary b)   = Just $ Complex a b
builtinAdd (Rational a)    (Complex br bi) = Just $ Complex (br + a) bi

builtinAdd (Imaginary a)   (Imaginary b)   = Just $ Imaginary (a + b)
builtinAdd (Imaginary a)   (Rational b)    = Just $ Complex b a
builtinAdd (Imaginary a)   (Complex br bi) = Just $ Complex br (a + bi)

builtinAdd (Complex ar ai) (Complex br bi) = Just $ Complex (ar + br) (ai + bi)
builtinAdd (Complex ar ai) (Rational b)    = Just $ Complex (ar + b) ai
builtinAdd (Complex ar ai) (Imaginary b)   = Just $ Complex ar (ai + b)

builtinAdd _ _ = Nothing


builtinSub :: Expr -> Expr -> Maybe Expr
builtinSub a b = builtinAdd a =<< (Rational (-1) `builtinMul` b)


-- could be derived from addition
builtinMul :: Expr -> Expr -> Maybe Expr
builtinMul (Rational a) (Rational b)    = Just $ Rational (a * b)
builtinMul (Rational a) (Imaginary b)   = Just $ Imaginary (a * b)
builtinMul (Rational a) (Complex br bi) = Just $ Complex (a * br) (a * bi)

builtinMul (Imaginary a) (Imaginary b)   = Just $ Imaginary (a * b)
builtinMul (Imaginary a) (Rational b)    = Just $ Complex b a
builtinMul (Imaginary a) (Complex br bi) = Just $ Complex (a * br) (a * bi)

builtinMul _ _ = Nothing


builtinDiv :: Expr -> Expr -> Maybe Expr
builtinDiv _ (Rational 0)  = Nothing
builtinDiv _ (Imaginary 0) = Nothing
builtinDiv _ (Complex 0 0) = Nothing
builtinDiv a b = builtinMul a =<< (b `builtinExp` Rational (-1))


builtinMod :: Expr -> Expr -> Maybe Expr
builtinMod _ _ = Nothing


-- could be derived from multiplication
builtinExp :: Expr -> Expr -> Maybe Expr
builtinExp (Rational a)  (Rational b) = Just $ Rational (a ** b)

builtinExp (Imaginary a) (Rational b)
  | b < 0     =  builtinDiv (Rational 1) =<< ((Imaginary a) `builtinExp` (Rational b))
  | b == 0    = Just $ Rational a
  | b == 1    = Just $ Imaginary a
  | b == 2    = Just $ Rational (-a)
  | b == 3    = Just $ Imaginary (-a)
  | otherwise = Imaginary a `builtinExp` (Rational (b - 4))

builtinExp _ _ = Nothing


builtinDot :: Expr -> Expr -> Maybe Expr
builtinDot (Matrix a) (Matrix b) = undefined
builtinDot _ _ = Nothing
