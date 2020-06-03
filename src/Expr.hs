module Expr where

import           Data.List

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
    show (Rational a)      = show a
    show (Imaginary b)     = show b ++ "i"
    show (Complex a b)     = show a ++ " + " ++ show (Imaginary b)
    show (Add e1 e2)       = show e1 ++ " + " ++ show e2
    show (Sub e1 e2)       = show e1 ++ " - " ++ show e2
    show (Mul e1 e2)       = show e1 ++ " * " ++ show e2
    show (Div e1 e2)       = show e1 ++ " / " ++ show e2
    show (Mod e1 e2)       = show e1 ++ " % " ++ show e2
    show (Exp e1 e2)       = show e1 ++ " ^ " ++ show e2
    show (Dot e1 e2)       = show e1 ++ " ** " ++ show e2
    show (Variable name)   = name
    show (Function name e) = name ++ "(" ++ show e ++ ")"

    show (Matrix rows)     = intercalate "\n" $ map showRow rows
        where showRow r = "[ " ++ (intercalate ", " $ map show r) ++ " ]"


-------------------------------------------------------------------------------
-- Operators
-------------------------------------------------------------------------------

add :: Expr -> Expr -> Maybe Expr

add (Rational a)    (Rational b)    = Just $ Rational (a + b)
add (Rational a)    (Imaginary b)   = Just $ Complex a b
add (Rational a)    (Complex br bi) = Just $ Complex (br + a) bi

add (Imaginary a)   (Imaginary b)   = Just $ Imaginary (a + b)
add (Imaginary a)   (Rational b)    = Just $ Complex b a
add (Imaginary a)   (Complex br bi) = Just $ Complex br (a + bi)

add (Complex ar ai) (Complex br bi) = Just $ Complex (ar + br) (ai + bi)
add (Complex ar ai) (Rational b)    = Just $ Complex (ar + b) ai
add (Complex ar ai) (Imaginary b)   = Just $ Complex ar (ai + b)

add _ _                             = Nothing


sub :: Expr -> Expr -> Maybe Expr
sub a b = add a =<< Rational (-1) `mul` b


mul :: Expr -> Expr -> Maybe Expr
mul (Rational a) (Rational b)     = Just $ Rational (a * b)
mul (Rational a) (Imaginary b)    = Just $ Imaginary (a * b)
mul (Rational a) (Complex br bi)  = Just $ Complex (a * br) (a * bi)

mul (Imaginary a) (Imaginary b)   = Just $ Imaginary (a * b)
mul (Imaginary a) (Rational b)    = Just $ Complex b a
mul (Imaginary a) (Complex br bi) = Just $ Complex (a * br) (a * bi)

mul _ _                           = Nothing


div :: Expr -> Expr -> Maybe Expr
div _ (Rational 0)  = Nothing
div _ (Imaginary 0) = Nothing
div _ (Complex 0 0) = Nothing
div a b             = mul a =<< b `Expr.exp` Rational (-1)


mod :: Expr -> Expr -> Maybe Expr
mod _ _ = Nothing


exp :: Expr -> Expr -> Maybe Expr
exp (Rational a)  (Rational b) = Just $ Rational (a ** b)

exp (Imaginary a) (Rational b)
  | b < 0     = Expr.div (Rational 1) =<< Imaginary a `Expr.exp` Rational b
  | b == 0    = Just $ Rational a
  | b == 1    = Just $ Imaginary a
  | b == 2    = Just $ Rational (-a)
  | b == 3    = Just $ Imaginary (-a)
  | otherwise = Imaginary a `Expr.exp` Rational (b - 4)

exp _ _ = Nothing


dot :: Expr -> Expr -> Maybe Expr
dot (Matrix a) (Matrix b) = undefined
dot _ _                   = Nothing
