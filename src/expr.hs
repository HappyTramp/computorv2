module Expr where

-- data X = Expr | Imag | Matrix

data Expr   = Expr Term Expr     | ExprSingle Term
data Term   = Term Factor Term   | TermSingle Factor
data Factor = Factor Base Factor | FactorSingle Base
data Base   = Base Expr          | BaseSingle Float

instance Show Expr where
    show (ExprSingle t) = show t
    show (Expr t e) = show t ++ " + " ++ show e

instance Show Term where
    show (TermSingle f) = show f
    show (Term f t) = show f ++ " * " ++ show t

instance Show Factor where
    show (FactorSingle b) = show b
    show (Factor b f) = show b ++ " ^ " ++ show f

instance Show Base where
    show (BaseSingle x) = show x
    show (Base e) = "( " ++ show e ++ " )"
