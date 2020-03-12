module Expr where

import Imag
import Matrix

-- data X = Expr | Imag | Matrix

-- class ExprElement where
--     subExpr :: ExprElement a => a -> a
--     composed :: ExprElement a -> a -> (a, a)

data AExpr  = AExpr Term AExpr   | AExprSingle Term
data Term   = Term Factor Term   | TermSingle Factor
data Factor = Factor Base Factor | FactorSingle Base
data Base   = Base AExpr         | BaseSingle Expr

data Expr   = ExprF Float | ExprI Imag | ExprM (Matrix AExpr)

instance Show AExpr where
    show (AExprSingle t) = show t
    show (AExpr t e) = show t ++ " + " ++ show e

instance Show Term where
    show (TermSingle f) = show f
    show (Term f t) = show f ++ " * " ++ show t

instance Show Factor where
    show (FactorSingle b) = show b
    show (Factor b f) = show b ++ " ^ " ++ show f

instance Show Base where
    show (BaseSingle x) = show x
    show (Base e) = "(" ++ show e ++ ")"

instance Show Expr where
    show (ExprF f) = show f
    show (ExprI i) = show i
    show (ExprM m) = show m

-- eval :: Expr -> Float
-- eval
