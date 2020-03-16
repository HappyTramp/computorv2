module Statement where

import Assignment
import Expr


data Statement
    = SAssignment Assignment
    | SExpr Expr

instance Show Statement where
    show (SAssignment a) = show a
    show (SExpr e) = show e

