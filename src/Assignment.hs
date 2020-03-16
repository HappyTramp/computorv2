module Assignment where

import qualified Expr as E

data Assignment
    = Variable String E.Expr
    | Function String String E.Expr

instance Show Assignment where
    show (Variable name e) = name ++ " = " ++ show e
    show (Function name arg e) = name ++ "(" ++ arg ++ ") = " ++ show e
