module Parser.Statement where

import Control.Applicative
import Parser.Core
import Parser.Expr
import Parser.Assignment
import Statement


statementP :: Parser Statement
statementP = SAssignment <$> assignmentP
             <|> SExpr <$> exprP <* char '=' <* char '?'
