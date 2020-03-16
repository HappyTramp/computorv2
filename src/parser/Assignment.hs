module Parser.Assignment where

import Control.Applicative
import Parser.Core
import qualified Parser.Expr as E
import Assignment

variableP :: Parser Assignment
variableP = Variable <$> alphaStringP <* char '=' <*> E.exprP

functionP :: Parser Assignment
functionP = Function <$> alphaStringP <*> parenthesize alphaStringP <* char '=' <*> E.exprP

assignmentP :: Parser Assignment
assignmentP = variableP <|> functionP
