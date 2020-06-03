module Parser.Statement where

import           Control.Applicative

import           Expr
import           Parser.Core
import           Parser.Expr


data Statement
    = Evaluation Expr
    | VariableDeclaration String Expr
    | FunctionDeclaration String String Expr

statementP :: Parser Statement
statementP = functionDeclarationP <|> variableDeclarationP <|> evaluationP
    where
        functionDeclarationP = FunctionDeclaration
                                    <$> alphaStringP
                                    <*> parenthesis alphaStringP
                                    <*> (char '=' *> exprP)

        variableDeclarationP = VariableDeclaration
                                    <$> alphaStringP
                                    <*> (char '=' *> exprP)

        evaluationP          = Evaluation <$> exprP <* char '=' <* char '?'
