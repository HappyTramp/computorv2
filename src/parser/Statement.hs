module Parser.Statement where

import           Control.Applicative

import           Expr
import           Parser.Core
import           Parser.Expr


data Statement
    = Evaluation Expr
    | PolynomEvaluation Expr Expr
    | VariableDeclaration String Expr
    | FunctionDeclaration String String Expr

statementP :: Parser Statement
statementP = functionDeclarationP <|> variableDeclarationP <|> polynomEvaluationP <|> evaluationP
    where
        functionDeclarationP = FunctionDeclaration
                                    <$> labelP
                                    <*> parenthesis labelP
                                    <*> (char '=' *> exprP)

        variableDeclarationP = VariableDeclaration
                                    <$> labelP
                                    <*> (char '=' *> exprP)

        polynomEvaluationP   = PolynomEvaluation <$>  exprP <*> (char '=' *> exprP <* char '?')

        evaluationP          = Evaluation <$> exprP <* char '=' <* char '?'
