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
statementP = functionDeclarationP <|> variableDeclarationP <|> evaluationP <|> polynomEvaluationP
    where
        functionDeclarationP = FunctionDeclaration
                                    <$> funLabelP
                                    <*> parenthesis varLabelP
                                    <*> (char '=' *> exprP)

        variableDeclarationP = VariableDeclaration
                                    <$> varLabelP
                                    <*> (char '=' *> exprP)

        polynomEvaluationP   = PolynomEvaluation <$>  exprP <*> (char '=' *> exprP <* char '?')

        evaluationP          = Evaluation <$> exprP <* char '=' <* char '?'
