module Parser.Expr where

import Control.Applicative

import Parser.Core
import Expr


imaginaryP :: Parser Expr
imaginaryP = Imaginary <$> (floatP <* char 'i')

rationalP :: Parser Expr
rationalP = Rational <$> floatP

-- Parse a matrix in the following format:
-- [ [a, b]; [c, d] ]
matrixP :: Parser Expr
matrixP = Matrix <$> brackets (matrixRowP `sepBy` (char ';'))
    where matrixRowP = brackets (exprP `sepBy` (char ','))
          brackets = between "[" "]"


-- Parse expression separated by one infix operator of the operator list
operatorChoiceChain :: Parser a -> [Parser (a -> a -> a)] -> Parser a
operatorChoiceChain x operators = x `chainl1` choice operators

-- Parse an expression (lowest operator priority)
exprP :: Parser Expr
exprP = operatorChoiceChain termP
            [ infixOp "+" Add
            , infixOp "-" Sub
            ]

termP :: Parser Expr
termP =  operatorChoiceChain factorP
            [ infixOp "**" Dot
            , infixOp "*" Mul
            , infixOp "/" Div
            , infixOp "%" Mod
            ]

factorP :: Parser Expr
factorP =  choice [ parenthesizedExprP
                  , imaginaryP
                  , rationalP
                  , matrixP
                  , functionP
                  , variableP
                  ] `chainl1` (infixOp "^" Exp)

    where variableP = Variable <$> alphaStringP
          functionP = Function <$> alphaStringP <*> parenthesizedExprP
          parenthesizedExprP = parenthesis exprP
