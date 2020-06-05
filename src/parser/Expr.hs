module Parser.Expr (exprP) where

import           Control.Applicative

import           Expr
import           Parser.Core


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
                  , Rational <$> floatP
                  , matrixP
                  , Function <$> funLabelP <*> parenthesizedExprP
                  , imaginaryP
                  , Variable <$> varLabelP
                  ] `chainl1` (infixOp "^" Exp)
    where
        parenthesizedExprP = parenthesis exprP

        imaginaryP = (Complex 0) <$> (floatP <|> pure 1.0) <* char 'i'

        -- Parse a matrix in the following format:
        -- [ [a, b]; [c, d] ]
        matrixP :: Parser Expr
        matrixP = Matrix <$> (brackets (matrixRowP `sepBy` (char ';')) >>= verify check)
            where matrixRowP = brackets (exprP `sepBy` (char ','))
                  brackets = between "[" "]"
                  check rows
                    | length rows == 0 = False
                    | otherwise        = all ((length (head rows) ==) . length) $ tail rows
