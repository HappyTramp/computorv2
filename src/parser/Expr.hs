module Parser.Expr where

import Control.Applicative

import Parser.Core
import Expr


imaginaryP :: Parser Expr
imaginaryP = Imaginary <$> (floatP <* char 'i')

rationalP :: Parser Expr
rationalP = Rational <$> floatP

matrixP :: Parser Expr
matrixP = Matrix <$> (char '[' *> sepBy (char ';') matrixRowP <* char ']')
    where matrixRowP = char '[' *> sepBy (char ',') exprP <* char ']'

exprP :: Parser Expr
exprP = termP `chainl1` termOpP
    where termOpP = infixOp "+" Add <|> infixOp "-" Sub

termP :: Parser Expr
termP =  factorP `chainl1` factorOpP
    where factorOpP = infixOp "**" Dot <|> infixOp "*" Mul <|> infixOp "/" Div <|> infixOp "%" Mod

factorP :: Parser Expr
factorP =  endpointP `chainl1` expOpP
    where expOpP = infixOp "^" Exp

          endpointP = parensExprP
                      <|> imaginaryP
                      <|> rationalP
                      <|> matrixP
                      <|> functionP
                      <|> variableP
              where variableP = Variable <$> alphaStringP
                    functionP = Function <$> alphaStringP <*> parensExprP
                    parensExprP = parenthesize exprP
