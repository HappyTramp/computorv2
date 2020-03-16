module Parser.Expr where

import Control.Applicative

import Parser.Core
import Expr


imaginaryP :: Parser Atom
imaginaryP = Imaginary <$> (floatP <* char 'i')

rationalP :: Parser Atom
rationalP = Rational <$> floatP

matrixP :: Parser Atom
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
          endpointP = parenthesisExprP <|> (EAtom <$> atomP) <|> functionP <|> variableP

variableP :: Parser Expr
variableP = Variable <$> alphaStringP

functionP :: Parser Expr
functionP = Function <$> alphaStringP <*> parenthesisExprP

parenthesisExprP :: Parser Expr
parenthesisExprP = parenthesize exprP

atomP :: Parser Atom
atomP = imaginaryP <|> rationalP <|> matrixP
