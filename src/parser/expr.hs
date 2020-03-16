module Parser.Expr where

import Control.Applicative

import Parser.Core
import Atom
import Expr


imaginaryP :: Parser Atom
imaginaryP = AImaginary <$> (floatP <* char 'i')

rationalP :: Parser Atom
rationalP = ARational <$> floatP

termOpP :: Parser (Expr -> Expr -> Expr)
termOpP = infixOp "+" Add <|> infixOp "-" Sub

factorOpP :: Parser (Expr -> Expr -> Expr)
factorOpP = infixOp "*" Mul <|> infixOp "/" Div <|> infixOp "%" Mod

expOpP :: Parser (Expr -> Expr -> Expr)
expOpP = infixOp "^" Exp

exprP :: Parser Expr
exprP = termP `chainl1` termOpP

termP :: Parser Expr
termP =  factorP `chainl1` factorOpP

variableP :: Parser Expr
variableP = Variable <$> alphaStringP

functionP :: Parser Expr
functionP = Function <$> alphaStringP <*> parenthesisExprP

factorP :: Parser Expr
factorP =  endpointP `chainl1` expOpP
    where endpointP = parenthesisExprP <|> (EAtom <$> atomP) <|> functionP <|> variableP

parenthesisExprP :: Parser Expr
parenthesisExprP = parenthesize exprP

atomP :: Parser Atom
atomP = imaginaryP <|> rationalP
