module Parser where

import Control.Applicative
import Control.Monad
import Data.Char

import Atom
import Expr

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

parseStrict :: Parser a -> String -> Maybe a
parseStrict p input = case parse p input of Just (a, "") -> Just a
                                            _            -> Nothing

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser new_p
        where new_p s = do
                (x, s') <- p s
                return (f x, s')

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = Parser (\s -> Just (x, s))
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser p1) <*> (Parser p2) = Parser new_p
        where new_p s = do
                (f, s') <- p1 s
                (x, s'') <- p2 s'
                return (f x, s'')

instance Alternative Parser where
    -- empty :: Parser a
    empty = Parser (\_ -> Nothing)
    -- (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p1) <|> (Parser p2) = Parser new_p
        where new_p s = p1 s <|> p2 s

instance Monad Parser where
    -- return :: a -> Parser a
    return x = pure x
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p1) >>= f = Parser new_p
        where new_p s = do
                (x, s') <- p1 s
                parse (f x) s'


satisfyChar :: (Char -> Bool) -> Parser Char
satisfyChar f = Parser p
    where p []     = Nothing
          p (c:cs) = if f c then Just (c, cs)
                            else Nothing

-- sepBy :: Parser b -> Parser a -> Parser [a]
-- sepBy sep x = (:) <$> x <*> (many (sep *> x))
-- sepByMap :: (b -> a -> a) -> Parser b -> Parser a -> Parser [a]
-- sepByMap f sep x = (:) <$> x <*> (many (f <$> sep <*> x))

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = chainl1 p op <|> pure a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do first <- p
                  rest first
    where rest prev = do f <- op
                         e <- p
                         rest (f prev e)
                      <|> return prev

signed :: Num a => Parser a -> Parser a
signed p = do charP '-'
              x <- p
              return (-x)
           <|> p

readParser :: Read a => Parser String -> Parser a
readParser p = read <$> p

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp opStr f = stringP opStr *> pure f

parenthesize :: Parser a -> Parser a
parenthesize p = charP '(' *> p <* charP ')'

charP :: Char -> Parser Char
charP c = satisfyChar (c ==)

stringP :: String -> Parser String
stringP s = sequenceA $ charP <$> s

alphaP :: Parser Char
alphaP = satisfyChar isAlpha

digitsP :: Parser String
digitsP = some (satisfyChar isDigit) -- at least one digit to avoid read exception

spacesP :: Parser String
spacesP = many (satisfyChar isSpace)

unsignedIntP :: Parser Int
unsignedIntP = readParser digitsP

intP :: Parser Int
intP = signed unsignedIntP

unsignedFloatP :: Parser Float
unsignedFloatP = readParser p
    where p = do pos <- digitsP
                 charP '.'
                 dec <- digitsP
                 return (pos ++ "." ++ dec)
              <|> digitsP

floatP :: Parser Float
floatP = signed unsignedFloatP

imaginaryP :: Parser Atom
imaginaryP = AImaginary <$> (floatP <* charP 'i')

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

factorP :: Parser Expr
factorP =  endpointP `chainl1` expOpP
    where endpointP = parenthesisP <|> (EAtom <$> atomP)

parenthesisP :: Parser Expr
parenthesisP = parenthesize exprP

atomP :: Parser Atom
atomP = imaginaryP <|> rationalP
