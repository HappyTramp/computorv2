module Parser where
-- ( parse
-- , equationP
-- ) where

import Control.Applicative
import Control.Monad
import Data.Char

import Expr
-- import Equation
-- import Complex


newtype Parser a = Parser (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser p) input = p input

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

sepBy :: Parser b -> Parser a -> Parser [a]
sepBy sep x = many (sep *> x)

sepByMap :: (b -> a -> a) -> Parser b -> Parser a -> Parser [a]
sepByMap f sep x = many (f <$> sep <*> x)

signed :: Num a => Parser a -> Parser a
signed p = do charP '-'
              x <- p
              return (-x)
           <|> p

readParser :: Read a => Parser String -> Parser a
readParser p = read <$> p

charP :: Char -> Parser Char
charP c = satisfyChar (c ==)

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

-- imaginaryP :: Parser Imaginary
-- imaginaryP = floatP <* charP 'i'

exprP :: Parser Expr
exprP = do x <- termP
           charP '+'
           y <- exprP
           return (Expr x y)
        <|> (ExprSingle <$> termP)

termP :: Parser Term
termP = do f <- factorP
           charP '*'
           t <- termP
           return (Term f t)
        <|> (TermSingle <$> factorP)


factorP :: Parser Factor
factorP = do b <- baseP
             charP '^'
             e <- factorP
             return (Factor b e)
          <|> (FactorSingle <$> baseP)

baseP :: Parser Base
baseP = (charP '(' *> (Base <$> exprP) <* charP ')')
        <|> (BaseSingle <$> floatP)
