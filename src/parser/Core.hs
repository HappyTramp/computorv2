{-# LANGUAGE FlexibleInstances #-}

module Parser.Core where

import           Control.Applicative
import           Data.Char


type Result a = Either String a
newtype Parser a = Parser { runParser :: String -> Result (a, String) }

runParserStrict :: Parser a -> String -> Result a
runParserStrict p input = case runParser p input of
    Right (a, "")   -> Right a
    Right (_, rest) -> Left $ "Unexpected string: \"" ++ rest ++ "\""
    Left err        -> Left err

-------------------------------------------------------------------------------
-- Parser instances
-------------------------------------------------------------------------------

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $
        \s -> do (x, s') <- p s
                 return (f x, s')

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = Parser (\s -> Right (x, s))
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser pf) <*> (Parser p) = Parser $
        \s -> do (f, s')  <- pf s
                 (x, s'') <- p s'
                 return (f x, s'')

instance Monad Parser where
    -- return :: a -> Parser a
    return x = pure x
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p1) >>= f = Parser $
        \s -> do (x, s') <- p1 s
                 runParser (f x) s'

-- instance for Either String so that it can be used in the Alternative for Parser
instance Alternative (Either String) where
    -- empty :: Result a
    empty = Left ""
    -- (<|>) :: Result a -> Result a -> Result a
    Left _ <|> x2 = x2
    x1     <|> _  = x1

instance Alternative Parser where
    -- empty :: Parser a
    empty = Parser (\_ -> empty)
    -- (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s


-------------------------------------------------------------------------------
-- Parser creation helper
-------------------------------------------------------------------------------

-- Create a parser of one character which must respect a predicate
satisfyChar :: (Char -> Bool) -> Parser Char
satisfyChar predicate = Parser p
    where p []     = Left "Expected input"
          p (c:cs) = if predicate c then Right (c, cs)
                                    else Left $ "Unexpected char '" ++ [c] ++ "'"

char :: Char -> Parser Char
char c = satisfyChar (c ==)

string :: String -> Parser String
string s = sequenceA $ char <$> s

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy x sepatator = (:) <$> x <*> (many (sepatator *> x))

-- Parse one or more occurences of p separated by op
-- Apply op in a left associative maner on each value in p
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
    where rest prev = do f <- op
                         operand <- p
                         rest (f prev operand)
                      <|> return prev

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp operatorStr f = string operatorStr *> pure f

-- Surround parser with opening and closing string
between :: String -> String -> Parser a -> Parser a
between open close p = string open *> p <* string close

parenthesis :: Parser a -> Parser a
parenthesis p = between "(" ")" p

-- try to apply parsers returns the first one that succeeds
choice :: [Parser a] -> Parser a
choice []     = empty
choice (p:ps) = p <|> choice ps

verify :: (a -> Bool) -> a -> Parser a
verify predicate x = if predicate x then pure x else empty

-- Parse a string of alpha character
-- Convert to lower case and check that the label isn't `i`
varLabelP :: Parser String
varLabelP = (map toLower <$> some (satisfyChar isAlpha)) >>= verify (/= "i")

funLabelP :: Parser String
funLabelP = map toLower <$> some (satisfyChar isAlpha)

floatP :: Parser Float
floatP = signed unsignedP
    where
        unsignedP :: Parser Float
        unsignedP = read <$> p
            where p = do pos <- digitsP
                         _ <- char '.'
                         dec <- digitsP
                         return (pos ++ "." ++ dec)
                      <|> digitsP

                  digitsP = some $ satisfyChar isDigit -- at least one digit to avoid read exception

        signed :: Num a => Parser a -> Parser a
        signed p = do _ <- char '-'
                      x <- p
                      return (-x)
                     <|> p
