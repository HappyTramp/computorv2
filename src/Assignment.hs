module Assignment where

import qualified Expr as E

data Assignment
    = Variable String E.Expr
    | Function String String E.Expr
    deriving (Eq)

name :: Assignment -> String
name (Variable n _) = n
name (Function n _ _) = n

type Context = [Assignment]

update :: Context -> Assignment -> Context
update context a
  | name a `elem` map name context = map replaceIf context
  | otherwise = a:context
  where replaceIf a' = if name a' == name a then a else a'

get :: Context -> String -> Maybe Assignment
get context n = case found of [] -> Nothing
                              [a] -> Just a
    where found = filter (\a -> name a == n) context

instance Show Assignment where
    show (Variable name e) = name ++ " = " ++ show e
    show (Function name arg e) = name ++ "(" ++ arg ++ ") = " ++ show e
