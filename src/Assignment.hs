module Assignment where

import Data.List
import qualified Expr as E

data Assignment
    = Variable String E.Expr
    | Function String String E.Expr

instance Eq Assignment where
    (Variable n1 _) == (Variable n2 _) = n1 == n2
    (Function n1 _ _) == (Function n2 _ _) = n1 == n2
    _ == _ = False

name :: Assignment -> String
name (Variable n _) = n
name (Function n _ _) = n

-- data Context a =  Context { vars :: [Assignment], payload :: a }
type Context = [Assignment]

-- instance Functor Context where
--     fmap f (Context as x) = Context as (f x)
--
-- instance Applicative Context where
--     pure x = Context [] x
--     (Context a1 f) <*> (Context a2 x) = Context (a1 `union` a2) (f x)
--
-- instance Monad Context where
--     return = pure
--     (Context a1 x) >>= f = Context (vars res `union` a1) (payload res)
--         where res = f x


update :: Context -> Assignment -> Context
update context a
  | a `elem` context = map replaceIf context
  | otherwise = a:context
  where replaceIf a' = if a' == a then a else a'

get :: Context -> String -> Maybe Assignment
get context n = case found of [] -> Nothing
                              [a] -> Just a
    where found = filter (\a -> name a == n) context

instance Show Assignment where
    show (Variable name e) = name ++ " = " ++ show e
    show (Function name arg e) = name ++ "(" ++ arg ++ ") = " ++ show e
