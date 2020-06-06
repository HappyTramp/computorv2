module Expr where

import           Control.Monad
import           Data.List
import qualified Data.Map      as M

data Expr
    = Rational Float  -- values
    | Complex Float Float
    | Matrix [[Expr]]

    | Add Expr Expr --- ops
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Mod Expr Expr
    | Exp Expr Expr
    | Dot Expr Expr

    | Variable String  -- lables
    | Function String Expr
    deriving (Eq)

instance Show Expr where
    show (Rational a)      = show a
    show (Complex a b)     = (if a /= 0 then show a ++ " + " else "") ++ show b ++ "i"
    show (Add e1 e2)       = show e1 ++ " + " ++ show e2
    show (Sub e1 e2)       = show e1 ++ " - " ++ show e2
    show (Mul e1 e2)       = show e1 ++ " * " ++ show e2
    show (Div e1 e2)       = show e1 ++ " / " ++ show e2
    show (Mod e1 e2)       = show e1 ++ " % " ++ show e2
    show (Exp e1 e2)       = show e1 ++ " ^ " ++ show e2
    show (Dot e1 e2)       = show e1 ++ " ** " ++ show e2
    show (Variable name)   = name
    show (Function name e) = name ++ "(" ++ show e ++ ")"

    show (Matrix rows)     = intercalate "\n" $ map showRow rows
        where showRow r = "[ " ++ (intercalate ", " $ map show r) ++ " ]"

isLitteral :: Expr -> Bool
isLitteral (Rational _)  = True
isLitteral (Complex _ _) = True
isLitteral _             = False

isIgnored :: Expr -> Bool
isIgnored (Variable _) = True
-- isIgnored (Function _ _) = True
isIgnored (Add _ _) = True
isIgnored (Sub _ _) = True
isIgnored (Mul _ _) = True
isIgnored (Div _ _) = True
isIgnored (Mod _ _) = True
isIgnored (Exp _ _) = True
isIgnored (Dot _ _) = True
isIgnored _ = False

-------------------------------------------------------------------------------
-- Operators
-------------------------------------------------------------------------------

add :: Expr -> Expr -> Maybe Expr
add (Rational a)    (Rational b)    = Just $ Rational (a + b)
add (Rational a)    (Complex br bi) = Just $ Complex (br + a) bi
add (Complex ar ai) (Complex br bi) = Just $ Complex (ar + br) (ai + bi)
add (Complex ar ai) (Rational b)    = Just $ Complex (ar + b) ai
add a b                   = ignoreCheck a b Add

sub :: Expr -> Expr -> Maybe Expr
sub a b = add a =<< Rational (-1) `mul` b

mul :: Expr -> Expr -> Maybe Expr
mul (Rational a) (Rational b)     = Just $ Rational (a * b)
mul (Rational a) (Complex br bi)  = Just $ Complex (a * br) (a * bi)
mul a b                   = ignoreCheck a b Mul

div :: Expr -> Expr -> Maybe Expr
div _ (Rational 0)  = Nothing
div _ (Complex 0 0) = Nothing
div a b             = mul a =<< b `Expr.exp` Rational (-1)

mod :: Expr -> Expr -> Maybe Expr
mod a b                   = ignoreCheck a b Mod

exp :: Expr -> Expr -> Maybe Expr
exp (Rational a)  (Rational b) = Just $ Rational (a ** b)
exp a b                   = ignoreCheck a b Exp

dot :: Expr -> Expr -> Maybe Expr
dot (Matrix a) (Matrix b)
    | shape a == shape bT = Matrix <$> mapM (\aRow -> mapM (dotProd aRow) bT) a
    | otherwise           = Nothing
    where bT = transpose b
          shape m = [length m, length (head m)]

          dotProd :: [Expr] -> [Expr] -> Maybe Expr
          dotProd r c = foldM add (Rational 0) =<< zipWithM mul r c

dot a b                   = ignoreCheck a b Dot


ignoreCheck :: Expr -> Expr -> (Expr -> Expr -> Expr) -> Maybe Expr
ignoreCheck a b constructor
    | isIgnored a || isIgnored b = Just $ constructor a b
    | otherwise = Nothing


-------------------------------------------------------------------------------
-- Evaluation
-------------------------------------------------------------------------------

type LabelMap a = M.Map String a
data Context = Context { variables :: LabelMap Expr
                       , functions :: LabelMap (String, Expr)
                       }

evalIgnored :: Expr -> Context -> String -> Maybe Expr

evalIgnored (Variable name) c i
    | name == i = Just $ Variable name
    | otherwise = name `M.lookup` (variables c) >>= (\e -> evalIgnored e c i)

evalIgnored (Function name e) (Context vars funcs) i =
    do arg <- evalIgnored e (Context vars funcs) i
       (argName, functionExpr) <- name `M.lookup` funcs
       let localVars = M.insert argName arg vars
       evalIgnored functionExpr (Context localVars funcs) i

evalIgnored (Matrix m) c i = Matrix <$> mapM (mapM (\e -> evalIgnored e c i)) m

evalIgnored (Add e1 e2) c i = evalIgnoredInfix e1 e2 Add add c i
evalIgnored (Sub e1 e2) c i = evalIgnoredInfix e1 e2 Sub sub c i
evalIgnored (Mul e1 e2) c i = evalIgnoredInfix e1 e2 Mul mul c i
evalIgnored (Div e1 e2) c i = evalIgnoredInfix e1 e2 Div Expr.div c i
evalIgnored (Mod e1 e2) c i = evalIgnoredInfix e1 e2 Mod Expr.mod c i
evalIgnored (Exp e1 e2) c i = evalIgnoredInfix e1 e2 Exp Expr.exp c i
evalIgnored (Dot e1 e2) c i = evalIgnoredInfix e1 e2 Dot dot c i

evalIgnored x _ _
    | isLitteral x = Just x
    | otherwise    = Nothing


evalIgnoredInfix :: Expr -> Expr ->
        (Expr -> Expr -> Expr) ->
        (Expr -> Expr -> Maybe Expr) ->
        Context -> String -> Maybe Expr
evalIgnoredInfix e1 e2 cons f c i

    | isVariable e1 && varName e1 == i && isVariable e2 && varName e2 == i = Just $ cons e1 e2

    | isVariable e1 && varName e1 == i = evalIgnored e2 c i >>= (\x -> Just $ cons e1 x)

    | isVariable e2 && varName e2 == i = evalIgnored e1 c i >>= (\x -> Just $ cons x e2)

    | otherwise = do a <- evalIgnored e1 c i
                     b <- evalIgnored e2 c i
                     f a b
    where varName (Variable name) = name
          varName _ = ""
          isVariable (Variable _) = True
          isVariable _ = False


-- bubbleVariable :: String -> Expr -> Expr -- with tree rotation
-- bubbleVariable i (Add l r)


eval :: Expr -> Context -> Maybe Expr
eval e c = evalIgnored e c ""
