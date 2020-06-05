module Expr where

import           Control.Monad
import           Data.List
import qualified Data.Map      as M

-- data Operand
--     = Rational Float
--     | Complex Float Float
--     | Matrix [[Operand]]
--
--
-- data (Operable a, Operable b) => Operator a b
--     = Add a b
--     | Sub a b
--     | Mul a b
--     | Div a b
--     | Mod a b
--     | Exp a b
--     | Dot a b
--
-- data Label
--     = Variable String
--     | Function String Expr

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

-------------------------------------------------------------------------------
-- Operators
-------------------------------------------------------------------------------

add :: Expr -> Expr -> Maybe Expr

add (Rational a)    (Rational b)    = Just $ Rational (a + b)
add (Rational a)    (Complex br bi) = Just $ Complex (br + a) bi

add (Complex ar ai) (Complex br bi) = Just $ Complex (ar + br) (ai + bi)
add (Complex ar ai) (Rational b)    = Just $ Complex (ar + b) ai

add _ _                             = Nothing


sub :: Expr -> Expr -> Maybe Expr
sub a b = add a =<< Rational (-1) `mul` b


mul :: Expr -> Expr -> Maybe Expr
mul (Rational a) (Rational b)     = Just $ Rational (a * b)
mul (Rational a) (Complex br bi)  = Just $ Complex (a * br) (a * bi)


mul _ _                           = Nothing


div :: Expr -> Expr -> Maybe Expr
div _ (Rational 0)  = Nothing
div _ (Complex 0 0) = Nothing
div a b             = mul a =<< b `Expr.exp` Rational (-1)


mod :: Expr -> Expr -> Maybe Expr
mod _ _ = Nothing


exp :: Expr -> Expr -> Maybe Expr
exp (Rational a)  (Rational b) = Just $ Rational (a ** b)
exp _ _ = Nothing


dot :: Expr -> Expr -> Maybe Expr
dot (Matrix a) (Matrix b)
    | shape a == shape bT = Matrix <$> mapM (\aRow -> mapM (dotProd aRow) bT) a
    | otherwise           = Nothing
    where bT = transpose b
          shape m = [length m, length (head m)]

          dotProd :: [Expr] -> [Expr] -> Maybe Expr
          dotProd r c = foldM add (Rational 0) =<< zipWithM mul r c

dot _ _                   = Nothing


-------------------------------------------------------------------------------
-- Evaluation
-------------------------------------------------------------------------------

type LabelMap a = M.Map String a
data Context = Context { variables :: LabelMap Expr
                       , functions :: LabelMap (String, Expr)
                       }

eval :: Expr -> Context -> Maybe Expr

eval (Add e1 e2) c = evalInfix e1 e2 add c
eval (Sub e1 e2) c = evalInfix e1 e2 sub c
eval (Mul e1 e2) c = evalInfix e1 e2 mul c
eval (Div e1 e2) c = evalInfix e1 e2 Expr.div c
eval (Mod e1 e2) c = evalInfix e1 e2 Expr.mod c
eval (Exp e1 e2) c = evalInfix e1 e2 Expr.exp c
eval (Dot e1 e2) c = evalInfix e1 e2 dot c

eval (Variable name) c = name `M.lookup` (variables c) >>= (\e -> eval e c)

eval (Function name e) (Context vars funcs) =
    do arg <- eval e (Context vars funcs)
       (argName, functionExpr) <- name `M.lookup` funcs
       let localVars = M.insert argName arg vars
       eval functionExpr (Context localVars funcs)

eval (Matrix m) c = Matrix <$> mapM (mapM (\e -> eval e c)) m

eval x _
    | isLitteral x = Just x
    | otherwise    = Nothing


evalInfix :: Expr -> Expr -> (Expr -> Expr -> Maybe Expr) -> Context -> Maybe Expr
evalInfix e1 e2 f c = do a <- eval e1 c
                         b <- eval e2 c
                         f a b
