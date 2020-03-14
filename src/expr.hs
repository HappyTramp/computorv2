module Expr where

import Data.List

type Label = String

class Expression where
    eval :: Expression a => a -> Maybe a
    eval (Context _ e) = Just e
    interpret :: Context a -> a
    reduce :: Expression a => a -> a
    reduce e = e

-- class ExpressionNum where
--     evalNum :: ExpressionNum a, Num a => Context a -> Maybe a
--     evalNum (Context c n) = eval n


-- leaf type
newtype Rational  = Float deriving (Expression)
newtype Imaginary = Float deriving (Expression)
newtype Matrix a  = [MatrixRow a]
type MatrixRow a  = [a]
type Var          = Label

-- recursive types
data Func a = Func
    { name     :: Label
    , argument :: a
    }

class MaybeNum where
    (+) :: a -> b -> Maybe c
    _ + _  = Nothing

    (-) :: a -> b -> Maybe c
    _ - _  = Nothing

    (*) :: a -> b -> Maybe c
    _ * _  = Nothing

    (/) :: a -> b -> Maybe c
    _ / _  = Nothing

    (%) :: a -> b -> Maybe c
    _ % _  = Nothing

    (^) :: a -> b -> Maybe c
    _ ^ _  = Nothing

    (**) :: a -> b -> Maybe c
    _ ** _  = Nothing


instance MaybeNum Rational where
    (Rational a) + (Rational b) = Just (a + b)
    (Rational a) + (Imaginary b) = Just (Complex a b)

    (Rational a) - (Rational b) = Just (a - b)
    (Rational a) - (Imaginary b) = Just (Complex a (-b))

    (Rational a) * (Rational b) = Just (a * b)
    (Rational a) * (Imaginary b)     = Just (Imag (a * b))
    (Rational a) * (Matrix b)   = Just (fmap (a*) b)

    _ / (Rational 0) = Nothing
    (Rational a) / (Rational b) = Just (a * b)
    -- x / yi possible?

    -- (Rational a) % (Rational b)  = Nothing

    (Rational a) ^ (Rational b) = Just (a ^ b)

instance MaybeNum Imaginary where
    (Imaginary a) + (Imaginary b) = Just $ Imaginary (a + b)
    (Imaginary a) + (Rational b) = Just (Complex b a)

    (Imaginary a) - (Imaginary b) = Just $ Imaginary (a - b)
    (Imaginary a) - (Rational b) = Just $ Complex (-b) a

    (Imaginary a) * (Imaginary b) = Just $ Imaginary (a * b)

    (Imaginary a) ^ (Rational 0) = Just $ Rational a
    (Imaginary a) ^ (Rational 1) = Just $ Imaginary a
    (Imaginary a) ^ (Rational 2) = Just $ Rational (-a)
    (Imaginary a) ^ (Rational 3) = Just $ Imaginary (-a)
    (Imaginary a) ^ (Rational b) = Just $ a ^ (b - 4)

instance MaybeNum Matrix where
    (Matrix a) + (Matrix b) = if shape a == shape b then Just $ (+) <$> a <*> b
                                                    else Nothing
    (Matrix a) - (Matrix b) = if shape a == shape b then Just $ (-) <$> a <*> b
                                                    else Nothing
    (Matrix a) * (Rational b) = Just $ fmap (*b) a
    (Matrix a) / (Rational b) = Just $ fmap (/b) a

    (Matrix a) ^ (Rational 1) = Just a
    (Matrix a) ^ (Rational b) = if isSquare a then a ** (a ^ (b - 1))
                                              else Nothing

    (Matrix a) ** (Matrix b) = undefined


data (Evaluable a, MaybeNum b) => Ast a b = Operation a (Ast a b) (Ast a b) | Operand b

instance Functor Ast where
    fmap f (Operation op l r) = Operation op (fmap f l) (fmap f r)
    fmap f (Operand x) = Operand (f x)

--
-- data BinExpr a a = BinExpr a a | BinEmpty
-- data Sum    = Sum    Term   Sum    | SumSingle    Term
-- data Term   = Term   Factor Term   | TermSingle   Factor
-- data Factor = Factor Base   Factor | FactorSingle Base
-- data Base   = Base   Expr          | BaseSingle   Expr

-- data X a = X a X | XSingle a



-- data Sum a = Sum a a

-- instance Expression a, Num a => Expression (Sum a) where
--     eval (Context c (Sum x y)) = (+) <$> eval (Context c x) <*> eval (Context c y)
--
-- data Multiplication a = Multiplication a a
-- instance Expression a, Num a => Expression (Multiplication a) where
--     eval (Context c (Multiplication x y)) = (*) <$> eval (Context c x) <*> eval (Context c y)

Bin b => b a  (a -> a -> a) -> a
x f = f <$> eval fst x <*> eval snd x

-- dot :: Matrix a -> Matrix a -> a

-- evalNum :: Num a => (a -> a -> a) -> b a -> b a -> Maybe a
-- evalNum f x y = f <$> eval x <*> eval y


-- data FuncExpr = FuncExpr
--     { getName :: String
--     , getParam :: Expr
--     }

-- data Expr
--     = ExprR Real
--     | ExprI Imag
--     | ExprM (Matrix Expr)
--     | ExprV Var
--     | ExprF FuncEval


data FuncDecl = FuncDecl
    { getName :: String
    , getArg :: String
    , getContent :: Expr
    }

instance Show a => Show (Matrix a) where
    show (Matrix m) = intercalate "\n" (map showLine m)
        where showLine l = "[ " ++ intercalate " , " (map show l) ++ " ]"

instance Show Imag where
    show (Imag i) = show i ++ "i"

-- instance Show AExpr where
--     show (AExprSingle t) = show t
--     show (AExpr t e) = show t ++ " + " ++ show e
--
-- instance Show Term where
--     show (TermSingle f) = show f
--     show (Term f t) = show f ++ " * " ++ show t
--
-- instance Show Factor where
--     show (FactorSingle b) = show b
--     show (Factor b f) = show b ++ " ^ " ++ show f
--
-- instance Show Base where
--     show (BaseSingle x) = show x
--     show (Base e) = "(" ++ show e ++ ")"
--
-- instance Show FuncExpr where
--     show (FuncExpr name arg) = (init $ tail (show name)) ++ "(" ++ (show arg) ++ ")"
--
-- instance Show Expr where
--     show (ExprR r) = show r
--     show (ExprI i) = show i
--     show (ExprM m) = show m
--     show (ExprV v) = init $ tail (show v)

