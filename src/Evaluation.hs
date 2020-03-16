module Evaluation where

import Expr
import qualified Assignment as A

eval :: A.Context -> Expr -> Maybe Atom
eval c (Variable n) = do (A.Variable _ e) <- A.get c n
                         eval c e
eval c (Function n e) = do x <- eval c e
                           (A.Function _ param fe) <- A.get c n
                           let tmp = A.update c (A.Variable param (EAtom x))
                           eval tmp fe
eval c (EAtom a) = Just a
eval c (Add e1 e2) = evalInfix c e1 e2 (+?)
eval c (Sub e1 e2) = evalInfix c e1 e2 (-?)
eval c (Mul e1 e2) = evalInfix c e1 e2 (*?)
eval c (Div e1 e2) = evalInfix c e1 e2 (/?)
eval c (Mod e1 e2) = evalInfix c e1 e2 (%?)
eval c (Exp e1 e2) = evalInfix c e1 e2 (^?)
eval c (Dot e1 e2) = evalInfix c e1 e2 (**?)
-- eval _ _ = Nothing

evalInfix :: A.Context -> Expr -> Expr -> (Atom -> Atom -> Maybe Atom) -> Maybe Atom
evalInfix c e1 e2 f = do a <- eval c e1
                         b <- eval c e2
                         f a b

infixl 6 +?
(+?) :: Atom -> Atom -> Maybe Atom
(Rational a) +? (Rational b) = Just $ Rational (a + b)
(Imaginary a) +? (Imaginary b) = Just $ Imaginary (a + b)
_ +? _ = Nothing

infixl 6 -?
(-?) :: Atom -> Atom -> Maybe Atom
(Rational a) -? (Rational b) = Just $ Rational (a - b)
(Imaginary a) -? (Imaginary b) = Just $ Imaginary (a - b)
_ -? _ = Nothing

infixl 7 *?
(*?) :: Atom -> Atom -> Maybe Atom
(Rational a) *? (Rational b) = Just $ Rational (a * b)
(Rational a) *? (Imaginary b) = Just $ Imaginary (a * b)
(Imaginary a) *? (Imaginary b) = (Imaginary (a * b)) ^? Rational 2
_ *? _ = Nothing

infixl 7 /?
(/?) :: Atom -> Atom -> Maybe Atom
_ /? (Rational 0) = Nothing
(Rational a) /? (Rational b) = Just $ Rational (a / b)
_ /? _ = Nothing

infixl 7 %?
(%?) :: Atom -> Atom -> Maybe Atom
_ %? _ = Nothing

infixr 8 ^?
(^?) :: Atom -> Atom -> Maybe Atom
(Rational a) ^? (Rational b) = Just $ Rational (a ** b)
(Imaginary a) ^? (Rational 0) = Just $ Rational a
(Imaginary a) ^? (Rational 1) = Just $ Imaginary a
(Imaginary a) ^? (Rational 2) = Just $ Rational (-a)
(Imaginary a) ^? (Rational 3) = Just $ Imaginary (-a)
(Imaginary a) ^? (Rational b) = Imaginary a ^? (Rational (b - 4))
_ ^? _ = Nothing

infixr 8 **?
(**?) :: Atom -> Atom -> Maybe Atom
_ **? _ = Nothing
