module Atom where


data Atom
    = ARational Float
    | AImaginary Float

infixl 6 +?
(+?) :: Atom -> Atom -> Maybe Atom
(ARational a) +? (ARational b) = Just $ ARational (a + b)
(AImaginary a) +? (AImaginary b) = Just $ AImaginary (a + b)
_ +? _ = Nothing

infixl 6 -?
(-?) :: Atom -> Atom -> Maybe Atom
(ARational a) -? (ARational b) = Just $ ARational (a - b)
(AImaginary a) -? (AImaginary b) = Just $ AImaginary (a - b)
_ -? _ = Nothing

infixl 7 *?
(*?) :: Atom -> Atom -> Maybe Atom
(ARational a) *? (ARational b) = Just $ ARational (a * b)
(ARational a) *? (AImaginary b) = Just $ AImaginary (a * b)
(AImaginary a) *? (AImaginary b) = (AImaginary (a * b)) ^? ARational 2
_ *? _ = Nothing

infixl 7 /?
(/?) :: Atom -> Atom -> Maybe Atom
_ /? (ARational 0) = Nothing
(ARational a) /? (ARational b) = Just $ ARational (a / b)
_ /? _ = Nothing

infixl 7 %?
(%?) :: Atom -> Atom -> Maybe Atom
_ %? _ = Nothing

infixr 8 ^?
(^?) :: Atom -> Atom -> Maybe Atom
(ARational a) ^? (ARational b) = Just $ ARational (a ** b)
(AImaginary a) ^? (ARational 0) = Just $ ARational a
(AImaginary a) ^? (ARational 1) = Just $ AImaginary a
(AImaginary a) ^? (ARational 2) = Just $ ARational (-a)
(AImaginary a) ^? (ARational 3) = Just $ AImaginary (-a)
(AImaginary a) ^? (ARational b) = AImaginary a ^? (ARational (b - 4))
_ ^? _ = Nothing

instance Show Atom where
    show (ARational r) = show r
    show (AImaginary i) = show i ++ "i"
