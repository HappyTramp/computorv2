module Imag where

newtype Imag = Imag { getImag :: Float }

instance Show Imag where
    show (Imag i) = show i ++ "i"
