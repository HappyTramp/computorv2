module Complex where

type Imaginary = Float

data Complex = Complex Float Imaginary

-- instance Num Complex where
--     (Complex r1 i1) + (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)
--     (Complex r1 i1) * (Complex r2 i2) = undefined
--     negate (Complex r1 i1) = undefined
--     abs (Complex r1 i1) = undefined
--     signum (Complex r1 i1) = undefined
    -- fromInteger r = Complex r 0

instance Show Complex where
    show (Complex r i) = show r ++ showI ++ "i"
        where showI = if i < 0 then " - " ++ show (-i) else " + " ++ show i
