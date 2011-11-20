module Common where

-- |Integer multiply. Multiplies integer by a factor given in a
-- rational number.
iMult :: (Integral a) => Rational -> a -> a
iMult a b = round $ toRational b * a