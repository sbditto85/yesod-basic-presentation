module Denominator
  ( Denominator
  , newDenominator
  , safeDivide
  ) where

import Import.NoFoundation

newtype Denominator = Denominator Int

newDenominator :: Int -> Maybe Denominator
newDenominator i = if i == 0 then Nothing else Just (Denominator i)

safeDivide :: Int -> Denominator -> Int
safeDivide numerator (Denominator denominator) = numerator `div` denominator
