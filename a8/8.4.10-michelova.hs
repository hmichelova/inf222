{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck (Args (maxSuccess), quickCheckWithResult, stdArgs
                       , forAllProperties, Property, (===), (==>)
                       )
import Data.Word (Word16)

f1 :: (Num a, Integral a) => a -> a -> a -> a
f1 a b x = mod (a * b + a + b) x

f2 :: (Num a, Integral a) => a -> a -> a -> a -> a
f2 a b c x = mod (a * b * c + a + b - c) x

f3 :: (Num a, Integral a) => a -> a -> a -> a -> a
f3 a b c x = mod (a * a - b * c) x

f1' :: (Num a, Integral a) => a -> a -> a -> a
f1' a b x = mod (mod (mod (a * b) x + a) x + b) x

f2' :: (Num a, Integral a) => a -> a -> a -> a -> a
f2' a b c x = mod (mod (mod (mod (a * b * c) x + a) x + b) x - c) x

f3' :: (Num a, Integral a) => a -> a -> a -> a -> a
f3' a b c x = mod (mod (a * a) x - mod (b * c) x) x

prop_f1 :: Word16 -> Word16 -> Word16 -> Property
prop_f1 a b x = 0 < x && x < 256 ==> f1 a b x == f1' a b x

prop_f2 :: Word16 -> Word16 -> Word16 -> Word16 -> Property
prop_f2 a b c x = 0 < x && x < 256 ==> f2 a b c x == f2' a b c x

prop_f3 :: Word16 -> Word16 -> Word16 -> Word16 -> Property
prop_f3 a b c x = 0 < x && x < 256 ==> f3 a b c x == f3' a b c x

prop_f1_specX :: Word16 -> Word16 -> Property
prop_f1_specX a b = a < x && b < x ==> f1 a b x == f1' a b x
  where
    x = 251

prop_f2_specX :: Word16 -> Word16 -> Word16 -> Property
prop_f2_specX a b c = a < x && b < x && c < x ==> f2 a b c x == f2' a b c x
  where
    x = 251

prop_f3_specX :: Word16 -> Word16 -> Word16 -> Property
prop_f3_specX a b c = a < x && b < x && c < x ==> f3 a b c x == f3' a b c x
  where
    x = 251

return []

runTests :: IO Bool
runTests = $forAllProperties (quickCheckWithResult stdArgs {maxSuccess = 1000})
