{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck (Args (maxSuccess), quickCheckWithResult, stdArgs
                       , forAllProperties, Property, (==>)
                       )
import Data.Int (Int16)

prop_addiction :: Int16 -> Int16 -> Int16 -> Property
prop_addiction a b c = a <= b ==> a + c <= b + c

prop_zero_compare :: Int16 -> Int16 -> Property
prop_zero_compare a b = 0 <= a && 0 <= b ==> 0 <= a * b

prop_addiction_integer :: Integer -> Integer -> Integer -> Property
prop_addiction_integer a b c = a <= b ==> a + c <= b + c

prop_zero_compare_integer :: Integer -> Integer -> Property
prop_zero_compare_integer a b = 0 <= a && 0 <= b ==> 0 <= a * b

return []

runTests :: IO Bool
runTests = $forAllProperties (quickCheckWithResult stdArgs {maxSuccess = 1000})
