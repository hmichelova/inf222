{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck (Args (maxSuccess), quickCheckWithResult, stdArgs
                       , forAllProperties, Property, (===), (==>)
                       )
import Data.Int (Int16)

prop_plus_associativity :: Int16 -> Int16 -> Int16 -> Property
prop_plus_associativity a b c = a + (b + c) === (a + b) + c

prop_plus_commutativity :: Int16 -> Int16 -> Property
prop_plus_commutativity a b = a + b === b + a

prop_plus_identity :: Int16 -> Property
prop_plus_identity a = a + 0 === a

prop_plus_inverse :: Int16 -> Property
prop_plus_inverse a = a + (-a) === 0

prop_mul_associativity :: Int16 -> Int16 -> Int16 -> Property
prop_mul_associativity a b c = a * (b * c) === (a * b) * c

prop_mul_commutativity :: Int16 -> Int16 -> Property
prop_mul_commutativity a b = a * b === b * a

prop_mul_identity :: Int16 -> Property
prop_mul_identity a = a * 1 === a

prop_mul_zero_divisor :: Int16 -> Int16 -> Property
prop_mul_zero_divisor a b = a /= 0 && b /= 0 ==> a * b /= 0

prop_left_distributivity :: Int16 -> Int16 -> Int16 -> Property
prop_left_distributivity a b c = a * (b + c) === (a * b) + (a * c)

prop_right_distributivity :: Int16 -> Int16 -> Int16 -> Property
prop_right_distributivity a b c = (a + b) * c === (a * c) + (b * c)

return []

runTests :: IO Bool
runTests = $forAllProperties (quickCheckWithResult stdArgs {maxSuccess = 1000})
