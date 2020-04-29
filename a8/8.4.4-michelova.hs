{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck (Args (maxSuccess), quickCheckWithResult, stdArgs
                       , forAllProperties, Property, (=/=), (==>)
                       )
import Data.Int (Int16)

prop_no_zero_division :: Int16 -> Int16 -> Property
prop_no_zero_division a b = a /= 0 && b /= 0 ==> a * b /= 0

prop_no_zero_division_Integer :: Property
prop_no_zero_division_Integer = (-2944) * 23552 =/= 0

prop_no_zero_division_Int16 :: Property
prop_no_zero_division_Int16 = (-2944 :: Int16) * (23552 :: Int16) =/= 0

return []

runTests :: IO Bool
runTests = $forAllProperties (quickCheckWithResult stdArgs {maxSuccess = 1000})
