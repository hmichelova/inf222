{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck (Args (maxSuccess), quickCheckWithResult, stdArgs
                       , forAllProperties, Property, (.||.), property
                       )
import Data.Int (Int16)

prop_antisymmetry :: Int16 -> Int16 -> Property
prop_antisymmetry a b = property $ (a > b || b > a) || a == b

prop_transitivity :: Int16 -> Int16 -> Int16 -> Property
prop_transitivity a b c = property $ (a > b || b > c) || a <= c

prop_connexity :: Int16 -> Int16 -> Property
prop_connexity a b = a <= b .||. b <= a

return []

runTests :: IO Bool
runTests = $forAllProperties (quickCheckWithResult stdArgs {maxSuccess = 1000})
