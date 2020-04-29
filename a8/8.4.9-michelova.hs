{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck (Args (maxSuccess), quickCheckWithResult, stdArgs
                       , forAllProperties, Property, (===)
                       )
import Data.Int (Int16, Int32)
import Data.Word (Word16)

f1 :: (Num a, Integral a) => a -> a -> a
f1 a b = a * b + a + b

f2 :: (Num a, Integral a) => a -> a -> a -> a
f2 a b c = a * b * c + a + b - c

f3 :: (Num a, Integral a) => a -> a -> a -> a
f3 a b c = a * a - b * c

prop_f1_word16ToInt16 :: Word16 -> Word16 -> Property
prop_f1_word16ToInt16 wa wb = f1 wa wb === fromIntegral (f1 ia ib)
  where
    ia :: Int16
    ia = fromIntegral wa
    ib :: Int16
    ib = fromIntegral wb

prop_f2_word16ToInt16 :: Word16 -> Word16 -> Word16 -> Property
prop_f2_word16ToInt16 wa wb wc = f2 wa wb wc === fromIntegral (f2 ia ib ic)
  where
    ia :: Int16
    ia = fromIntegral wa
    ib :: Int16
    ib = fromIntegral wb
    ic :: Int16
    ic = fromIntegral wc

prop_f3_word16ToInt16 :: Word16 -> Word16 -> Word16 -> Property
prop_f3_word16ToInt16 wa wb wc = f3 wa wb wc === fromIntegral (f3 ia ib ic)
  where
    ia :: Int16
    ia = fromIntegral wa
    ib :: Int16
    ib = fromIntegral wb
    ic :: Int16
    ic = fromIntegral wc

prop_f1_int16ToWord16 :: Int16 -> Int16 -> Property
prop_f1_int16ToWord16 ia ib = f1 ia ib === fromIntegral (f1 wa wb)
  where
    wa :: Word16
    wa = fromIntegral ia
    wb :: Word16
    wb = fromIntegral ib

prop_f2_int16ToWord16 :: Int16 -> Int16 -> Int16 -> Property
prop_f2_int16ToWord16 ia ib ic = f2 ia ib ic === fromIntegral (f2 wa wb wc)
  where
    wa :: Word16
    wa = fromIntegral ia
    wb :: Word16
    wb = fromIntegral ib
    wc :: Word16
    wc = fromIntegral ic

prop_f3_int16ToWord16 :: Int16 -> Int16 -> Int16 -> Property
prop_f3_int16ToWord16 ia ib ic = f3 ia ib ic === fromIntegral (f3 wa wb wc)
  where
    wa :: Word16
    wa = fromIntegral ia
    wb :: Word16
    wb = fromIntegral ib
    wc :: Word16
    wc = fromIntegral ic

prop_f1_int16ToInt32 :: Int16 -> Int16 -> Property
prop_f1_int16ToInt32 ia ib = f1 ia ib === fromIntegral (f1 i32a i32b)
  where
    i32a :: Int32
    i32a = fromIntegral ia
    i32b :: Int32
    i32b = fromIntegral ib

prop_f2_int16ToInt32 :: Int16 -> Int16 -> Int16 -> Property
prop_f2_int16ToInt32 ia ib ic = f2 ia ib ic === fromIntegral (f2 i32a i32b i32c)
  where
    i32a :: Int32
    i32a = fromIntegral ia
    i32b :: Int32
    i32b = fromIntegral ib
    i32c :: Int32
    i32c = fromIntegral ic

prop_f3_int16ToInt32 :: Int16 -> Int16 -> Int16 -> Property
prop_f3_int16ToInt32 ia ib ic = f3 ia ib ic === fromIntegral (f3 i32a i32b i32c)
  where
    i32a :: Int32
    i32a = fromIntegral ia
    i32b :: Int32
    i32b = fromIntegral ib
    i32c :: Int32
    i32c = fromIntegral ic

return []

runTests :: IO Bool
runTests = $forAllProperties (quickCheckWithResult stdArgs {maxSuccess = 1000})
