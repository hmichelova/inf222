module DSLIntegerSolution where

-- | Use the DSL runtime tools.
import DSLRuntime

-- | Use the DSL signature tools.
import DSLDefinitions

-- | Use the DIPL abstract syntax.
import DIPLDefinitions

-- | Use the DIPL state data structure.
import DIPLState

import Data.Word

dslintegers = (types, funcs, procs) :: DSLsignature
  where
    types = ["Boolean", "Integer"]
    funcs = 
      [ ("plus", ["Integer", "Integer"], "Integer") 
      , ("minus", ["Integer", "Integer"], "Integer")
      , ("mult", ["Integer", "Integer"], "Integer")
      , ("idiv", ["Integer", "Integer"], "Integer")
      , ("rem", ["Integer", "Integer"], "Integer")
      , ("gcd", ["Integer", "Integer"], "Integer")
      , ("lcm", ["Integer", "Integer"], "Integer")
      , ("fact", ["Integer", "Integer"], "Integer")
      , ("sum3", ["Integer", "Integer", "Integer"], "Integer")
      , ("no0", [], "Integer")
      , ("no1", [], "Integer")
      , ("no2", [], "Integer")
      , ("no3", [], "Integer")
      , ("no4", [], "Integer")
      , ("no5", [], "Integer")
      , ("no6", [], "Integer")
      , ("no7", [], "Integer")
      , ("no8", [], "Integer")
      , ("no9", [], "Integer")
      , ("no19", [], "Integer")
      , ("no100", [], "Integer")
      , ("no25", [], "Integer")
      , ("no15", [], "Integer")
      , ("no30", [], "Integer")
      , ("no32", [], "Integer")
      , ("no11", [], "Integer")
      , ("no22", [], "Integer")
      , ("no451", [], "Integer")
      , ("no31", [], "Integer")
      , ("no114", [], "Integer")
      , ("le", ["Integer", "Integer"], "Boolean")
      , ("l", ["Integer", "Integer"], "Boolean")
      , ("ge", ["Integer", "Integer"], "Boolean")
      , ("g", ["Integer", "Integer"], "Boolean")
      , ("eq", ["Integer", "Integer"], "Boolean")
      , ("and", ["Boolean", "Boolean"], "Boolean")
      , ("or", ["Boolean", "Boolean"], "Boolean")
      , ("xor", ["Boolean", "Boolean"], "Boolean")
      , ("not", ["Boolean"], "Boolean")
      , ("true", [], "Boolean")
      , ("false", [], "Boolean")
      ]
    procs = []

-- | Using Haskell's Bool and Integer as the value space for the dslinteger signature.
data DSLintegervalue
  = DSLintegerBV Bool
  | DSLintegerIV Integer
  deriving (Eq, Read, Show)

-- | Defining ValueType's type class operations - integrates this DSL with the interpreter.
instance ValueType DSLintegervalue where
  truthvalue (DSLintegerBV b) = b
  truthvalue (DSLintegerIV i) = 0 < i

-- | Default values for out parameter variables of each type.
dslintegerdefault :: Type -> String
dslintegerdefault "Boolean" = "True"
dslintegerdefault "Integer" = "0"

-- | Reads a value of the given type into a DSLintegervalue.
-- Uses the Haskell built in read function for the Haskell built in types.
readdslintegervalue :: Type -> String -> DSLintegervalue
readdslintegervalue "Boolean" str = DSLintegerBV $ read str
readdslintegervalue "Integer" str = DSLintegerIV $ read str
readdslintegervalue tname str = error $ "Trying to read value of unknown type " ++ tname ++ " from string " ++ str

-- | Shows a value of the given type of DSLintegervalue.
-- Uses the Haskell built in show function for Haskell types.
showdslintegervalue :: Type -> DSLintegervalue -> String
showdslintegervalue "Boolean" (DSLintegerBV b) = show b
showdslintegervalue "Integer" (DSLintegerIV i) = show i
showdslintegervalue tname val = error $ "Trying to show value of unknown type " ++ tname ++ " from value " ++ (show val)

-- | Semantics of the dslinteger DSL: Haskell semantics of integer functions.
dslintegerfun :: String -> [DSLintegervalue] -> DSLintegervalue
dslintegerfun "plus" [DSLintegerIV i1, DSLintegerIV i2] = DSLintegerIV $ i1 + i2
dslintegerfun "minus" [DSLintegerIV i1, DSLintegerIV i2] = DSLintegerIV $ i1 - i2
dslintegerfun "mult" [DSLintegerIV i1, DSLintegerIV i2] = DSLintegerIV $ i1 * i2
dslintegerfun "idiv" [DSLintegerIV i1, DSLintegerIV i2] = DSLintegerIV $ div i1 i2
dslintegerfun "rem" [DSLintegerIV i1, DSLintegerIV i2] = DSLintegerIV $ rem i1 i2
dslintegerfun "gcd" [DSLintegerIV i1, DSLintegerIV i2] = DSLintegerIV $ gcd i1 i2
dslintegerfun "lcm" [DSLintegerIV i1, DSLintegerIV i2] = DSLintegerIV $ lcm i1 i2
dslintegerfun "fact" [DSLintegerIV i] = DSLintegerIV $ fact i
  where
    fact i = if i <= 0 then 1 else i * fact (i - 1)
dslintegerfun "sum3" [DSLintegerIV i1, DSLintegerIV i2, DSLintegerIV i3] = 
  DSLintegerIV $ i1 + i2 + i3
dslintegerfun "no0" [] = DSLintegerIV $ 0
dslintegerfun "no1" [] = DSLintegerIV $ 1
dslintegerfun "no2" [] = DSLintegerIV $ 2
dslintegerfun "no3" [] = DSLintegerIV $ 3
dslintegerfun "no4" [] = DSLintegerIV $ 4
dslintegerfun "no5" [] = DSLintegerIV $ 5
dslintegerfun "no6" [] = DSLintegerIV $ 6
dslintegerfun "no7" [] = DSLintegerIV $ 7
dslintegerfun "no8" [] = DSLintegerIV $ 8
dslintegerfun "no9" [] = DSLintegerIV $ 9
dslintegerfun "no19" [] = DSLintegerIV $ 19
dslintegerfun "no100" [] = DSLintegerIV $ 100
dslintegerfun "no25" [] = DSLintegerIV $ 25
dslintegerfun "no15" [] = DSLintegerIV $ 15
dslintegerfun "no30" [] = DSLintegerIV $ 30
dslintegerfun "no32" [] = DSLintegerIV $ 32
dslintegerfun "no11" [] = DSLintegerIV $ 11
dslintegerfun "no22" [] = DSLintegerIV $ 22
dslintegerfun "no451" [] = DSLintegerIV $ 451
dslintegerfun "no31" [] = DSLintegerIV $ 31
dslintegerfun "no114" [] = DSLintegerIV $ 114
dslintegerfun "le" [DSLintegerIV i1, DSLintegerIV i2] = DSLintegerBV $ i1 <= i2
dslintegerfun "l" [DSLintegerIV i1, DSLintegerIV i2] = DSLintegerBV $ i1 < i2
dslintegerfun "ge" [DSLintegerIV i1, DSLintegerIV i2] = DSLintegerBV $ i1 >= i2
dslintegerfun "g" [DSLintegerIV i1, DSLintegerIV i2] = DSLintegerBV $ i1 > i2
dslintegerfun "eq" [DSLintegerIV i1, DSLintegerIV i2] = DSLintegerBV $ i1 == i2
dslintegerfun "and" [DSLintegerBV b1, DSLintegerBV b2] = DSLintegerBV $ b1 && b2
dslintegerfun "or" [DSLintegerBV b1, DSLintegerBV b2] = DSLintegerBV $ b1 || b2
dslintegerfun "xor" [DSLintegerBV b1, DSLintegerBV b2] = 
  DSLintegerBV $ (b1 && not b2) || (not b1 && b2)
dslintegerfun "not" [DSLintegerBV b1] = DSLintegerBV $ not b1
dslintegerfun "true" [] = DSLintegerBV True
dslintegerfun "false" [] = DSLintegerBV False
dslintegerfun str args = error $ "Function " ++ str ++ show args ++ " not defined in DSL integer."

-- | Semantics of the dslinteger DSL: Haskell semantics of integer procedures.
dslintegerprc :: String -> [ProcArg DSLintegervalue] -> State DSLintegervalue -> State DSLintegervalue
dslintegerprc str args _ = error $ "Procedure " ++ str ++ (show args) ++ " not defined in DSL integer."

-- | Run the specialisation of the general interpreter.
rundslintegerhaskell :: FilePath -> String -> IO ()
rundslintegerhaskell filename input = do
  generalruninterpreter dslintegers dslintegerprc dslintegerfun dslintegerdefault readdslintegervalue showdslintegervalue (startstate::(State DSLintegervalue)) filename input

-- | Specialisation of the general program parser and validator.
rundslintegerparse :: FilePath -> String -> IO ()
rundslintegerparse filename input = do
  generalrunparse dslintegers dslintegerdefault filename input


------------------------------------ WORD16 -----------------------------------
-- | Using Haskell's Bool and Word16 as the value space for the dslintegers signature.
data DSLwordvalue
  = DSLwordBV Bool
  | DSLwordIV Word16
  deriving (Eq, Read, Show)

-- | Defining ValueType's type class operations - integrates this DSL with the interpreter.
instance ValueType DSLwordvalue where
  truthvalue (DSLwordBV b) = b
  truthvalue (DSLwordIV i) = 0 < i

-- | Default values for out parameter variables of each type.
dslworddefault :: Type -> String
dslworddefault "Boolean" = "True"
dslworddefault "Integer" = "0"

-- | Reads a value of the given type into a DSLwordvalue.
-- Uses the Haskell built in read function for the Haskell built in types.
readdslwordvalue :: Type -> String -> DSLwordvalue
readdslwordvalue "Boolean" str = DSLwordBV $ read str
readdslwordvalue "Integer" str = DSLwordIV $ read str
readdslwordvalue tname str = error $ "Trying to read value of unknown type " ++ tname ++ " from string " ++ str

-- | Shows a value of the given type of DSLwordvalue.
-- Uses the Haskell built in show function for Haskell types.
showdslwordvalue :: Type -> DSLwordvalue -> String
showdslwordvalue "Boolean" (DSLwordBV b) = show b
showdslwordvalue "Integer" (DSLwordIV i) = show i
showdslwordvalue tname val = error $ "Trying to show value of unknown type " ++ tname ++ " from value " ++ (show val)

-- | Semantics of the dslword DSL: Haskell semantics of integers functions.
dslwordfun :: String -> [DSLwordvalue] -> DSLwordvalue
dslwordfun "plus" [DSLwordIV i1, DSLwordIV i2] = DSLwordIV $ i1 + i2
dslwordfun "minus" [DSLwordIV i1, DSLwordIV i2] = DSLwordIV $ i1 - i2
dslwordfun "mult" [DSLwordIV i1, DSLwordIV i2] = DSLwordIV $ i1 * i2
dslwordfun "idiv" [DSLwordIV i1, DSLwordIV i2] = DSLwordIV $ div i1 i2
dslwordfun "rem" [DSLwordIV i1, DSLwordIV i2] = DSLwordIV $ rem i1 i2
dslwordfun "gcd" [DSLwordIV i1, DSLwordIV i2] = DSLwordIV $ gcd i1 i2
dslwordfun "lcm" [DSLwordIV i1, DSLwordIV i2] = DSLwordIV $ lcm i1 i2
dslwordfun "fact" [DSLwordIV i] = DSLwordIV $ fact i
  where
    fact i = if i <= 0 then 1 else i * fact (i - 1)
dslwordfun "sum3" [DSLwordIV i1, DSLwordIV i2, DSLwordIV i3] = 
  DSLwordIV $ i1 + i2 + i3
dslwordfun "no0" [] = DSLwordIV $ 0
dslwordfun "no1" [] = DSLwordIV $ 1
dslwordfun "no2" [] = DSLwordIV $ 2
dslwordfun "no3" [] = DSLwordIV $ 3
dslwordfun "no4" [] = DSLwordIV $ 4
dslwordfun "no5" [] = DSLwordIV $ 5
dslwordfun "no6" [] = DSLwordIV $ 6
dslwordfun "no7" [] = DSLwordIV $ 7
dslwordfun "no8" [] = DSLwordIV $ 8
dslwordfun "no9" [] = DSLwordIV $ 9
dslwordfun "no19" [] = DSLwordIV $ 19
dslwordfun "no100" [] = DSLwordIV $ 100
dslwordfun "no25" [] = DSLwordIV $ 25
dslwordfun "no15" [] = DSLwordIV $ 15
dslwordfun "no30" [] = DSLwordIV $ 30
dslwordfun "no32" [] = DSLwordIV $ 32
dslwordfun "no11" [] = DSLwordIV $ 11
dslwordfun "no22" [] = DSLwordIV $ 22
dslwordfun "no451" [] = DSLwordIV $ 451
dslwordfun "no31" [] = DSLwordIV $ 31
dslwordfun "no114" [] = DSLwordIV $ 114
dslwordfun "le" [DSLwordIV i1, DSLwordIV i2] = DSLwordBV $ i1 <= i2
dslwordfun "l" [DSLwordIV i1, DSLwordIV i2] = DSLwordBV $ i1 < i2
dslwordfun "ge" [DSLwordIV i1, DSLwordIV i2] = DSLwordBV $ i1 >= i2
dslwordfun "g" [DSLwordIV i1, DSLwordIV i2] = DSLwordBV $ i1 > i2
dslwordfun "eq" [DSLwordIV i1, DSLwordIV i2] = DSLwordBV $ i1 == i2
dslwordfun "and" [DSLwordBV b1, DSLwordBV b2] = DSLwordBV $ b1 && b2
dslwordfun "or" [DSLwordBV b1, DSLwordBV b2] = DSLwordBV $ b1 || b2
dslwordfun "xor" [DSLwordBV b1, DSLwordBV b2] = 
  DSLwordBV $ (b1 && not b2) || (not b1 && b2)
dslwordfun "not" [DSLwordBV b1] = DSLwordBV $ not b1
dslwordfun "true" [] = DSLwordBV True
dslwordfun "false" [] = DSLwordBV False
dslwordfun str args = error $ "Function " ++ str ++ show args ++ " not defined in DSL word."

-- | Semantics of the dslword DSL: Haskell semantics of integers procedures.
dslwordprc :: String -> [ProcArg DSLwordvalue] -> State DSLwordvalue -> State DSLwordvalue
dslwordprc str args _ = error $ "Procedure " ++ str ++ (show args) ++ " not defined in DSLintegers."

-- | Run the specialisation of the general interpreter.
rundslintegeruint16 :: FilePath -> String -> IO ()
rundslintegeruint16 filename input = do
  generalruninterpreter dslintegers dslwordprc dslwordfun dslworddefault readdslwordvalue showdslwordvalue (startstate::(State DSLwordvalue)) filename input

-- | Specialisation of the general program parser and validator.
rundslwordparse :: FilePath -> String -> IO ()
rundslwordparse filename input = do
  generalrunparse dslintegers dslworddefault filename input

