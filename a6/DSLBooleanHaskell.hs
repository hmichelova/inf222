-- | This module provides a minimal DSL for Boolean operations with a complete toolset.
-- • The DSL signature for Boolean: dslbool.
-- • Support for parsing DIPL code for the Boolean DSL: rundslboolparse.
-- • Support for reading parameters and showing results: readdslboolvalue showdslboolvalue.
-- • Value data type and its connection to type class ValueType (from DIPLState).
-- • Semantic functions defining the meaning of the DSL: dslboolfun dslboolprc.
-- • Support for interpreting DIPL code for the Boolean DSL: rundslboolinterpreter.
-- This provides a pattern for implementing other DSLs as well.
--
-- Author Magne Haveraaen
-- Since 2020-02-29



module DSLBooleanHaskell where


-- | Use the DSL runtime tools.
import DSLRuntime

-- | Use the DSL signature tools.
import DSLDefinitions

-- | Use the DIPL abstract syntax.
import DIPLDefinitions

-- | Use the DIPL state data structure.
import DIPLState

{-
-- | The static semantic checker is used indirectly via the general parse/interpreter in DSLRuntime.
import DIPLStaticSemantics

-- | Use Haskell's general tree library if you want to traverse an extended CST for the DIPL grammar
import Data.Tree (Tree(..), drawTree)
-}


--------------------------

-- | Complete mini-DSL for Boolean operations.
-- It includes the DSLsignature, the value type DSLboolvalue with read readdslboolvalue and show showdslboolvalue operations,
-- semantic functions dslboolfun for DSL functions and semantic functions dslboolpro for DSL procedures.
-- Finally the general parser and interpreter are instantiated to rundslboolparse and rundslboolinterpreter,
-- allowing easy access to interpret DIPL source code with doman specific extensions for Boolean.

-- | Define a boolean DSL signature.
dslbool = (types,funcs,procs) :: DSLsignature
  where
    types = ["Boolean"]
    funcs = [
      ("and",["Boolean","Boolean"],"Boolean"),
      ("not",["Boolean"],"Boolean"),
      ("true",[],"Boolean"),
      ("false",[],"Boolean")
      ]
    procs = [
      -- Swapping two arguments
      ("swap",[(Inout,"Boolean"),(Inout,"Boolean")]),
      -- Accumulate and: like "b &= c" in C-like languages.
      ("accumulateAnd",[(Inout,"Boolean"),(In,"Boolean")])
      ]

-- | Using Haskell's Bool as the value space for the dslbool signature.
data DSLboolvalue
  = DSLboolBV Bool
  deriving (Eq, Read, Show)

-- | Defining ValueType's type class operations - integrates this DSL with the interpreter.
dslboolvaluetruth :: DSLboolvalue -> Bool
dslboolvaluetruth (DSLboolBV b) = b
-- dslboolvaluetruth arg = error $ "Expects truth value, got " ++ (show arg)
instance ValueType DSLboolvalue where
  truthvalue v = dslboolvaluetruth v

-- | Default values for out parameter variables of each type.
dslbooldefault :: Type -> String
dslbooldefault "Boolean" = "True"

-- | Reads a value of the given type into a DSLboolvalue.
-- Uses the Haskell built in read function for the Haskell built in types.
readdslboolvalue :: Type -> String -> DSLboolvalue
readdslboolvalue "Boolean" str = DSLboolBV (read str)
readdslboolvalue tname str = error $ "Trying to read value of unknown type " ++ tname ++ " from string " ++ str

-- | Shows a value of the given type of DSLboolvalue.
-- Uses the Haskell built in show function for Haskell types.
showdslboolvalue :: Type -> DSLboolvalue -> String
showdslboolvalue "Boolean" (DSLboolBV b) = (show b)
showdslboolvalue tname val = error $ "Trying to show value of unknown type " ++ tname ++ " from value " ++ (show val)

-- | Semantics of the dslbool DSL: Haskell semantics of boolean functions.
dslboolfun :: String -> [DSLboolvalue] -> DSLboolvalue
dslboolfun "and" [DSLboolBV b1, DSLboolBV b2] = DSLboolBV (b1 && b2)
dslboolfun "not" [DSLboolBV b1] = DSLboolBV (not b1)
dslboolfun "true" [] = DSLboolBV (True)
dslboolfun "false" [] = DSLboolBV (False)
dslboolfun str args = error $ "Function " ++ str ++ (show args) ++ " not defined in DSL bool."

-- | Semantics of the dslbool DSL: Haskell semantics of boolean procedures.
dslboolprc :: String -> [ProcArg DSLboolvalue] -> State DSLboolvalue -> State DSLboolvalue
dslboolprc "swap" [VarArg var1, VarArg var2] state = changevariable (changevariable state var1 tmp2) var2 tmp1
  where
    tmp1 = getvalue state var1
    tmp2 = getvalue state var2
-- The second argument has mode "in", but the parser may provide both a VarArg and a ValArg argument
dslboolprc "accumulateAnd" [VarArg var1, VarArg var2] state = changevariable state var1 (DSLboolBV (tmp1 && tmp2))
  where
    DSLboolBV tmp1 = getvalue state var1
    DSLboolBV tmp2 = getvalue state var2
dslboolprc "accumulateAnd" [VarArg var1, ValArg val2] state = changevariable state var1 (DSLboolBV (tmp1 && tmp2))
  where
    DSLboolBV tmp1 = getvalue state var1
    DSLboolBV tmp2 = val2
dslboolprc str args _ = error $ "Procedure " ++ str ++ (show args) ++ " not defined in DSL bool."


-- | Run the specialisation of the general interpreter.
{-
   rundslboolinterpreter "DSLBoolean-and.dipl" "True True"
   rundslboolinterpreter "DSLBoolean-swap.dipl" "True False"
-}
rundslboolinterpreter filename input = do
  generalruninterpreter dslbool dslboolprc dslboolfun dslbooldefault readdslboolvalue showdslboolvalue (startstate::(State DSLboolvalue)) filename input

-- | Specialisation of the general program parser and validator.
{-
   rundslboolparse "DSLBoolean-and.dipl" "True True"
   rundslboolparse "DSLBoolean-swap.dipl" "True False"
-}
rundslboolparse filename input = do
  generalrunparse dslbool dslbooldefault filename input




