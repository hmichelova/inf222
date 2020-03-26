module DSLSortingSolution where

-- | Use the DSL runtime tools.
import DSLRuntime

-- | Use the DSL signature tools.
import DSLDefinitions

-- | Use the DIPL abstract syntax.
import DIPLDefinitions

-- | Use the DIPL state data structure.
import DIPLState

import Data.Array

dslsorting = (types, funcs, procs) :: DSLsignature
  where
    types = ["Boolean", "Element", "Array", "Index"]
    funcs = 
      [ ("le", ["Element", "Element"], "Boolean") 
      , ("get", ["Array", "Index"], "Element")
      , ("leindex", ["Index", "Index"], "Boolean")
      , ("dec", ["Index"], "Index")
      , ("inc", ["Index"], "Index")
      , ("startindex", ["Array"], "Index")
      , ("endindex", ["Array"], "Index")
      ]
    procs = [ ("swap", [(Inout, "Array"), (In, "Index"), (In, "Index")]) ]

data DSLstringvalue
  = DSLstringBV Bool
  | DSLstringEV String
  | DSLstringAV (Array Int String)
  | DSLstringIV Int
  deriving (Eq, Read, Show)

instance ValueType DSLstringvalue where
  truthvalue (DSLstringBV b) = b
  truthvalue (DSLstringEV s) = s == ""
  truthvalue (DSLstringAV a) = let (i, j) = bounds a in i <= j
  truthvalue (DSLstringIV i) = 0 < i

dslstringdefault :: Type -> String
dslstringdefault "Boolean" = "True"
dslstringdefault "Element" = "\"\""
dslstringdefault "Array" = "[]"
dslstringdefault "Index" = "0"

readdslstringvalue :: Type -> String -> DSLstringvalue
readdslstringvalue "Boolean" str = DSLstringBV $ read str
readdslstringvalue "Element" str = DSLstringEV $ read str
readdslstringvalue "Array" str = DSLstringAV $ listArray (1, length list) list
  where 
    list :: [String]
    list = read str
readdslstringvalue "Index" str = DSLstringIV $ read str
readdslstringvalue tname str = error $ "Trying to read value of unknown type " ++ tname ++ " from string " ++ str

showdslstringvalue :: Type -> DSLstringvalue -> String
showdslstringvalue "Boolean" (DSLstringBV b) = show b
showdslstringvalue "Element" (DSLstringEV e) = show e
showdslstringvalue "Array" (DSLstringAV a) = show $ elems a
showdslstringvalue "Index" (DSLstringIV i) = show i
showdslstringvalue tname val = error $ "Trying to show value of unknown type " ++ tname ++ " from value " ++ (show val)

dslstringfun :: String -> [DSLstringvalue] -> DSLstringvalue
dslstringfun "le" [DSLstringEV e1, DSLstringEV e2] = DSLstringBV $ e1 <= e2
dslstringfun "get" [DSLstringAV a, DSLstringIV i] = DSLstringEV $ a ! i
dslstringfun "leindex" [DSLstringIV i1, DSLstringIV i2] = DSLstringBV $ i1 <= i2
dslstringfun "dec" [DSLstringIV i] = DSLstringIV $ i - 1
dslstringfun "inc" [DSLstringIV i] = DSLstringIV $ i + 1
dslstringfun "startindex" [DSLstringAV a] = DSLstringIV $ fst $ bounds a
dslstringfun "endindex" [DSLstringAV a] = DSLstringIV $ snd $ bounds a
dslstringfun str args = error $ "Function " ++ str ++ show args ++ " not defined in DSL sorting."

dslstringprc :: String -> [ProcArg DSLstringvalue] -> State DSLstringvalue -> State DSLstringvalue
dslstringprc "swap" [VarArg array, VarArg i, VarArg j] state = 
  changevariable state array (DSLstringAV arrayNew)
  where 
    DSLstringAV arrayOld = getvalue state array
    DSLstringIV ii = getvalue state i
    DSLstringIV jj = getvalue state j
    arrayNew = arrayOld // [(ii, arrayOld ! jj), (jj, arrayOld ! ii)]
dslstringprc str args _ = error $ "Procedure " ++ str ++ show args ++ " not defined in DSL sorting."

rundslsortstring :: FilePath -> String -> IO ()
rundslsortstring filename input = do
  generalruninterpreter dslsorting dslstringprc dslstringfun dslstringdefault readdslstringvalue showdslstringvalue (startstate::(State DSLstringvalue)) filename input

-- | Specialisation of the general program parser and validator.
rundslstringparse :: FilePath -> String -> IO ()
rundslstringparse filename input = do
  generalrunparse dslsorting dslstringdefault filename input

------------------------------------ INTEGER -----------------------------------
data DSLintegervalue
  = DSLintegerBV Bool
  | DSLintegerEV Integer
  | DSLintegerAV (Array Int Integer)
  | DSLintegerIV Int
  deriving (Eq, Read, Show)

instance ValueType DSLintegervalue where
  truthvalue (DSLintegerBV b) = b
  truthvalue (DSLintegerEV s) = s == 0
  truthvalue (DSLintegerAV a) = let (i, j) = bounds a in i <= j
  truthvalue (DSLintegerIV i) = 0 < i

dslintegerdefault :: Type -> String
dslintegerdefault "Boolean" = "True"
dslintegerdefault "Element" = "0"
dslintegerdefault "Array" = "[]"
dslintegerdefault "Index" = "0"

readdslintegervalue :: Type -> String -> DSLintegervalue
readdslintegervalue "Boolean" str = DSLintegerBV $ read str
readdslintegervalue "Element" str = DSLintegerEV $ read str
readdslintegervalue "Array" str = DSLintegerAV $ listArray (1, length list) list
  where 
    list :: [Integer]
    list = read str
readdslintegervalue "Index" str = DSLintegerIV $ read str
readdslintegervalue tname str = error $ "Trying to read value of unknown type " ++ tname ++ " from integer " ++ str

showdslintegervalue :: Type -> DSLintegervalue -> String
showdslintegervalue "Boolean" (DSLintegerBV b) = show b
showdslintegervalue "Element" (DSLintegerEV e) = show e
showdslintegervalue "Array" (DSLintegerAV a) = show $ elems a
showdslintegervalue "Index" (DSLintegerIV i) = show i
showdslintegervalue tname val = error $ "Trying to show value of unknown type " ++ tname ++ " from value " ++ (show val)

dslintegerfun :: String -> [DSLintegervalue] -> DSLintegervalue
dslintegerfun "le" [DSLintegerEV e1, DSLintegerEV e2] = DSLintegerBV $ e1 <= e2
dslintegerfun "get" [DSLintegerAV a, DSLintegerIV i] = DSLintegerEV $ a ! i
dslintegerfun "leindex" [DSLintegerIV i1, DSLintegerIV i2] = DSLintegerBV $ i1 <= i2
dslintegerfun "dec" [DSLintegerIV i] = DSLintegerIV $ i - 1
dslintegerfun "inc" [DSLintegerIV i] = DSLintegerIV $ i + 1
dslintegerfun "startindex" [DSLintegerAV a] = DSLintegerIV $ fst $ bounds a
dslintegerfun "endindex" [DSLintegerAV a] = DSLintegerIV $ snd $ bounds a
dslintegerfun str args = error $ "Function " ++ str ++ show args ++ " not defined in DSL sorting."

dslintegerprc :: String -> [ProcArg DSLintegervalue] -> State DSLintegervalue -> State DSLintegervalue
dslintegerprc "swap" [VarArg array, VarArg i, VarArg j] state = 
  changevariable state array (DSLintegerAV arrayNew)
  where 
    DSLintegerAV arrayOld = getvalue state array
    DSLintegerIV ii = getvalue state i
    DSLintegerIV jj = getvalue state j
    arrayNew = arrayOld // [(ii, arrayOld ! jj), (jj, arrayOld ! ii)]
dslintegerprc str args _ = error $ "Procedure " ++ str ++ show args ++ " not defined in DSL sorting."

rundslsortinteger :: FilePath -> String -> IO ()
rundslsortinteger filename input = do
  generalruninterpreter dslsorting dslintegerprc dslintegerfun dslintegerdefault readdslintegervalue showdslintegervalue (startstate::(State DSLintegervalue)) filename input

-- | Specialisation of the general program parser and validator.
rundslintegerparse :: FilePath -> String -> IO ()
rundslintegerparse filename input = do
  generalrunparse dslsorting dslintegerdefault filename input
