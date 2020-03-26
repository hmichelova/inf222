-- | Definition of DSL abstract signatures.
-- These are given as a database of type, function and procedure declarations with consistency (wellformedness) requirements.
-- DSL stands for Domain Specific (programming) Language.
--
-- Author: Magne Haveraaen
-- Since: 2020-02-20

module DSLDefinitions where


--------------------------

-- | Signature declaration for a Domain Specific Language (DSL) using the extended signature language (ESL) abstract syntax.
-- The data structure is accompanied by wellformedness rules, function dslConsistent.

{- Abstract syntax for the declaratio of a DSL.
-- Declarations of types
  symbol dtype : String → TYPE
-- Declaration of functions
  symbol func : String × TYPE* × TYPE → FUNC
-- Declaration of procedures
  symbol proc : String × ((in|inout|out) TYPE)* → PROC
-}

-- | A DSL signature declaration (abstract syntax) has three components:
-- • A list of declared types
-- • A list of function declarations
-- • A list of procedure declarations
type DSLsignature = (TYPES, FUNCS, PROCS)

-- | A declared type is simply a string
type TYPE = String
-- | Declared types are stored in a list: the types database
type TYPES = [TYPE]

-- | Declaration of a function:
-- • A string representing the function name
-- • A list of parameter types for the function
-- • The return type of the function
type FUNC = (String,[TYPE],TYPE)
-- | Declaration of functions are stored in a list: the functions database
type FUNCS = [FUNC]

-- | Declaration of a procedure:
-- • A string representing the procedure name
-- • A list of parameter types for the procedure
type PROC = (String, [(Parametermode,TYPE)])
-- | Declaration of procedures are stored in a list: the procedures database
type PROCS = [PROC]

-- | Parameter transfer modes: input, input/output and output mode.
data Parametermode 
  = In
  | Inout
  | Out
  deriving (Eq, Show, Read)

-- | Argument data for procedure calls: values for the input mode, variables for input/output and output modes.
-- The value domain is decided for each DSL implementation.
data ProcArg value
  = ValArg value
  | VarArg String
  deriving (Eq, Show)


-------------------------

-- | Wellformed DSL signature requirements.

-- | Check that the syntax of a DSL is consistent.
dslConsistent :: DSLsignature -> Bool
-- Consistency of each of the components with repect to the typing.
dslConsistent dsl@(types,funcs,procs)
  = and [dslConsistenttype types, dslConsistentfunc types funcs, dslConsistentproc types procs]


-- | Every type should be declared only once, and there must be a type Boolean.
dslConsistenttype :: TYPES -> Bool
dslConsistenttype types = (distinctnames types) && (elem "Boolean" types)

-- | Every function should have a unique name and all parameter and result types must be in the type database.
dslConsistentfunc :: TYPES -> FUNCS -> Bool
dslConsistentfunc types (func:funcs)
  = -- Function names are unique and all types are known
    not (elem fname fnames)
      &&
    and (map (\t -> elem t types) (tresult:targs))
      &&
    dslConsistentfunc types funcs
    where
      (fname, targs, tresult) = func
      fnames = map (\(fname, targs, tresult) -> fname) funcs
dslConsistentfunc types [] = True

-- | Every procedure should have a unique name and all parameter types must be in the type database.
dslConsistentproc :: TYPES -> PROCS -> Bool
dslConsistentproc types (proc:procs)
  = -- Procedure names are unique and all types are known
    not (elem pname pnames)
      &&
    and (map (\t -> elem t types) targs)
      &&
    dslConsistentproc types procs
    where
      (pname, mtargs) = proc
      pnames = map (\(pname, mtargs) -> pname) procs
      targs = map (\(pmode, targ) -> targ) mtargs
dslConsistentproc types [] = True
-- dslConsistentproc _ _ = False


------------------------

-- | DSL data structure utility functions.

-- | Checking that a type str is declared in the DSL signature
findtypdecl :: String -> DSLsignature -> Bool
findtypdecl str (ts,fs,ps) = elem str ts
-- | Finding a function str declared in the DSL signature
findfundecl :: String -> DSLsignature -> FUNC
findfundecl str (ts,fs,ps) = findfunctiondecl str fs
-- | Finding a procedure str declared in the DSL signature
findprodecl :: String -> DSLsignature -> PROC
findprodecl str (ts,fs,ps) = findproceduredecl str ps


-- | Finding a specific function by name
findfunctiondecl :: String -> FUNCS -> FUNC
findfunctiondecl str ((fstr,ts,tr):funcs) = if str == fstr then (fstr,ts,tr) else findfunctiondecl str funcs
findfunctiondecl str [] = error $ "Function not found in DSL: " ++ str

-- | Finding a specific function by name
findproceduredecl :: String -> PROCS -> PROC
findproceduredecl str ((pstr,tps):procs) = if str == pstr then (pstr,tps) else findproceduredecl str procs
findproceduredecl str [] = error $ "Procedure not found in DSL: " ++ str


------------------------

-- | General utility functions.

-- | Checking that a list only contains distinct entries.
distinctnames :: Eq a => [a] -> Bool
distinctnames (str:strs) = not (elem str strs) && distinctnames strs
distinctnames [] = True


------------------------

-- | Unit tests for DSL support: declarations and wellformedness.

-- | Example declaring integer type and some operations.
exampledslsigastintegers = (types,funcs,procs)
  where
    types = ["Boolean","Integer"]
    funcs = [("_+_",["Integer","Integer"],"Integer"),("-_",["Integer"],"Integer"),("_<=_",["Integer","Integer"],"Boolean")]
    procs = []
-- | Example declaring integer type and some operations but forgetting to include the type Boolean.
exampledslsigastintegersnoboolean = (types,funcs,procs)
  where
    types = ["Integer"]
    funcs = [("_+_",["Integer","Integer"],"Integer"),("-_",["Integer"],"Integer")]
    procs = []
-- | Example declaring integer type and some operations, but with a misspelling of a type name Iteger.
exampledslsigastintegersspellerror = (types,funcs,procs)
  where
    types = ["Boolean","Integer"]
    funcs = [("_+_",["Integer","Integer"],"Integer"),("-_",["Integer"],"Integer"),("_<=_",["Integer","Iteger"],"Boolean")]
    procs = []
-- | Example declaring integer type and some operations but with a duplicate function name _+_
exampledslsigastintegersduplicatename = (types,funcs,procs)
  where
    types = ["Boolean","Integer"]
    funcs = [("_+_",["Integer","Integer"],"Integer"),("-_",["Integer"],"Integer"),("_+_",["Integer","Integer"],"Boolean")]
    procs = []



-- | Tests for checking the DSL consistency functions.
unittestDSLconsistent :: IO ()
unittestDSLconsistent = do
  print $ "Testing some DSL type databases: "
  print $ not $ dslConsistenttype []
     && ( dslConsistenttype ["Boolean"] )
     && ( not $ dslConsistenttype ["Boolean","Boolean"] )
     && ( dslConsistenttype ["abc","d","e","fghijkl","Boolean","mn","D"] )
     && ( not $ dslConsistenttype ["abc","d","e","fghijkl","mn","d","opqrst","uvw","xyz"] )
  print $ "Testing some DSL function databases: "
  print $ dslConsistentfunc [] []
     && ( dslConsistentfunc ["Boolean"] [("true",[],"Boolean")])
     && ( not $ dslConsistentfunc [] [("true",[],"Boolean")])
  print $ "Testing some mini-DSLs: "
  print $ dslConsistent exampledslsigastintegers
     && ( not $ dslConsistent exampledslsigastintegersnoboolean )
     && ( not $ dslConsistent exampledslsigastintegersspellerror )
     && ( not $ dslConsistent exampledslsigastintegersduplicatename )


