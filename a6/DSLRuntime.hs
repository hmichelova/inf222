-- | This module contains runtime support for DSLs: a complete toolset for parsing and executing DSL code including IO for parameters.
-- • Extends the DIPL language with a typed parameter list
-- • Support for reading parameters and showing results.
-- • Support for parsing DIPL code for a DSL: generalrunparse (should be instantiated for the specific DSL).
-- • Support for interpreting DIPLcode for a DSL: generalruninterpreter (should be instantiated for the specific DSL).
-- The syntax also includes scripts for reading external files and interpreting them.
--
-- Author Magne Haveraaen
-- Since 2020-02-29


module DSLRuntime where

-- | Use the DSL signature tools.
import DSLDefinitions

-- | Use the DIPL abstract syntax.
import DIPLDefinitions

-- | Use the general DIPL grammar
import DIPLGrammar

-- | Use the static semantic checker
import DIPLStaticSemantics

-- | Use the DIPL state data structure.
import DIPLState

-- | Use DIPL interpreter
import DIPLInterpreter

-- | Use the parser generator
import ParserGenerator

-- | Use the identifier grammars
import IdentifierLiteralGrammar

-- | Use Haskell's general tree library since we will traverse an extended CST to the DIPL grammar
import Data.Tree (Tree(..), drawTree)


--------------------------

-- | Extending the language with programs, a parameter declaration in front of the statements.
-- This is followed up with transforming a program CST to a program AST.


-- | Grammar to parse programs: a parameter clause which contains typed parameters followed by a statement as in DIPLGrammar.diplrules.
-- A parameter has a parameter mode (one of the forms in, inout) out, a name and a type.
-- The name and the type are separated by a colon (as in the Pascal family of languages).
-- The parameters are given in a comma separated list.
-- There are 0 or more parameters.
diplprogramrule =
  [
    ( "program", [ T "parameters", T "(", N "IOList", T ")", N "Stmt" ] )
  , ( "IOList", [ N "parammode", N "identifier", T ":", N "identifier", T ",", N "IOList" ] )
  , ( "IOList", [ N "parammode", N "identifier", T ":", N "identifier" ] )
  , ( "IOList", [ ] )
  , ( "parammode", [ T "in" ] )
  , ( "parammode", [ T "inout" ] )
  , ( "parammode", [ T "out" ] )
  ] :: Rules
diplprogram = ( "program", diplprogramrule++diplrules )

-- | Program abstract AST: contains parameters to the program as well as the program statement.
data Program
  = Prog [PParam] Stmt
  deriving (Eq, Read, Show)
type PParam = (Parametermode, String, Type)

-- | Convert the program subtrees of a CST parsed with diplprogram to the Program AST for DIPL.
diplcsttoprogram :: CST -> Program
diplcsttoprogram (Node (N "program") [Node (T "parameters")[], Node (T "(") [], ciolist, Node (T ")") [], cstmt])
  = Prog (diplflattenpparamlist(diplcsttopparamlist ciolist)) (diplcsttostmt cstmt)
diplcsttoprogram e = error $ "UNKNOWN alternative:" ++ (show e)


-- | Intermediate parameter list AST for capturing the grammar productions for IOList.
data PParamList
  = PParamEnd
  | PParamOneMore PParam PParamList
  deriving (Eq, Read, Show)

-- | Convert an PParamList to a list of PParam.
diplflattenpparamlist :: PParamList -> [PParam]
diplflattenpparamlist (PParamOneMore pp pps) = pp:(diplflattenpparamlist pps)
diplflattenpparamlist PParamEnd = []

-- | Convert the ciolist subtree of a CST parsed with diplprogram to the PParamList AST.
diplcsttopparamlist :: CST -> PParamList
diplcsttopparamlist (Node (N "IOList") [cmod, cid, Node (T ":") [], ctid, Node (T ",") [], ciolist])
  = PParamOneMore (diplcsttopparammode cmod,identifiertostring cid,identifiertostring ctid) (diplcsttopparamlist ciolist)
diplcsttopparamlist (Node (N "IOList") [cmod, cid, Node (T ":") [], ctid])
  = PParamOneMore (diplcsttopparammode cmod,identifiertostring cid,identifiertostring ctid) PParamEnd
diplcsttopparamlist (Node (N "IOList") [])
  = PParamEnd
diplcsttopparamlist e = error $ "UNKNOWN diplcsttopparamlist alternative:" ++ (show e)

-- | Convert the parameter mode CST to the parameter mode AST.
diplcsttopparammode :: CST -> Parametermode
diplcsttopparammode (Node (N "parammode") [(Node (T "in")[])]) = In
diplcsttopparammode (Node (N "parammode") [(Node (T "inout")[])]) = Inout
diplcsttopparammode (Node (N "parammode") [(Node (T "out")[])]) = Out
diplcsttopparammode e = error $ "UNKNOWN diplcsttopparammode alternative:" ++ (show e)


--------------------------

-- | Takes a program text (string) and a string of space separated data values (words) as arguments, and a default string for missing data.
-- The result is a quadruple of
-- • parameter list with the parameter mode and formal parameter names,
-- • a list of variable name-type names extracted from the parameter list
-- • a list of variable name-data strings matched up from the parameter list and the words of the input data string
-- • matching list of integer values, from the argument list if in or inout, 0 otherwise
-- • the AST corresponding program code's statement.
programclosure :: String -> String -> (Type -> String) -> ([PParam], [(String,Type)], [(String,Type,String)], Stmt)
programclosure program instring defaultdata
  = (params, paramtlist, paramdlist, stmt')
  where
    -- Parse input program into a parameter list-statement pair
    Prog params stmt = diplcsttoprogram $ extractCSTweak $ parse diplprogram program
    -- Parse parameter list into a list of variable-type pairs
    paramtlist = getparameternametypepairs params
    -- Parse input data values and fill in with default values where needed
    paramdlist = getparameternamedatapairs defaultdata params (words instring)
    stmt' = if (length params) == (length paramtlist) && (length params) == (length paramdlist)
            then stmt 
            else error $ "Parameter length mismatch: " ++ (show params) ++ " paramtlist=" ++ (show paramtlist) ++ " paramdlist=" ++ (show paramdlist)
    

-- | Takes a program text (file name) and a string of space separated data values (words) as arguments, and a default string for missing data.
-- programfileclosure :: FilePath -> String -> IO ([PParam], [Readable], Stmt)
programfileclosure pathname instring defaultdata = do
  program <- readFile pathname
  return (programclosure program instring defaultdata)
  -- where IO program = readFile pathname

-- | Take a parameter list and return a list of parameter name-parameter type pairs.
getparameternametypepairs :: [PParam] -> [(String,Type)]
getparameternametypepairs ((pmode,vname,tname):pparams) = (vname,tname):(getparameternametypepairs pparams)
getparameternametypepairs [] = []

-- | Take a parameter list, a list of input strings and a default value.
-- Return a list of parameter name-argument value pairs.
getparameternamedatapairs :: (Type -> String) -> [PParam] -> [String] -> [(String,Type,String)]
getparameternamedatapairs defaultdata ((In,vname,tname):pparams) (indata:indatas)
  = (vname,tname,indata):(getparameternamedatapairs defaultdata pparams indatas)
getparameternamedatapairs defaultdata ((Inout,vname,tname):pparams) (indata:indatas)
  = (vname,tname,indata):(getparameternamedatapairs defaultdata pparams indatas)
getparameternamedatapairs defaultdata ((Out,vname,tname):pparams) indatas
  = (vname,tname,defaultdata tname):(getparameternamedatapairs defaultdata pparams indatas)
getparameternamedatapairs defaultdata [] []
  = []
getparameternamedatapairs defaultdata pparams datas
  = error $ "Error parsing argument data, remaining pparamas=" ++ (show pparams) ++ " remainig args=" ++  (show datas)

-- | Add a list of data to the current environment of the state.
addtostate :: ValueType value => (Type -> String -> value) -> [(String,Type,String)] -> State value -> State value
addtostate readval ((vname,tname,indata):vdatas) state
  = addtostate readval vdatas (addvariable state vname (readval tname indata))
addtostate readval [] state
  = state

-- | Take a show value function, a parameter list, and a state.
-- Return a list of return values from the function.
-- showoutputvalues :: ValueType value => ([PParam] -> String -> value) -> [PParam] -> State -> [String]
showoutputvalues showval ((In,vname,tname):pparams) state
  = showoutputvalues showval pparams state
showoutputvalues showval ((Inout,vname,tname):pparams) state
  = (showval tname (getvalue state vname)):(showoutputvalues showval pparams state)
showoutputvalues showval ((Out,vname,tname):pparams) state
  = (showval tname (getvalue state vname)):(showoutputvalues showval pparams state)
showoutputvalues showval [] state
  = []
-- | Turn a parameter list and a state into a string representing output values.
showoutput showval pparams state = unwords (showoutputvalues showval pparams state)


--------------------------

runtimeexampleprogram1 = "parameters ( ) skip"
runtimeexampleprogram2 = "parameters ( in x y z : B ) skip"
runtimeexampleprogram3 = "parameters ( in x y z : S , out j o h n : S ) skip"
runtimeexampleprogram4 = "parameters ( in x y z : S t r i n g , inout d 3 4 : T u v ) skip"
runtimeexampledefaults :: Type -> String
runtimeexampledefaults "B" = "bad"
runtimeexampledefaults "Boolean" = "truth"
runtimeexampledefaults "Integer" = "count"
runtimeexampledefaults "S" = "strong"
runtimeexampledefaults "String" = "story"
runtimeexampledefaults "Tuv" = "Vilde"
runtimeexampleparams (params,_,_,_) = params
runtimeexampleintypes (_,intypes,_,_) = intypes
runtimeexamplestmt (_,_,_,stmt) = stmt
runtimeexampletest dslsignature dsldefault program input = do
  let dslcnst = dslConsistent dslsignature
  if dslcnst then putStr "" else print $ "DSL signature " ++ (show dslsignature) ++ " is not consistent."
  let p = programclosure program input dsldefault
  let wfmd = wellformedstmt ([map fst (runtimeexampleintypes p)]) (runtimeexamplestmt p)
  if wfmd then putStr "" else print $ "Program " ++ (show p) ++ " is not wellformed."
  let wtstmt = welltypedstmt dslsignature (runtimeexamplestmt p,[runtimeexampleintypes p])
  putStrLn $ show wtstmt

unittestRuntimeProgram = do
  print $ "-- Testing parsing of programs"
  runtimeexampletest (["Boolean"],[],[]) runtimeexampledefaults runtimeexampleprogram1 ""
  runtimeexampletest (["Boolean","B"],[],[]) runtimeexampledefaults runtimeexampleprogram2 "bold"
  runtimeexampletest (["Boolean","S"],[],[]) runtimeexampledefaults runtimeexampleprogram3 "stream"
  runtimeexampletest (["Boolean","String","Tuv"],[],[]) runtimeexampledefaults runtimeexampleprogram4 "more music"
  programcomputus <- readFile "DSLInteger-computus.dipl"
  let p5 = programclosure programcomputus "2020" runtimeexampledefaults
  programeuclidean <- readFile "DSLInteger-euclidean.dipl"
  let p6 = programclosure programeuclidean "23 4" runtimeexampledefaults
  let dslbool = (["Boolean"],[("and",["Boolean","Boolean"],"Boolean")],[("swap",[(Inout,"Boolean"),(Inout,"Boolean")])]) :: DSLsignature
  programdsland <- readFile "DSLBoolean-and.dipl"
  print $ "Testing program: " ++ programdsland
  runtimeexampletest dslbool runtimeexampledefaults programdsland "False False"
  programdslswap <- readFile "DSLBoolean-swap.dipl"
  print $ "Testing program: " ++ programdslswap
  runtimeexampletest dslbool runtimeexampledefaults programdslswap "False True"
  print $ "Test finished"
  

--------------------------

-- | Reading in file and initialisation data, and parsing it, and printing useful status information.
generalrunparse :: DSLsignature -> (Type -> String) -> String -> String -> IO ()
generalrunparse dslsignature dsldefaultdata filename input = do
  -- Compile program and use input for argument data
  let dslconsistent = dslConsistent dslsignature
  if not dslconsistent then print $ "Input DSL is not well formed " ++ (show(dslsignature)) else putStr ""
  -- let (params, intypes, inputlist, stmt) = programclosure filename input dsldefaultdata
  (params, intypes, inputlist, stmt) <- programfileclosure filename input dsldefaultdata
  -- putStrLn $ "params=" ++ (show params)
  -- putStrLn $ "intypes=" ++ (show intypes)
  putStrLn $ "inputlist=" ++ (show inputlist)
  -- putStrLn $ "stmt=" ++ (show stmt)
  -- Check static semantics of and add type annotations to parsed program.
  let stenv = ([intypes])
  let (stmt',stenv') = welltypedstmt dslsignature (stmt,stenv)
  putStrLn $ "stmt'=" ++ (show stmt')
  if stenv' == stenv then putStr "" else error $ "Mismatch of input type environment " ++ (show stenv) ++ " and output type environment " ++ (show stenv')
  if stenv' == stenv && welltypedstmt dslsignature (stmt',stenv') == (stmt',stenv')
  then putStr "" -- print $ "Static semantics OK stmt'=" ++ (show (stmt',senv'))
  else print $ "Static semantic error: stmt'=" ++ (show (stmt',stenv,stenv'))
  putStrLn "Done"


-- | Reading in file and initialisation data, and parsing them, and running it with appropriately detailed DSL information.
generalruninterpreter :: ValueType value => 
  DSLsignature -> 
  EvaluateProCall value -> EvaluateFunCall value -> 
  (Type -> String) -> (Type -> String -> value) -> (Type -> value -> String) -> 
  State value ->
  String -> String -> IO ()
generalruninterpreter dslsignature dslsemprc dslsemfun dsldefaultdata dslreadval dslshowval startupstate filename input = do
  -- Compile program and use input for argument data
  let dslconsistent = dslConsistent dslsignature
  if not dslconsistent then print $ "Input DSL is not well formed " ++ (show(dslsignature)) else putStr ""
  -- let (params, intypes, inputlist, stmt) = programclosure filename input dsldefaultdata
  (params, intypes, inputlist, stmt) <- programfileclosure filename input dsldefaultdata
  -- putStrLn $ "params=" ++ (show params)
  -- putStrLn $ "intypes=" ++ (show intypes)
  -- putStrLn $ "inputlist=" ++ (show inputlist)
  -- putStrLn $ "stmt=" ++ (show stmt)
  -- Check static semantics of and add type annotations to parsed program.
  let stenv = ([intypes])
  let (stmt',stenv') = welltypedstmt dslsignature (stmt,stenv)
  -- putStrLn $ "stmt'=" ++ (show stmt')
  if stenv' == stenv then putStr "" else error $ "Mismatch of input type environment " ++ (show stenv) ++ " and output type environment " ++ (show stenv')
  if stenv' == stenv && welltypedstmt dslsignature (stmt',stenv') == (stmt',stenv')
  then putStr "" -- print $ "Static semantics OK stmt'=" ++ (show (stmt',senv'))
  else print $ "Static semantic error: stmt'=" ++ (show (stmt',stenv,stenv'))
  -- Interpret program and extract output.
  let state0 = addtostate dslreadval inputlist (startupstate)
  let state1 = execute stmt' dslsemprc dslsemfun state0
  -- putStrLn $ "state0=" ++ (show state0)
  -- putStrLn $ "state1=" ++ (show state1)
  let output = showoutput dslshowval params state1
  print$ output
  putStrLn "Done"


