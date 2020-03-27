-- | Definition of the type checker/type inference functions for DIPL, DSL Imperative Programming Language, AST.
-- DSL stands for Domain Specific (programming) Language.
-- Type checking / inference is a versy efficient technique for discovering coding errors.
-- 
-- The module provides two main functions:
-- • Checking that a DIPL program is wellformed: wellformedexpr wellformedstmt
--   DIPL is a scoped language, i.e.,
--   variables declared in if-then/if-then-else branches, in the while statement or block statement belong to the local scope of that statement.
--   Checks that variables are declared before use, that the same variable is not declared multiple times in the same scope.
--   These checks are subsumed by the type checker and is therefore not vital for checking wellformedness.
--   A minimal version is provided.
-- • Checking the typing of a DIPL program for a DSL: welltypedexp welltypedstmt
--   The parser leaves the typing of a DIPL program as place holders "_" in the AST.
--   From the DSL declarations (argument and return types for the DSL functions, argument type and mode for the DSL procedures) and typing of variables,
--   it is possible for the type checker to infer the type from the smallest subexpressions upwards.
--   For this the type checker has to keep track of the variables, their scoping and types, thus subsuming the wellformedness check above.
--   There are also general rules that the conditional expression for if-then, if-then-else and while have to be of type boolean.
--   Likewise the argument to a function or procedure have to match the operation's declared argument types.
--   If the type inference is in conflict with these requirements, the program has a type error.
--   This can be signalled by an error stop with a relevant diagnostic message.
--
-- The functions defined here are called by their name in other parts of the DIPL framework, e.g., in DSLRuntime.hs,
-- it is therefore important that function names and their declared type parameters are unchanged when their implementation is extended.
--
--
-- Author: Magne Haveraaen
-- Since: 2020-02-20

module DIPLStaticSemantics where

-- | Uses the DSL signature delarations for checking the wellformedness and typing of DIPL expressions for a DSL.
import DSLDefinitions

-- | DIPL Abstract syntax.
import DIPLDefinitions


--------------------------

-- | Wellformed DIPL

-- | An environment for checking static scoping. 
-- An environment sequentially accumulates variable names in the inner list.
-- New scopes open an empty environment in the outer list, and end of scope removes that environment.
-- A variable is searched for from the most recent scope towards the enclosing ones. 
type StaticEnvironment = [[String]]
data Bind
  = Scope
  | Declared String
  deriving (Eq, Show)

-- | Testing the wellformedness of expressions given a scoped environment of variables names.
-- Recall that no type information is available since we do not have access to the DSL signature.
wellformedexpr :: StaticEnvironment -> Expr -> Bool
wellformedexpr senv (FunCall _ args) = and $ map (wellformedexpr senv) $ map fst args
wellformedexpr senv (Eif expr1 expr2 expr3) = 
  and $ map (wellformedexpr senv) $ map fst [expr1, expr2, expr3]
wellformedexpr senv (Var var) = or $ map (elem var) senv

-- | Testing the wellformedness of statements given a scoped environment of variables names.
-- Recall that no type information is available since we do not have access to the DSL signature.
-- This function maintains the scoping rules of variable declarations.
wellformedstmt :: StaticEnvironment -> Stmt -> Bool
wellformedstmt senv (ProCall _ args) = and $ map (wellformedexpr senv) $ map fst args
wellformedstmt senv (Assign name expr) = wellformedexpr senv (fst expr) 
  && or (map (elem (fst name)) senv)
wellformedstmt senv@(env : _) (Vardec name expr) = wellformedexpr senv (fst expr)
  && not (elem name env)
wellformedstmt senv (Ifte expr stmt1 stmt2) = wellformedexpr senv (fst expr)
  && wellformedstmt ([] : senv) stmt1
  && wellformedstmt ([] : senv) stmt2
wellformedstmt senv (Ift expr stmt) = wellformedexpr senv (fst expr)
  && wellformedstmt ([] : senv) stmt
wellformedstmt senv (While expr stmt) = wellformedexpr senv (fst expr)
  && wellformedstmt ([] : senv) stmt
wellformedstmt senv (Block stmts) = wellformedstmtlist ([] : senv) stmts
wellformedstmt _ Skip = True 

-- | Testing the wellformedness of statement lists given a scoped environment of variables names.
wellformedstmtlist :: StaticEnvironment -> [Stmt] -> Bool
wellformedstmtlist (_ : _) [] = True
wellformedstmtlist se@(scope : senv) (stmt@(Vardec name _) : stmts) = 
  wellformedstmt se stmt
  && wellformedstmtlist ((name : scope) : senv) stmts
wellformedstmtlist senv (stmt : stmts) = wellformedstmt senv stmt 
  && wellformedstmtlist senv stmts
wellformedstmtlist senv stmts = error $ "Improper use of wellformedstmt, senv=" ++ (show senv) ++ " stmts=" ++ (show stmts)


--------------------------

-- | Checking that DIPL code is both wellformed and welltyped given the DSL: checks type consistency within DIPL.


-- | Welltyped DIPL expression given the DSL declarations and typed external environment data.
-- This function checks type consistency within DIPL by inferring the type form subexpressions upwards
-- (variable types in the scoped environment, return type of functions, return type of if-then expressions),
-- and also checks if the type of a subexpression matches its expected type
-- (Boolean for conditionals, argument types for function and procedure calls).
-- If there is a match, type inference continues.
-- If not a diagnostic message is printed and type checking stops.
-- A placeholder type "_" is replaced by a proper type if type checking is successfull.
--
-- After type checking, all placeholder types shall have been replaced by proper type names from the DSL.
welltypedexp :: ScopedTypeEnvironment -> DSLsignature -> TExpr -> TExpr
welltypedexp senv dsldecl (FunCall str texprs, tu) = (FunCall str ctexprs, rtes)
  where
    te = if tu == "_" || findtypdecl tu dsldecl then tu else error $ "Inappropriate expected expression type " ++ tu ++ " for call to function " ++ str
    (fname,tps,tr) = findfundecl str dsldecl
    rtes = if te == tr || te == "_" 
      then tr 
      else error $ "Wrong expected return type " ++ te ++ " for function " ++ str ++ " : " ++ tr
    ctexprs = welltypedexplist senv dsldecl tps texprs
welltypedexp senv dsldecl (Var str, t) = (Var str, tr)
  where
    tv = scopedtypefind str senv
    tr = if t == tv || t == "_" 
      then tv 
      else error $ "Wrong expected type " ++ t ++ " for variable " ++ str ++ " with known type " ++ tv
welltypedexp senv dsldecl (Eif expr1 expr2 expr3, t) 
  = (Eif cexpr1 cexpr2 cexpr3, tr)
  where
    te = if t == "_" || findtypdecl t dsldecl 
      then t 
      else error $ "Inappropriate expected expression type " ++ t ++ " for eif expression "
    cexpr1@(_, t1) = welltypedexp senv dsldecl expr1
    cexpr2@(_, t2) = welltypedexp senv dsldecl expr2
    cexpr3@(_, t3) = welltypedexp senv dsldecl expr3
    tr 
      | t /= te && t /= "_" = error $ "Wrong expected type " ++ t ++ " for eif expression " ++ show (Eif expr1 expr2 expr3)
      | t1 /= "Boolean"     = error $ "Inappropriate type of first expression " ++ show expr1 ++ " for eif expression"
      | t2 /= t3            = error $ "Inappropriate type of second expression " ++ show expr2 ++" and third expression " ++ show expr3 ++ " for eif expression - do not have the same type"
      | otherwise           = t2
-- welltypedexp senv dsl texpr = error $ "Unknown typing of " ++ show texpr ++ " in DSL."

-- | Checking the type list against the declared types of the expressions as inferred from the DSL.
welltypedexplist :: ScopedTypeEnvironment -> DSLsignature -> TYPES -> [TExpr] -> [TExpr]
welltypedexplist senv dsldecl (t1:t1s) ((tx2,t2):tx2s)
  = if t1 == t2 || t2 == "_"
    then (welltypedexp senv dsldecl (tx2,t1)):(welltypedexplist senv dsldecl t1s tx2s)
    else error $ "Argument type mismatch: expected " ++ t1 ++ " found " ++ t2 ++ " for subexpression " ++ (show tx2)
welltypedexplist senv dsldecl [] [] = []
welltypedexplist senv dsldecl t1s tx2s = error $ "Type lists do not match: " ++ (show t1s) ++ " vs " ++ (show tx2s)


-- | Checks type consistency of statements within DIPL.
-- Adds type annotations to the AST for expressions based on the given DSL signature.
-- Uses a scoped environment to keep track of variable-type bindings.
welltypedstmt :: DSLsignature -> (Stmt, ScopedTypeEnvironment) -> (Stmt, ScopedTypeEnvironment)
welltypedstmt dsldecl (ProCall str texprs, senv) = (ProCall str texprs', senv)
  where
    (pname, tps) = findprodecl str dsldecl
    texprs' = welltypedprocallparamlist senv dsldecl pname tps texprs
welltypedstmt dsldecl (Assign (str, strt) (expr, exprt), senv)
  = (Assign (str, strt'') (expr', exprt'), senv)
  where
    strt'  = scopedtypefind str senv
    -- subexpression is welltyped
    (expr', exprt') = welltypedexp senv dsldecl (expr, exprt)
    strt'' = if strt' /= "_" && strt' == exprt' && (strt == "_" || strt == strt')
      then strt' 
      else error $ "Wrong typing of variable " ++ str ++ " expected type " ++ strt ++ " declared type " ++ strt' ++ " expression type " ++ exprt
welltypedstmt dsldecl (Vardec name ext@(expr, exprt), senv) = 
  (Vardec name (expr', exprt''), senv')
  where
    senv' = scopedvaradd name exprt'' senv
    (expr', exprt') = welltypedexp senv dsldecl ext
    exprt'' = if exprt == "_" || exprt == exprt'
      then exprt'
      else error $ "Wrong typing of expression " ++ show expr ++ " expected type " ++ exprt ++ " declared type " ++ exprt'
welltypedstmt dsldecl (Ifte ex@(expr, exprt) stmt1 stmt2, senv) = 
  (Ifte ex' stmt1' stmt2', senv)
  where
    ex' = welltypedexpCheckBoolean senv dsldecl ex
    (stmt1', _) = welltypedstmt dsldecl (stmt1, newtypescope senv)
    (stmt2', _) = welltypedstmt dsldecl (stmt2, newtypescope senv)
welltypedstmt dsldecl (Ift ex@(expr, exprt) stmt, senv) = (Ift ex' stmt', senv)
  where
    ex' = welltypedexpCheckBoolean senv dsldecl ex
    (stmt', _) = welltypedstmt dsldecl (stmt, newtypescope senv)
welltypedstmt dsldecl (While ex@(expr, exprt) stmt, senv) = (While ex' stmt', senv)
  where
    ex' = welltypedexpCheckBoolean senv dsldecl ex
    (stmt', _) = welltypedstmt dsldecl (stmt, newtypescope senv)
welltypedstmt dsldecl (Block stmts, senv) = (Block stmts', senv)
  where
    (stmts', _) = welltypedstmtlist dsldecl (stmts, newtypescope senv)
welltypedstmt dsldecl (Skip, senv) = (Skip, senv)
-- welltypedstmt dsldecl (stmt,senv) = error $ "Unknown DIPL/DSL statement " ++ show stmt ++ " with environment " ++ show senv

welltypedexpCheckBoolean :: ScopedTypeEnvironment -> DSLsignature -> TExpr -> TExpr
welltypedexpCheckBoolean senv dsldecl ex@(expr, exprt) = (expr', exprt'')
  where
    (expr', exprt') = welltypedexp senv dsldecl ex
    exprt'' = if (exprt == "_" || exprt == exprt') && exprt' == "Boolean"
      then exprt'
      else error $ "Wrong typing of expression " ++ show expr ++ " expected type " ++ exprt ++ " declared type " ++ exprt'


-- | Checking that a statement list is well typed.
welltypedstmtlist dsldecl (stmt : stmts, senv) = (stmt' : stmts', senv)
  where
    (stmt', senv') = welltypedstmt dsldecl (stmt, senv)
    (stmts', senv'') = welltypedstmtlist dsldecl (stmts, senv')
welltypedstmtlist dsldecl ([], senv) = ([], senv)


-- | Type checking the argument list against the declared parameter list for procedure pname.
welltypedprocallparamlist senv dsldecl pname ((Inout, vt) : tps) ((Var ev, evt) : texprs)
  = (e', evt') : welltypedprocallparamlist senv dsldecl pname tps texprs
  where
    (e',evt') = if evt == "_" || evt == vt
      then welltypedexp senv dsldecl (Var ev,vt) 
      else error $ "Wrongly typed procedure " ++ pname ++ " argument, expected " ++ vt ++ " got type " ++ evt
welltypedprocallparamlist senv dsldecl pname ((Out, vt) : tps) ((Var ev, evt) : texprs)
  = (e', evt') : welltypedprocallparamlist senv dsldecl pname tps texprs
  where
    (e', evt') = if evt == "_" || evt == vt
      then welltypedexp senv dsldecl (Var ev,vt) 
      else error $ "Wrongly typed procedure " ++ pname ++ " argument, expected " ++ vt ++ " got type " ++ evt
welltypedprocallparamlist senv dsldecl pname ((In, vt) : tps) ((e, et) : texprs)
  = (e', et') : welltypedprocallparamlist senv dsldecl pname tps texprs
  where
    (e',et') = if et == "_" || et == vt
      then welltypedexp senv dsldecl (e,vt) 
      else error $ "Wrongly typed procedure " ++ pname ++ " argument, expected " ++ vt ++ " got type " ++ et
welltypedprocallparamlist senv dsldecl pname [] [] = []


--------------------------

-- | A scoped type environment for variable-type bindings is a list of type environments.
-- A type environment is an association list of variable names and type names.
-- A variable can only be defined in an environment, though it may be defined in multiple enclosing scopes.

-- A scoped type environment is a list of type environments.
-- The newest (topmost or innermost) environment is the first element of the list.
-- An environment further down the list is enclosing the ones towards the top.
-- A variable is looked up from the topmost environment to the enclosing ones.
-- When searching for a variable in the scoped environment:
-- • If it is declared more than once in a scoped environment, only the topmost occurance is returned.
-- • If it is not declared, the search fails with an error message.
type ScopedTypeEnvironment = [TypeEnvironment]

-- | A new scoped environment contains an empty environment.
newscopedtypeenvironment :: ScopedTypeEnvironment
newscopedtypeenvironment = [newtypeenvironment]

-- | Add a new, empty topmost scope to the environment.
newtypescope :: ScopedTypeEnvironment -> ScopedTypeEnvironment
newtypescope senv = newtypeenvironment:senv

-- | Delete the topmost scoped environment.
deltypescope :: ScopedTypeEnvironment -> ScopedTypeEnvironment
deltypescope (env:senv) = senv
deltypescope [] = error $ "Trying to delete the empty scope of type environments."

-- | Add a variable-type binding to the topmost scope.
-- Give an error message if it was already registered in the topmost scope.
scopedvaradd :: String -> Type -> ScopedTypeEnvironment -> ScopedTypeEnvironment
scopedvaradd str tval (env:senv)
  = if findenvtype str env == "_"
   then (newvartype env str tval):senv
   else error $ "Variable " ++ str ++ ":" ++ tval ++ " is already declared in the topmost scope of " ++ (show (env:senv))
scopedvaradd str tval senv = error $ "Trying to add variable " ++ str ++ ":" ++ tval ++ " to an empty scoped environment " ++ (show senv)

-- | Look up the type of a variable name in a scoped type environment.
-- Looking up a nonexistent variable name is an error..
scopedtypefind :: String -> ScopedTypeEnvironment -> Type
scopedtypefind str (env:senv) = if res == "_" then scopedtypefind str senv else res
  where
    res = findenvtype str env
scopedtypefind str [] = error $ "No type for variable name: " ++ str


-- | An environemnt is an association list of variable names and type names.
-- The most recent declaration is first in the list.
-- If multiple declarations of a variable is in an environment, only the first is found.
type TypeEnvironment = [(String,Type)]

-- | Create an emty type environment
newtypeenvironment :: TypeEnvironment
newtypeenvironment = []

-- | Add a variable-type binding.
newvartype :: TypeEnvironment -> String -> Type -> TypeEnvironment
newvartype env str tval = (str,tval):env

-- | Look up the type of a variable.
-- If the variable is not registered, return placeholder "_"
findenvtype :: String -> TypeEnvironment -> Type
findenvtype str ((vname,vtype):env) = 
  if str == vname then vtype else findenvtype str env
findenvtype str [] = "_"


--------------------------

-- | Some unit tests
unittestdiplstaticsemantics = do
  let texp = (FunCall "rem" [(Var "year","_"),(FunCall "no100" [],"_")],"_")
  let dslintegers
        = (["Boolean","Integer"],
           [("plus",["Integer","Integer"],"Integer"),("minus",["Integer","Integer"],"Integer"),("mult",["Integer","Integer"],"Integer"),
            ("idiv",["Integer","Integer"],"Integer"),("rem",["Integer","Integer"],"Integer"),
            ("no0",[],"Integer"),("no1",[],"Integer"),("no2",[],"Integer"),("no3",[],"Integer"),("no4",[],"Integer"),("no5",[],"Integer"),
            ("no6",[],"Integer"),("no7",[],"Integer"),("no8",[],"Integer"),("no9",[],"Integer"),
            ("le",["Integer","Integer"],"Boolean"),("and",["Boolean","Boolean"],"Boolean"),("not",["Boolean"],"Boolean"),
            ("true",[],"Boolean"),("false",[],"Boolean"),
            ("no100",[],"Integer")
           ],
           [("double",[(Inout,"Integer")]),("add",[(Inout,"Integer"),(In,"Integer")])]
          )
  let wtexp = welltypedexp [[],[("year","Integer")]] dslintegers texp
  print $ welltypedexp [[],[("year","Integer")]] dslintegers wtexp
  print $ "Done"

