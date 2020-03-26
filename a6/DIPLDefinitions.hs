-- | Definition of the DIPL, DSL Imperative Programming Language, AST.
-- DSL stands for Domain Specific (programming) Language.
--
-- NB! THE CONSISTENCY AND TYPE CHECKS FOR DIPL BELONG TO THE DIPLStaticSemantics MODULE.
--
-- Author: Magne Haveraaen
-- Since: 2020-02-20

module DIPLDefinitions where

------------------------

-- | DIPL Abstract syntax.
-- The abstract syntax is open for any DSL following the type-function-procedure understanding of software.
-- The AST does not support arbitrary software languages.
-- It only deals with langugaes where there is a separation between expression, which are evaluated to a value, and
-- statements which changes the state of the program.

{- | The DIPL AST is defined using Extended Syntax Language (ESL) from the SL book. 
-- Typed expressions and typed variable names
  type TExpr = Expr × Type
  type TString = String × Type
-- Expressions
  symbol funCall : String × TExpr* → Expr
  symbol eif : TExpr × TExpr × TExpr → Expr
  symbol var : String → Expr
-- Statements
  symbol proCall : String × TExpr* → Stmt
  symbol assign : TString × TExpr → Stmt
  symbol vardec : String × TExpr → Stmt
  symbol ifte : TExpr × Stmt × Stmt → Stmt
  symbol ift : TExpr × Stmt → Stmt
  symbol while : TExpr × Stmt → Stmt
  symbol block : Stmt* → Stmt
  symbol skip: → Stmt;  
-}


-- | Expression AST
data Expr
  = FunCall String [TExpr]
  | Eif TExpr TExpr TExpr
  | Var String 
  deriving (Eq, Read, Show)

-- | Statement AST
data Stmt
  = ProCall String [TExpr]
  | Assign TString TExpr
  | Vardec String TExpr
  | Ifte TExpr Stmt Stmt
  | Ift TExpr Stmt
  | While TExpr Stmt
  | Block [Stmt]
  | Skip
  deriving (Eq, Read, Show)


--------------------------

-- | Definition of typed expressions and variables.

-- | A typed expression is an expression with a type.
type TExpr = (Expr, Type)
-- | A typed variable is a variable name (string) with a type.
type TString = (String, Type)
-- | A type is just a string.
type Type = String


--------------------------




