-- | Grammar for DIPL from INF222 lecture 0802 on DSLs.
-- The grammar parses the concrete DIPL syntax and is DSL independent as it does not embed knowledge about specific types, functions and procedures from the DSL.
-- Instead it has general rules for identifiers - and literals, but these are not supported by DIPL.
-- The DIPL CST expressions can be converted to the DIPL AST for expressions.
-- Likewise statements CSTs can be converted to the DIPL AST for statements.
-- These conversions uses the String format for identifiers and leaves placeholders "_" for types.
-- Thus further type checking and type inference related to the DSL declarations are needed to ensure that the AST relates to the DSL.
--
-- Author: Magne Haveraaen
-- Since: 2020-02-27

module DIPLGrammar where

-- | Use the parser generator from INF222 notes 3.
import ParserGenerator
-- | Use the identifier and literal syntax
import IdentifierLiteralGrammar

-- | Use the DIPL abstract syntax
import DIPLDefinitions

-- | Use Haskell's general tree library
import Data.Tree (Tree(..), drawTree)


------------------------------

-- | Concrete grammar for DIPL using a BGL like notation encoded as Haskell data types.
-- (BGL is basic grammar language from the SL book.)
-- The DIPL language is built from expressions, expression lists, statements, statement lists and identifiers.
-- Using the parser with diplgrammar on a DIPL program represented as a string will build
-- a concrete syntax tree (CST) encoded in the general Haskell Tree type.
-- A CST can be displayed as a tree using ASCII graphics with displaytree.
-- It can also be printed back to a parsable string using printtree.
-- Most importantly it can be converted, using diplcsttoexpr and diplcsttostmt, to a proper DIPL AST as defined in DIPLDefinitions.
-- This AST uses only placeholders "_" for the (sub)expression typing field.
-- Static analysis and typechecking should be performed on the ST before it is used for purposes like interpretation and runtime sematics.

{- | A sketch of the concrete syntax.

-- Expressions
  funcall:     fname ( expressionlist )
  eif:         if e0 then e1 else e2
  variable:    vname
  parenthesis: ( e0 )

-- Statements
  procall:    pname ( expressionlist )
  assign:     vname = e0
  vardec:     var vname = e0
  ifthenelse: if e0 then stmt1 else stmt2
  ifthen:     if e0 then stmt1
  while:      while ( e0 ) stmt1
  block:      { stmtlist }
  skip:       skip

-- Expression list  
  expressionlist: ε
  expressionlist: e0, .. , ek

-- Statement list
  statementlist:  ε
  statementlist:  stmt1 ; .. ; stmtn
  
-}

------------------------------

{- | Intermediate abstract syntax for DIPL.
-- The typed expressions have been replaced by plain expressions since we do not want to have types all over the concrete syntax.
-- The basic type string has been replaced by a notion of Identifier which has grammar rules.
-- This is abstract syntax, but reorganised to fit a grammar formalism without operators ?+*.
-- Note how the list productions have been changed from Expr* and Stmt* to explicit list types ExprList and StmtList.

  type Identifier = String
  
  symbol funcall : Identifier × ExprList → Expr ; // Function call
  symbol eif : Expr × Expr × Expr → Expr ; // Conditional expression
  symbol variable : Identifier → Expr; // Variable use

  symbol procall : Identifier × ExprList → Expr ; // procedure call
  symbol assign: Identifier × Expr → Stmt; // Assign to a variable
  symbol vardec: Identifier × Expr → Stmt; // Declare variable and assign to it
  symbol ifthenelse: Expr × Stmt × Stmt → Stmt; // Conditional: if then else
  symbol ifthen: Expr × Stmt → Stmt; // Conditional: if then
  symbol while : Expr × Stmt → Stmt; // While loop
  symbol block : StmtList → Stmt ; // Statement block
  symbol skip: → Stmt; // Do nothing statement  
  
  symbol ExprEnd: → ExprList
  symbol ExprOneMore: Expr × ExprList → ExprList
  symbol StmtEnd: → StmtList
  symbol StmtOneMore: Stmt × StmtList → StmtList
-}


-- | Adding two new AST data structures corresponding to the list productions.
  
-- | Intermediate expression list AST.
data ExprList
  = ExprEnd
  | ExprOneMore Expr ExprList
  deriving (Eq, Show)

-- | Intermediate statement list AST
data StmtList
  = StmtEnd
  | StmtOneMore Stmt StmtList
  deriving (Eq, Show)


------------------------------

-- | The DIPL grammar has Stmt as starting symbol and is defined by diplrules.
diplgrammar = ("Stmt",diplrules)

-- | The DIPL rules combine:
-- • Rules for DIPL expressions.
-- • Rules for DIPL expression lists.
-- • Rules for DIPL statements.
-- • Rules for DIPL statement lists.
-- • Rules for identifiers.
-- Rules for number and string literals are not included.
diplrules
  =  diplexprrules 
  ++ diplexprlistrules 
  ++ diplstmtrules 
  ++ diplstmtlistrules
  ++identifierrules
  -- ++numberrules
  -- ++stringrules


-- | Rules for the DIPL concrete expression syntax.
-- The rules are ordered so that the most specific syntax comes first, the more general further down.
diplexprrules =
  [ ("Expr", [T "if", N "Expr", T "then", N "Expr", T "else", N "Expr"])
  , ("Expr", [T "(", N "Expr", T ")"])
  , ("Expr", [N "identifier", T "(", N "ExprList", T ")"])
  , ("Expr", [N "identifier"])
  ] :: Rules

-- | Rules for list of DIPL expression.
-- The prefix rules come later down in the rule list.
diplexprlistrules =
  [ ("ExprList", [N "Expr", T ",", N "ExprList"])
  , ("ExprList", [N "Expr"])
  , ("ExprList", [])
  ] :: Rules

-- | Rules for the DIPL statement syntax.
-- The rules are ordered so that the most specific syntax comes first, the more general further down.
diplstmtrules =
  [ ("Stmt", [T "skip"])
  , ("Stmt", [T "var", N "identifier", T "=", N "Expr"])
  , ("Stmt", [T "if", N "Expr", T "then", N "Stmt", T "else", N "Stmt"])
  , ("Stmt", [T "if", N "Expr", T "then", N "Stmt"])
  , ("Stmt", [T "while", N "Expr", N "Stmt"])
  , ("Stmt", [T "{", N "StmtList", T "}"])
  , ("Stmt", [N "identifier", T "(", N "ExprList", T ")"])
  , ("Stmt", [N "identifier", T "=", N "Expr"])
  ] :: Rules

-- | Rules for list of DIPL statements.
-- A prefix rules come after a more specific rule in the rule list.
diplstmtlistrules =
  [ ("StmtList", [N "Stmt", T ";", N "StmtList"])
  , ("StmtList", [N "Stmt"])
  , ("StmtList", [])
  ] :: Rules


------------------------------

-- | Conversion from a DIPL concrete syntax tree (CST) to the basic DIPL AST.
-- The CST is represented as a Haskell Tree data structure defined from the DIPL grammar.
-- The conversion only inserts placeholders "_" for the typing of the subexpressions.
-- Type checking and other consistency checks should be performed before the AST is used for important purposes.

-- | Convert the expression subtrees of a CST parsed with diplgrammar to the DSLDefinitions.Expr AST for DIPL.
diplcsttoexpr :: CST -> Expr
diplcsttoexpr (Node (N "Expr") [id, Node (T "(") [], el, Node (T ")") []])
  = FunCall (identifiertostring id) (diplflattenexprlist(diplcsttoexprlist(el)))
diplcsttoexpr (Node (N "Expr") [Node (T "if") [], e0, Node (T "then") [], e1, Node (T "else") [], e2])
  = Eif (diplcsttoexpr e0,"_") (diplcsttoexpr e1,"_") (diplcsttoexpr e2,"_")
diplcsttoexpr (Node (N "Expr") [Node (T "(") [], e, Node (T ")") []])
  = diplcsttoexpr e
diplcsttoexpr (Node (N "Expr") [Node (N "identifier") is])
  = Var (identifiertostring (Node (N "identifier") is))
diplcsttoexpr e = error $ "UNKNOWN alternative:" ++ (show e)

-- | Convert the expression list subtrees of a CST parsed with diplgrammar to the ExprList AST for DIPL.
diplcsttoexprlist :: CST -> ExprList
diplcsttoexprlist (Node (N "ExprList") [e, Node (T ",") [], el])
  = ExprOneMore (diplcsttoexpr e) (diplcsttoexprlist el)
diplcsttoexprlist (Node (N "ExprList") [e])
  = ExprOneMore (diplcsttoexpr e) ExprEnd
diplcsttoexprlist (Node (N "ExprList") [])
  = ExprEnd
diplcsttoexprlist cst = error $ "diplcsttoexprlist failed for: " ++ (show cst)

-- | Convert an ExprList to a list of DSLDefinitions.TExpr as used in the DSLDefinitions.Expr and DSLDefinitions.Stmt ASTs.
diplflattenexprlist :: ExprList -> [TExpr]
diplflattenexprlist (ExprOneMore e es) = (e,"_"):(diplflattenexprlist es)
diplflattenexprlist ExprEnd = []


-- | Convert the statement subtrees of a CST parsed with diplgrammar to the DSLDefinitions.Stmt AST for DIPL.
diplcsttostmt :: CST -> Stmt
diplcsttostmt (Node (N "Stmt") [Node (T "skip") []])
  = Skip
diplcsttostmt (Node (N "Stmt") [Node (T "var") [], vid, Node (T "=") [], e1])
  = Vardec (identifiertostring vid) (diplcsttoexpr e1,"_")
diplcsttostmt (Node (N "Stmt") [Node (T "if") [], e0, Node (T "then") [], stmt1, Node (T "else") [], stmt2])
  = Ifte (diplcsttoexpr e0,"_") (diplcsttostmt stmt1) (diplcsttostmt stmt2)
diplcsttostmt (Node (N "Stmt") [Node (T "if") [], e0, Node (T "then") [], stmt1])
  = Ift (diplcsttoexpr e0,"_") (diplcsttostmt stmt1)
diplcsttostmt (Node (N "Stmt") [Node (T "while") [], e0, stmt1])
  = While (diplcsttoexpr e0,"_") (diplcsttostmt stmt1)
diplcsttostmt (Node (N "Stmt") [Node (T "{") [], stmts, Node (T "}") []])
  = Block (diplflattenstmtlist(diplcsttostmtlist stmts))
diplcsttostmt (Node (N "Stmt") [pid, Node (T "(") [], args, Node (T ")") []])
  = ProCall (identifiertostring pid) (diplflattenexprlist(diplcsttoexprlist(args)))
diplcsttostmt (Node (N "Stmt") [vid, Node (T "=") [], e1])
  = Assign (identifiertostring vid,"_") (diplcsttoexpr e1,"_")
diplcsttostmt stmt = error $ "UNKNOWN alternative:" ++ (show stmt)

-- | Convert the expression list subtrees of a CST parsed with diplgrammar to the StmtList AST for DIPL.
diplcsttostmtlist :: CST -> StmtList
diplcsttostmtlist (Node (N "StmtList") [stmt, Node (T ";") [], stmts])
  = StmtOneMore (diplcsttostmt stmt) (diplcsttostmtlist stmts)
diplcsttostmtlist (Node (N "StmtList") [stmt])
  = StmtOneMore (diplcsttostmt stmt) StmtEnd
diplcsttostmtlist (Node (N "StmtList") [])
  = StmtEnd
diplcsttostmtlist cst = error $ "diplcsttostmtlist failed for: " ++ (show cst)

-- | Convert an ExprList to a list of DSLDefinitions.Stmt as used in the DSLDefinitions.Stmt ASTs.
diplflattenstmtlist :: StmtList -> [Stmt]
diplflattenstmtlist (StmtOneMore stmt stmts) = stmt:(diplflattenstmtlist stmts)
diplflattenstmtlist StmtEnd = []


------------------------------

-- | Unit testing of the DIPL grammar.
-- Here various text strings that correspond to fragments of DIPL code are created, and
-- these are then parsed and the result is output for visual inspection.
-- This a very slow unit testing approach, but is very useful for understanding what parsing does for us.


-- | Euclidean division of x by y.
dipleucliddiv = "\
\   {\
\     var q = z e r o ( ) ;\
\     var r = x ;\
\     while ( l e ( y , r ) ) {\
\       r = m i n u s ( r , y ) ;\
\       q = p l u s ( q , o n e ( ) )\
\     } ;\
\     skip ;\
\   }\
\"

unittestDIPLparsing = do
  print $ "-- Testing parser on some programs. "
  -- Uncomment the displaytree function calls below to visualise the parse.
  print $ "-- Variable expression"
  -- displayParseResult $ parse ("Expr",diplrules) "x"
  print $ diplcsttoexpr $ extractCSTstrict $ parse ("Expr",diplrules) "x"
  -- displayParseResult $ parse ("Expr",diplrules) "( x )"
  print $ diplcsttoexpr $ extractCSTstrict $ parse ("Expr",diplrules) "( x )"
  print $ "-- Function call expression"
  -- displayParseResult $ parse ("Expr",diplrules) "x ( )"
  print $ diplcsttoexpr $ extractCSTstrict $ parse ("Expr",diplrules) "x ( )"
  -- displayParseResult $ parse ("Expr",diplrules) "f ( x )"
  print $ diplcsttoexpr $ extractCSTstrict $ parse ("Expr",diplrules) "f ( x )"
  -- displayParseResult $ parse ("Expr",diplrules) "f ( x , y )"
  print $ diplcsttoexpr $ extractCSTstrict $ parse ("Expr",diplrules) "f ( x , y )"
  -- displayParseResult $ parse ("Expr",diplrules) "f ( x , y , f ( x , y ) )"
  print $ diplcsttoexpr $ extractCSTstrict $ parse ("Expr",diplrules) "f ( x , y , f ( x , y ) )"
  print $ "-- Conditional expression (if then else)"
  -- displayParseResult $ parse ("Expr",diplrules) "if a then b else c"
  print $ diplcsttoexpr $ extractCSTstrict $ parse ("Expr",diplrules) "if a then b else c"
  -- displayParseResult $ parse ("Expr",diplrules) "if x y then v a r else p l u s ( a )"
  print $ diplcsttoexpr $ extractCSTstrict $ parse ("Expr",diplrules) "if x y then v a r else p l u s ( a )"
  -- displayParseResult $ parse ("Expr",diplrules) "if f ( x , y , f ( x , y ) ) then v a r else p l u s ( m u l t ( b , a ) , b )"
  print $ diplcsttoexpr $ extractCSTstrict $ parse ("Expr",diplrules) "if f ( x , y , f ( x , y ) ) then v a r else p l u s ( m u l t ( b , a ) , b )"
  print $ "-- Statements"
  -- displayParseResult $ parse diplgrammar "skip"
  print $ diplcsttostmt $ extractCSTstrict $ parse diplgrammar "skip"
  -- displayParseResult $ parse diplgrammar "p ( )"
  print $ diplcsttostmt $ extractCSTstrict $ parse diplgrammar "p ( )"
  -- displayParseResult $ parse diplgrammar "var r = q"
  print $ diplcsttostmt $ extractCSTstrict $ parse diplgrammar "var r = q"
  -- displayParseResult $ parse diplgrammar "x = p l u s ( r , q )"
  print $ diplcsttostmt $ extractCSTstrict $ parse diplgrammar "x = p l u s ( r , q )"
  -- -- displayParseResult $ parse diplgrammar "x = _ + _ ( r , q )"
  -- displayParseResult $ parse diplgrammar "if x then skip else r = q"
  print $ diplcsttostmt $ extractCSTstrict $ parse diplgrammar "if x then skip else r = q"
  -- displayParseResult $ parse diplgrammar "{ var r = y ; if x then skip else r = q ; skip }"
  print $ diplcsttostmt $ extractCSTstrict $ parse diplgrammar "{ var r = y ; if x then skip else r = q ; skip }"
  -- displayParseResult $ parse diplgrammar "while ( l e ( y , r ) ) { q = p l u s ( q , o n e ( ) ) }"
  print $ diplcsttostmt $ extractCSTstrict $ parse diplgrammar "while ( l e ( y , r ) ) { q = p l u s ( q , o n e ( ) ) }"
  -- displayParseResult $ parse diplgrammar "while ( l e ( y , r ) ) { r = m i n u s ( r , y ) ; q = p l u s ( q , o n e ( ) ) }"
  print $ diplcsttostmt $ extractCSTstrict $ parse diplgrammar "while ( l e ( y , r ) ) { r = m i n u s ( r , y ) ; q = p l u s ( q , o n e ( ) ) }"
  -- displayParseResult $ parse diplgrammar "if if T then F else F then if V then p ( a ) else r = if q then T else F"
  print $ diplcsttostmt $ extractCSTstrict $ parse diplgrammar "if if T then F else F then if V then p ( a ) else r = if q then T else F"
  -- displayParseResult $ parse diplgrammar "if if T then F else F then if V then p ( a ) else r = if q then T else F else var g = if V then F else H"
  print $ diplcsttostmt $ extractCSTstrict $ parse diplgrammar "if if T then F else F then if V then p ( a ) else r = if q then T else F else var g = if V then F else H"
  print $ "-- Euclidean division"
  print $ lexer dipleucliddiv
  print $ printtree $ extractCSTstrict $ parse diplgrammar dipleucliddiv
  -- displayParseResult $ parse diplgrammar dipleucliddiv
  print $ diplcsttostmt $ extractCSTstrict $ parse diplgrammar dipleucliddiv
  print $ "-- Expression vs expression list on single identifier"
  print $ diplcsttoexpr $ extractCSTstrict $ parse ("Expr",diplrules) "x y z"
  print $ diplcsttoexprlist $ extractCSTstrict $ parse ("ExprList",diplrules) "x y z"
  print $ "-- Various expression lists to lists of expressions"
  print $ diplflattenexprlist $ diplcsttoexprlist $ extractCSTstrict $ parse ("ExprList",diplrules) ""
  print $ diplflattenexprlist $ diplcsttoexprlist $ extractCSTstrict $ parse ("ExprList",diplrules) "x y z"
  -- displayParseResult $ parse ("ExprList",diplrules) "x y z , a b c"
  print $ diplflattenexprlist $ diplcsttoexprlist $ extractCSTstrict $ parse ("ExprList",diplrules) "x y z , a b c"
  -- displayParseResult $ parse ("ExprList",diplrules) "x y z , a b c , d e f"
  print $ diplflattenexprlist $ diplcsttoexprlist $ extractCSTstrict $ parse ("ExprList",diplrules) "x y z , a b c , d e f"
  print $ diplflattenexprlist $ diplcsttoexprlist $ extractCSTstrict $ parse ("ExprList",diplrules) "x y z , a b c , d e f , k l m n o p q"
  print $ "end"


