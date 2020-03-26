-- | Generic DIPL interpreter, parameterised by the semantics of the underlying DSL.
-- This provides a runtime semantics for DIPL programs given semantic functions for the underlying DSL.
-- The interpreter consists of two functions:
-- • Evaluate for interpreter expressions
-- • Execute for interpreting statements
--
-- Author Magne Haveraaen
-- Since 2020-02-27

module DIPLInterpreter where

-- | Uses the DSL signature delarations
import DSLDefinitions

-- | Uses the DIPL AST definition
import DIPLDefinitions

-- | Data and functions to keep track of the state of a DIPL program.
import DIPLState


------------------------

-- | The interpreter needs three central pieces of information about the DSL:
-- • Its value domain
-- • The semantics of the primitive functions
-- • The semantics of the primitive procedures

-- | The type of functions doing the DSL semantics for the functions in the DSL signature.  
type EvaluateFunCall value = String -> [value] -> value

-- | The type of functions doing the DSL semantics for the procedures the DSL signature.  
type EvaluateProCall value = String -> [ProcArg value] -> State value -> State value


------------------------

-- | The interpreter for DIPL assumes a semantics for the DSL value types, functions and procedures.
-- Since this interpreter is independent of the actual DSL, it specifically does not know the mode of primitive procedure arguments.
-- It therefore assumes that variable arguments are for input/output parameters, while other expressions are for input arguments.
-- In some cases it will therefore provide a variable rather than a value for input arguments.
-- The DSL's semantic function for procedures must take this into account and in those cases transform the variable to the corresponding value from the state.

-- | Evaluator for DIPL expressions
evaluate :: ValueType value => Expr -> EvaluateFunCall value -> State value -> value
evaluate (FunCall str texprs) evaluateFunCall state 
  = evaluateFunCall str (map (\(expr,tname) -> evaluate expr evaluateFunCall state) texprs)
evaluate (Eif (e0,tname0) (e1,tname1) (e2,tname2)) evaluateFunCall state 
  = if truthvalue (evaluate e0 evaluateFunCall state) then evaluate e1 evaluateFunCall state else evaluate e2 evaluateFunCall state
evaluate (Var str) evaluateFunCall state 
  = getvalue state str

-- | Executor for DIPL statements
execute :: ValueType value => Stmt -> EvaluateProCall value -> EvaluateFunCall value -> State value -> State value
execute Skip evaluateProCall evaluateFunCall state = state
execute (ProCall str args) evaluateProCall evaluateFunCall state = evaluateProCall str (evaluateProArgs args evaluateFunCall state) state
execute (Assign (vname,tvar) (expr,tname)) evaluateProCall evaluateFunCall state
  = changevariable state vname (evaluate expr evaluateFunCall state)
execute (Vardec vname (expr,tname)) evaluateProCall evaluateFunCall state
  = addvariable state vname (evaluate expr evaluateFunCall state)
execute (Ifte (e0,tname0) stmt1 stmt2) evaluateProCall evaluateFunCall state
  = if truthvalue (evaluate e0 evaluateFunCall state)
    then deletescope (execute stmt1 evaluateProCall evaluateFunCall (addscope state))
    else deletescope (execute stmt2 evaluateProCall evaluateFunCall (addscope state))
execute (Ift (e0,tname0) stmt1) evaluateProCall evaluateFunCall state
  = if truthvalue (evaluate e0 evaluateFunCall state)
    then deletescope (execute stmt1 evaluateProCall evaluateFunCall (addscope state))
    else state
execute (While (e0,tname0) stmt1) evaluateProCall evaluateFunCall state
  = if truthvalue (evaluate e0 evaluateFunCall state)
    then (execute (While (e0,tname0) stmt1) evaluateProCall evaluateFunCall (deletescope(execute stmt1 evaluateProCall evaluateFunCall (addscope state))))
    else state
execute (Block stmts) evaluateProCall evaluateFunCall state
  = deletescope (executestmts stmts evaluateProCall evaluateFunCall (addscope state))
-- execute stmt evaluateProCall evaluateFunCall state
--   = error $ "Unknown statement executed: " ++ (show stmt) ++ " in state " ++ (show state)

-- | Execute a statement list, one statement at a time
executestmts (stmt:stmts) evaluateProCall evaluateFunCall state
  = (executestmts stmts evaluateProCall evaluateFunCall (execute stmt evaluateProCall evaluateFunCall state))
executestmts [] evaluateProCall evaluateFunCall state
  = state
  

-- | Evaluation of procedure arguments: they are left intact if variables, otherwise fully evaluated to a value.
evaluateProArgs :: ValueType value => [TExpr] -> EvaluateFunCall value -> State value -> [ProcArg value]
evaluateProArgs ((Var str,_):args) evaluateFunCall state = (VarArg str):(evaluateProArgs args evaluateFunCall state)
evaluateProArgs ((arg,_):args) evaluateFunCall state = ValArg (evaluate arg evaluateFunCall state):(evaluateProArgs args evaluateFunCall state)
evaluateProArgs [] evaluateFunCall state = []

