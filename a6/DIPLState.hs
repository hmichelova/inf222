-- | Defines what the runtime state for DIPL interpreter should be.
-- The state is based on:
-- • a scoped environment of varible names-store indices and
-- • a modifiable store of values indexed by store indices.
-- The environment links to store indices to allow fast update of stored values.
-- Currently all stored data types are considered to take up the same amount of storages, i.e., one store index.
-- 
-- Author Magne Haveraaen
-- Since 2020-02-26


module DIPLState where

-- | Uses the Haskell array type for the store.
import Data.Array


--------------------------

-- | Here we demand that the value type, whatever it will be, must have a mapping to the Haskell Bool type.
-- The values to be kept in the store are not known until the semantic domain of the DSL is chosen.
-- We know that DIPL requires a boolean type for its conditionals, and 
-- we expect that the interpreter will need Haskell Bool to make the choices.
-- We need to define it already here since Haskell demands to know what type (or type class) we use for our data as we build the types.
class Show a => ValueType a where
  truthvalue :: a -> Bool



-- | The state contains a scoped environment for variables and a store for values.
-- This corresponds to the static semantic requirements of the DIPL statement language.
type State value = (ScopedEnvironment,Store value)


--------------------------

-- | Convenience functions on the state:
-- • Empty start state (a constant)
-- • Adding and removing a scope for variable declarations (to follow DIPL scoping rules).
-- • Adding a variable to the outermost scope and adding its value to a fresh store location.
-- • Getting the value, or changing it, contained in a variable by accessing the corresponding store location. 


-- | Setting up an empty runtime environment.
startstate :: ValueType value => State value
startstate = (newscopedenvironment,newstore)


-- | A new topmost scope, contained within the earlier scopes.
addscope :: ValueType value => State value -> State value
addscope (senv,st) = (newscope senv, st)

-- | Delete the topmost scope, retaining the earlier scopes.
deletescope :: ValueType value => State value -> State value
deletescope (senv,st) = (delscope senv, st)

-- | Add a new variable to the topmost environment, and add its value to a fresh location in the store.
addvariable :: ValueType value => State value -> String -> value -> State value
addvariable (env:senv,st) str val = (newenv:senv,newst)
  where (newenv,newst) = newvarval (env,st) str val

-- | Get the value associated with an accessible variable from its store location.
getvalue :: ValueType value => State value -> String -> value
getvalue (senv,st) str = getval st (scopedfind str senv)

-- | Change the value associated with an accessible variable at its store location.
changevariable :: ValueType value => State value -> String -> value -> State value
changevariable (senv,st) str val = (senv, update st (scopedfind str senv) val)


--------------------------

-- | The DIPL store model has one global store, each location is indexed by a store index.
-- Accessing the store is normally related to the environment keeping track of relevant store indices.
-- In that case using the conveience functions on the State is recommended.
--
-- The store is represented a pair: the top (the next vacant index) and array containing values.
-- The valid store indices are integers in the range 0..top-1.
-- The store has primitive functions for manipulating it.
-- • Empty start store: a constant which on creation needs to be explicitly typed by the value type it will contain.
-- • A canonical out of bounds index, storeerrorindex, used to signal information about the store.
-- • Extending the store with one more store index and placing a value there.
-- • Getting the newest store index that is used (updated when extending the store).
-- • Getting the value, or updating it, at a store location. 
-- For simplicity we assume all values occupy the same amount of storage.
-- We do not clean up the store when data go out of scope (the owning variable dissappears).

-- | A store is a mapping from StoreIndex (starting at 0) to values.
-- It is represented as a pair (top, array (0,top-1) [(0,x0),...,(topm1,xtopm1)]),
-- with actual index range 0..(top-1).
type Store value = (StoreIndex, Array StoreIndex value)
-- | The Store's locations, its index domain, is integer.
type StoreIndex = Integer

-- | Canonical index of a missing store element, used to report errors.
storeerrorindex = -1

-- | Defines a store with an empty index range
newstore :: ValueType value => Store value
newstore = (0,array (0,-1) []) -- :: ValueType value => Store value

-- | Get the newest index, i.e., the store's index (top-1).
-- Note how this becomes storeerrorindex if the store is empty.
getnewestindex :: ValueType value => Store value -> StoreIndex
getnewestindex (top,st) = top-1

-- | Extends the store range by one element
extend :: ValueType value => Store value -> value -> Store value
extend (top,st) val = (top+1, array (0,top) ((assocs st)++[(top,val)]))

-- | Get the value stored at the given store index.
getval :: ValueType value => Store value -> StoreIndex -> value
getval (top,st) ind = 
  if ind < top then st ! ind else error "Index out of bounds"

-- | Changes the value at a given store index
update :: ValueType value => Store value -> StoreIndex -> value -> Store value
update (top,st) si val = (top,st//[(si,val)])


--------------------------

-- | A scoped environment for variable-store index bindings.
-- It allows creating and deleting scopes as required by the DIPL semantics.
-- A scoped environment maintains a list of environments, where the leftmost list element is the topmost environment.
-- An environment is for simplicity just an association list of variable name-store index pairs,
-- where the leftmost pair is the most resent addition to the environment.
-- There are two groups of primitive functions, one for environments and one for scoped environments.
-- Environemnt functions:
-- • Create an empty start environment.
-- • Look up a variable name in the environment, returning the store index if its there, the error index if not.
-- • Convenience function that adds a new value to the store,
--   then places the corresponding variable-value binding first in the environment.
-- Scoped environemnt functions:
-- • Start scoped environment which contains one empty environment.
-- • Adding a new, empty, topmost scope, i.e., place a new environment first in the list of environments.
-- • Deleting the topmost scope.
-- • Convenience function the looks up a variable in successive environments from the top,
--   returning the store index of the first found occurance of the variable.
--   If there is no entry for the vairable name in any of the scoped environments, the lookup fails with an error exception.


-- | An environemnt is an association list of variable names and store indices.
type Environment = [(String,StoreIndex)]
newenvironment :: Environment
newenvironment = []

-- | Add a data value to the store and add a variable to the environment referring to the recently stored value.
newvarval :: ValueType value => (Environment, Store value) -> String -> value -> (Environment, Store value)
newvarval (env,st) str val = (newenv,newst)
  where
    newst = extend st val
    newenv = (str,getnewestindex newst):env

-- | Look up the store index of a variable name in an environment.
-- If the variable is not registered, return storeerrorindex
findenv :: String -> Environment -> StoreIndex
findenv str ((vname,sti):env) = 
  if str == vname then sti else findenv str env
findenv str [] = storeerrorindex


-- | A scoped environment is a list of environments.
-- The newest (topmost) environment is the first element of the list.
type ScopedEnvironment = [Environment]

newscopedenvironment :: ScopedEnvironment
newscopedenvironment = [newenvironment]

-- | Add a new, empty topmost scope to the environment.
newscope :: ScopedEnvironment -> ScopedEnvironment
newscope senv = newenvironment:senv

-- | Delete the topmost scoped environment.
delscope :: ScopedEnvironment -> ScopedEnvironment
delscope (env:senv) = senv
delscope [] = error $ "Trying to delete the empty scope of environments."

-- | Look up the store index of a variable name in a scoped environment.
-- Looking up a nonexistent variable name is an error..
scopedfind :: String -> ScopedEnvironment -> StoreIndex
scopedfind str (env:senv) = if res == storeerrorindex then scopedfind str senv else res
  where
    res = findenv str env
scopedfind str [] = error $ "No such variable name: " ++ str



--------------------------

-- | Unit tests for the State implementation, including some supporting data structures.

newtype Exampledstateinteger = Exampledstatevalue Integer
  deriving (Eq,Show)
instance ValueType Exampledstateinteger where
  truthvalue (Exampledstatevalue i) = 0 < i

testStateexampleinteger = do
  print $ "-- Testing the State implementation."
  let state0 = (startstate :: State Exampledstateinteger)
  let state1 = addscope state0
  let state2 = addvariable state1 "x0" (Exampledstatevalue 0)
  let state3 = addvariable state2 "x1" (Exampledstatevalue 1)
  let state4 = addscope state3
  let state5 = addvariable state4 "y2" (Exampledstatevalue 2)
  let state6 = deletescope state5
  print $ (    ([[]],(0,array (0,-1) [])) == state0 )
       && ( ([[],[]],(0,array (0,-1) [])) == state1 )
       && ( ([[("x0",0)],[]],(1,array (0,0) [(0,Exampledstatevalue 0)])) == state2 )
       && (    ([[("x1",1),("x0",0)],[]],(2,array (0,1) [(0,Exampledstatevalue 0),(1,Exampledstatevalue 1)])) == state3 )
       && ( ([[],[("x1",1),("x0",0)],[]],(2,array (0,1) [(0,Exampledstatevalue 0),(1,Exampledstatevalue 1)])) == state4 )
       && ( ([[("y2",2)],[("x1",1),("x0",0)],[]],
            (3,array (0,2) [(0,Exampledstatevalue 0),(1,Exampledstatevalue 1),(2,Exampledstatevalue 2)])) == state5 )
       && ( [Exampledstatevalue 0,Exampledstatevalue 1,Exampledstatevalue 2] == (map (getvalue state5) ["x0", "x1", "y2"]) )
       && ( ([[("x1",1),("x0",0)],[]],
            (3,array (0,2) [(0,Exampledstatevalue 0),(1,Exampledstatevalue 1),(2,Exampledstatevalue 2)])) == state6 )
       && ( ([("V0",0)],(1,array (0,0) [(0,Exampledstatevalue 0)]))
            == (newvarval (newenvironment,newstore :: Store Exampledstateinteger) "V0" (Exampledstatevalue 0)) )
       && ( ([("V1",1),("V0",0)],(2,array (0,1) [(0,Exampledstatevalue 0),(1,Exampledstatevalue 1)]))
            == (newvarval
                 (newvarval (newenvironment,newstore :: Store Exampledstateinteger) 
                  "V0" (Exampledstatevalue 0)
                 ) 
                "V1" (Exampledstatevalue 1)
               ) 
          )    
  --
  -- The following should crash since "y2" no longer is a registered variable
  -- print $ map (getvalue state6) ["x0", "x1", "y2"]
  --

