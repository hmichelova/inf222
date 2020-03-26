-- | Defines a grammar driven language acceptor and parser.
-- The grammar is a choice of start symbol and a list of rules defining context-free languages.
-- No attempt is made to give nice warning messages if the grammar is inconsistent or an accept/parse fails.
-- Also provides some simple utility functions to view the contents of a CST as ASCII graphics tree or a string.
--
-- Authors: Magne Haveraaen & Jaakko Järvi
-- Since 2020-02-12

module ParserGenerator where

-- | Use Haskell's general tree library
-- This defines "data Tree a = Node a [Tree a]" and many utility functions.
import Data.Tree (Tree(..), drawTree)


------------------------------

-- | General definition of a context free grammar as Haskell code.
-- We do not declare which terminals and nonterminals are in use as grammar theory defines.
-- Instead we use the actual strings used to represent terminals and nonterminals as the ones declared.
-- This has the possible drawback that a misspelling dramatically may change the intended grammar in unexpected ways.


-- | A grammar defines a start symbol and rules.
type Grammar = (String,Rules)
-- | A Rule is a nonterminal symbol (String) and a list of Grammar symbols.
type Rule = (Nonterminal, [GSymbol])
type Rules = [Rule]
-- | A grammar symbol is either a terminal (T String) or a nonterminal (N String).
data GSymbol = T Terminal | N Nonterminal deriving Eq
type Terminal = String
type Nonterminal = String

-- | Grammar symbols are displayed BNF style, i.e. a nonterminal is a string in angular brackets and a terminal is a string.
instance Show GSymbol where
  show (T s) = s
  show (N s) = "<" ++ s ++ ">"



------------------------------

-- | A lexer takes a string and splits it into a list of tokens.
type Lexer = String -> [Token]

-- | An unlexer takes a list of tokens back to a string.
-- Note that  "unlexer (lexer str)" may not always yield back the same source string.
type Unlexer = [Token] -> String

-- | Property that retockenising a string from an unlexer should be the identity
lexerProperty :: [Token] -> Bool
lexerProperty tokens = lexer (unlexer tokens) == tokens
-- | It is important for lexerProperty that the token list is a result of using the proper lexer.
lexerTest :: String -> Bool
lexerTest str = lexerProperty (lexer str)


-- | A token here is just a string.
type Token = String

-- | Implementation of a lexer that splits strings on spaces.
lexer :: Lexer
lexer str = words str

-- | Unlexer goes from a list of tokens to a string
unlexer :: Unlexer
unlexer tokens = unwords tokens


------------------------------

-- | An acceptor takes a grammar and a string, and tells if the string is in the language defined by the grammar.
-- Note that the input string is split into a list of strings by a "lexer" function.
accept :: Grammar -> String -> Bool
accept (start,rules) str = steps rules [N start] (lexer str)


-- | A more general acceptor takes a grammar, a lexer and a string, and tells if the string is in the language defined by the grammar.
-- Note that the input string is split into a list of strings by a "lexer" function given as parameter.
glaccept :: Grammar -> Lexer -> String -> Bool
glaccept (start,rules) lexer str = steps rules [N start] (lexer str)



-- | The call "steps rules gsyms istrs" checks if the list of grammar symbols gsyms can generate the list istrs.
steps :: Rules -> [GSymbol] -> [String] -> Bool

-- Both gsyms and istrs are empty: grammar accepts string
steps _ [] [] = True
-- If the first gsymbol is a terminal, then the first istr must be that same terminal.
steps rules (T t:gsyms) (t':istrs) | t == t' = steps rules gsyms istrs
-- If the first gsymbol is a nonterminal N, then replace that nonterminal with all possible right-hand side of rules N->rhs.
-- For instance, if the rules contain N->ab, N->aN, and the gsymbols is NxyC, then we try gsymbols abxyC, aNxyC in turn.
steps rules (N n:gsyms) istrs =
  or (map (\rhs -> steps rules (rhs++gsyms) istrs) rhss)
  where
    rhss = [ rhs | (n', rhs) <- rules, n == n']
-- Reject if none of the cases above worked out.
steps _ _ _ = False



------------------------------

-- | The parser creates a concrete syntax tree according to the production rules of the grammar that were used to parse the input string.
-- If the grammar is ambigous, i.e., more than one set of rules could generate the same string, the parser chooses one of those possibilities.
-- Care should be taken to formulate the grammar such that parsing terminates (avoid certain kinds of circularities), and ambiguities,
-- e.g., arising from lack of presedence of operators or prefix constructs ("dangling else" problem).


-- | The concrete syntax tree (CST) data structure.
-- It contains terminal symbols and defines nesting by expansion of nonterminals.
type CST = Tree GSymbol

-- | The parser return type, the unparsed suffix string list and the CST of the parsed prefix.
type Parse = Maybe ([String], CST)


-- | The parser takes the grammar, the input string and returns the CST from the start symbol, and any remaining unparsed part of the input
-- Since parsing may fail, the result is encapsulated by the Maybe monad.
-- Note that the input string is split into a list of strings by the "lexer" function.
parse :: Grammar -> String -> Parse
-- Parcing starts with the start symbol given by the grammar, and selects relevant rules for deciding the shape of the CST.
parse (start,rules) str = tree rules (N start) (lexer str)

-- | A more general parser which takes the grammar, the lexer, and the input string and returns the CST from the start symbol, and any remaining unparsed part of the input
-- Since parsing may fail, the result is encapsulated by the Maybe monad.
-- Note that the input string is split into a list of strings by the "lexer" function.
glparse :: Grammar -> Lexer -> String -> Parse
glparse (start,rules) lexer str = tree rules (N start) (lexer str)



-- | The tree function takes rules, a gsymbol and a list of input strings.
-- If a prefix of the input strings is reachable from the gsymbol (according to the rules), the tree function returns the remainder of the input and the CST for the prefix.
tree :: Rules -> GSymbol -> [String] -> Parse
-- If the gsymbol is a terminal, then the first istr must be that same terminal.
tree _ (T t) (i:istrs) | t == i = Just (istrs, Node (T t) [])
-- If the gsymbol is a nonterminal, then apply the tryrule function to each of the rules of the grammar, returning the first successful parse.
tree rules (N n) istrs = firstJust (map tryrule rules)
  where
    -- In the context of istrs and nonterminal n, for a given rule, if the lhs of the rule matches n, then attempt to parse the input strings 
    tryrule :: Rule -> Parse
    tryrule (n', rhs) = do
      (istrs', csts) <- if (n==n') then trees rules rhs istrs else Nothing
      return (istrs', Node (N n) csts)
-- Parsing failed, return Nothing, thus no error message explaining the problem either.
tree _ _ _ = Nothing


-- | The trees function takes rules, a list of gsymbols and a list of input strings.
-- It gobbles up the prefix of the input strings reachable from the gsymbols (neck to neck), returning the remainder of the input and the list of CSTs from the gsymbols.
trees :: Rules -> [GSymbol] -> [String] -> Maybe ([String], [CST])
trees _ [] istrs = return (istrs, [])
trees rules (gsym:gsyms) istrs = do
  -- Parse the prefix of the input strings according to the first gsymbol.
  (istrs', cst) <- tree rules gsym istrs
  -- Parse the remaining input strings according to the remaining gsymbols.
  (istrs'', csts) <- trees rules gsyms istrs'
  -- Create the Just value of the unparsed suffix of the input strings and the sequence of parsed CSTs.
  return (istrs'', cst:csts)


-- | Extract the first "Just a" from a list of "Maybe a", return "Nothing" if there is no proper value in the list.
firstJust [] = Nothing
firstJust (Just v:_) = Just v
{- Does not achieve intended effect
firstJust ((Just v@(strs,cst)):ms) =
  if strs == [] then Just v else if rest == Nothing then Just v else rest
    where rest = firstJust (ms)
-}
firstJust (_:ms) = firstJust ms


------------------------------

-- | Support tools for extracting and visualising concrete parse trees from the parsed Maybe construct.

-- | Extract the CST, ignore unparsed suffix, i.e., there is extra text following the parsed bit of the input text.
extractCSTweak :: Parse -> CST
extractCSTweak Nothing = error "No parse"
extractCSTweak (Just ([], cst)) = cst
extractCSTweak (Just (strs, cst)) = cst

-- | Extract the CST, stop with an error message if the unparsed suffix string list is non-empty. 
extractCSTstrict :: Parse -> CST
extractCSTstrict Nothing = error "No parse"
extractCSTstrict (Just ([], cst)) = cst
extractCSTstrict (Just (strs, _)) = error $ "Unparsed suffix: " ++ (unlexer strs)

-- | Display the CST as a tree, i.e., a Data.Tree ASCII graphics.
-- A warning message is provided if there is an unparsed suffix of the input remaining.
displayParseResult :: Parse -> IO ()
displayParseResult Nothing = putStrLn "No parse"
displayParseResult (Just ([], cst)) = displaytree cst
displayParseResult (Just (strs, cst)) = do
  displaytree cst
  putStrLn $ "Unparsed suffix: " ++ (unlexer strs)


------------------------------

-- | Support tools for visualising concrete parse trees.

-- | Presents a concrete parse tree as ASCII graphics.
displaytree :: Show a => Tree a -> IO ()
displaytree cst = putStrLn $ drawTree (fmap show cst)

-- | Traverse a concrete parse tree and concatenate the terminals in order, giving back a parsable string.
printtree :: CST -> String
printtree (Node (T str) []) = str
printtree (Node (N str) []) = ""
printtree (Node (T str) (e:es)) = str ++ " " ++ printtree e ++ " " ++ printtree (Node (N "temp") es)
printtree (Node (N str) (e:es)) = printtree e ++ " " ++ printtree (Node (N "temp") es)


-- | Printing a parse tree and reparsing it should yield the same parse tree.
-- Note that combining the other way, i.e., going from a string to a parse tree and back to a string, may yield a different string.
printtreeProperty :: Grammar -> CST -> Bool
printtreeProperty grammar cst = extractCSTstrict (parse grammar (printtree cst)) == cst
-- | It is important for printtreeProperty that the CST is a result of parsing with the chosen grammar.
printtreeTest :: Grammar -> String -> Bool
printtreeTest grammar str = if accept grammar str then printtreeProperty grammar (extractCSTstrict (parse grammar str)) else True


------------------------------

-- | Refactoring tools for grammars and concrete parse trees.
-- Tools include:
-- Going from a list to repeated use of pairs
-- Changing a left recursive grammar to a safe grammar.
-- Providing priority and associativity for operators.
-- Dealing with dangling else problem: 
-- here it is just rearranging the grammar so that the longer rule is tried before the prefix rule: this gives priority to the inner binding of the long form.



------------------------------

-- | Support tools for renaming symbols in grammars and concrete parse trees.
-- This can be useful for keeping symbols separate when combining grammars and controlling the overlap.

-- | Traverses a concrete parse tree and replaces the gsymbols.
-- A call "replacetree cst gs1 gs2" creates a new parse tree where gs1 is replaced by gs2.
replacetree :: CST -> GSymbol -> GSymbol -> CST
replacetree cst gs1@(T str1) gs2@(N str2) = error $ "Syntax rule terminal/nonterminal mismatch: " ++ (show gs1) ++ " " ++ (show gs2)
replacetree cst gs1@(N str1) gs2@(T str2) = error $ "Syntax rule terminal/nonterminal mismatch: " ++ (show gs1) ++ " " ++ (show gs2)
replacetree (Node (T str) []) (T str1) (T str2) = if str == str1 then (Node (T str2) []) else (Node (T str) [])
replacetree (Node (T str) (cst:csts)) _ _ = error $ "Terminal symbol has children: " ++ (show $ cst:csts)
replacetree (Node (N str) csts) (N str1) (N str2) =
  if str == str1 then (Node (N str2) (replacetrees csts (N str1) (N str2))) else (Node (N str) (replacetrees csts (N str1) (N str2)))
replacetree (Node gsym csts) gs1 gs2 = (Node gsym (map (\cst -> replacetree cst gs1 gs2) csts))

-- | Replace gsymbols in a list of concrete parse tree.
replacetrees :: [CST] -> GSymbol -> GSymbol -> [CST]
replacetrees [] _ _ = []
replacetrees (cst:csts) gs1 gs2 = replacetree cst gs1 gs2 : replacetrees csts gs1 gs2

-- | Replace gsymbols in a rule list. 
-- NB! Will only replace terminals by terminals and non-terminal by non-terminals.
replacerules :: Rules -> GSymbol -> GSymbol -> Rules
replacerules rules gs1@(T str1) gs2@(N str2) = error $ "Syntax rule terminal/nonterminal mismatch: " ++ (show gs1) ++ " " ++ (show gs2)
replacerules rules gs1@(N str1) gs2@(T str2) = error $ "Syntax rule terminal/nonterminal mismatch: " ++ (show gs1) ++ " " ++ (show gs2)
replacerules [] _ _ = []
replacerules ((nt,gsyms):rules) gs1@(T str1) gs2@(T str2) =
  (nt,map (\g -> case g of {(T str) -> (T (if str == str1 then str2 else str)) ; _ -> g}) (gsyms)) : replacerules rules gs1 gs2
replacerules ((nt,gsyms):rules) gs1@(N str1) gs2@(N str2) =
  (if nt == str1 then str2 else nt,
   map (\g -> case g of { (N str) -> (N (if str == str1 then str2 else str)); _ -> g}) (gsyms)
  ) : replacerules rules gs1 gs2
 

-- | The union of two grammars with the new start symbol given as first parameter.
grammarunion :: String -> Grammar -> Grammar -> Grammar
grammarunion str (n1,rules1) (n2,rules2) =
  (str,(str,[N n1]):((str,[N n2]):rules1)++rules2)

  
------------------------------

-- | Print nice message rather than just True / False
ok True = putStrLn "OK"
ok False = putStrLn "*** TEST FAILED ***"


