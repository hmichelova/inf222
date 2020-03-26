-- | Grammar for identifiers and literals (integer and string).
-- Identifiers follow the normal letter followed by zero or more letter/digit sequence, where letters are lower/upper case Norwegian alfabet.
-- Integer literals are of the form 0|(1|2|3|4|5|6|7|8|9)(0|1|2|3|4|5|6|7|8|9)* and interpreted as decimals. Negative numbers are not included.
-- String literals are surrounded by " and " (no escaping included, thus no way to include " in the string). 
-- It accepts large portion of unicode (including many emojis) according to Haskell notation, but this creates huge grammars.
-- Lowering maxstringliteralchar to a few thousand will dramatically lower the size of the string literal grammar.
--
-- Author: Magne Haveraaen
-- Since: 2020-02-19
module IdentifierLiteralGrammar where

-- | Use the parser generator
import ParserGenerator
-- | Use Haskell's general tree library
import Data.Tree (Tree(..), drawTree)


------------------------------

-- | Grammar for identifiers: An identifier is a letter followed by zero or more letters or digits.
-- Due to the simple lexer which splits code at spaces, identifiers need to be written with spaces in the source code.
-- More advanced tokenisers may allow normal writing of identifiers.

-- | An expression that computes the production rules for identifiers.
-- The rules are of the form:
--   identifier ::= letter identsuffix
--   identsuffix ::= letterORdigit identsuffix
--   identsuffix ::= ε
identifierrules =
  singlecharacteridentifier
  ++
  singlecharacteridentsuffix
  ++
  [
    ("identsuffix", [])
  ] :: Rules
singlecharacteridentifier = [ ("identifier", [T (char:[]), N "identsuffix"]) | char <- identifierletter ]
identifierletter = ['a'..'z']++['A'..'Z']++"æøåÆØÅ"
singlecharacteridentsuffix = [ ("identsuffix", [T (char:[]), N "identsuffix"]) | char <- identifierletterORdigit ]
identifierletterORdigit = identifierletter ++ ['0'..'9']

-- | Grammar for identifiers, start symbol is "identifier".
identifiergrammar = ("identifier",identifierrules)

-- | Translater from identifier CST to String by concatenating the characters of the identifier in order.
identifiertostring :: CST -> String
identifiertostring (Node (N "identifier") [ch,chs]) = identifiertostring ch ++ identifiertostring chs
identifiertostring (Node (N "identsuffix") [ch,chs]) = identifiertostring ch ++ identifiertostring chs
identifiertostring (Node (N "identsuffix") []) = []
identifiertostring (Node (T (c:[])) []) = c:[]


------------------------------

-- | A integer literal is one or more decimal digits, such that if the first digit is 0 this is the only digit.
-- Due to our simple lexer which splits code at spaces, 
-- integer literals need to be written with spaces separating each character in the source code.

-- | The production rules for integers written as decimal digits.
numberrules =
  [
    ("number", [T "0"])
  , ("number", [T "1", N "digits"])
  , ("number", [T "2", N "digits"])
  , ("number", [T "3", N "digits"])
  , ("number", [T "4", N "digits"])
  , ("number", [T "5", N "digits"])
  , ("number", [T "6", N "digits"])
  , ("number", [T "7", N "digits"])
  , ("number", [T "8", N "digits"])
  , ("number", [T "9", N "digits"])
  , ("digits", [T "0", N "digits"])
  , ("digits", [T "1", N "digits"])
  , ("digits", [T "2", N "digits"])
  , ("digits", [T "3", N "digits"])
  , ("digits", [T "4", N "digits"])
  , ("digits", [T "5", N "digits"])
  , ("digits", [T "6", N "digits"])
  , ("digits", [T "7", N "digits"])
  , ("digits", [T "8", N "digits"])
  , ("digits", [T "9", N "digits"])
  , ("digits", [])
  ] :: Rules

-- | Grammar for integer literals, start symbol is "number".
numbergrammar = ("number",numberrules)

-- | Translator from number CST to Integer.
numbertointeger :: CST -> Integer
numbertointeger cst = integerlisttointeger $ reverse $ numbertointegerlist cst

-- | Translator from a list of digits (in reverse order) to an integer.
integerlisttointeger :: [Integer] -> Integer
integerlisttointeger (i:[]) = i
integerlisttointeger (i:is) = integerlisttointeger is * 10 + i

-- | Translator from number CST to list of integer digits (in order).
numbertointegerlist :: CST -> [Integer]
numbertointegerlist (Node (N "number") [no]) = numbertointegerlist no
numbertointegerlist (Node (N "number") [no,digs]) = numbertointegerlist no ++ numbertointegerlist digs
numbertointegerlist (Node (N "digits") [no,digs]) = numbertointegerlist no ++ numbertointegerlist digs
numbertointegerlist (Node (N "digits") []) = []
numbertointegerlist (Node (T "0") []) = 0:[]
numbertointegerlist (Node (T "1") []) = 1:[]
numbertointegerlist (Node (T "2") []) = 2:[]
numbertointegerlist (Node (T "3") []) = 3:[]
numbertointegerlist (Node (T "4") []) = 4:[]
numbertointegerlist (Node (T "5") []) = 5:[]
numbertointegerlist (Node (T "6") []) = 6:[]
numbertointegerlist (Node (T "7") []) = 7:[]
numbertointegerlist (Node (T "8") []) = 8:[]
numbertointegerlist (Node (T "9") []) = 9:[]


------------------------------

-- | Treatment of string literals, which are a string of characters surrounded by " (and no escape for ").
-- Due to our simple lexer which splits code at spaces, 
-- string literals need to be written with spaces separating each character in the source code.

-- Here is a program that computes all the production rules for strings.
-- | An expression that computes the production rules for identifiers.
-- The rules are of the form:
--   string ::= " characters
--   characters ::= stringcharacter characters
--   characters ::= "
-- A string starts and ends with " (which are not part of the string) and has letters, and special symbols.
stringrules =
  [ ("string", [T "\"", N "characters"]) 
  ]
  ++
  singlestringcharacter
  ++
  [ ("characters", [T "\""])
  ] :: Rules
-- | Production rules for individual characters from space up to and including maxstringliteralchar, without ".
singlestringcharacter = [ ("characters", [T (c:[]), N "characters"]) | c <- [' '..maxstringliteralchar], c /= '"']
maxstringliteralchar = '\150000'

-- | Grammar for string literals, start symbol is "string".
stringgrammar = ("string",stringrules)

-- | Translater from string literal CST to String.
stringtostring :: CST -> String
stringtostring (Node (N "string") [Node (T "\"")[],chs]) = stringtostring chs
stringtostring (Node (N "characters") [ch,chs]) = stringtostring ch ++ stringtostring chs
stringtostring (Node (N "characters") [Node (T "\"")[]]) = ""
stringtostring (Node (T (c:[])) []) = c:[]



------------------------------

-- | test programs.
testliteral = do
  print $ "-- Parse some identifiers "
  ok $ ("Æ" == (identifiertostring $ extractCSTstrict $ parse identifiergrammar "Æ") )
    && ("yYu7i8" == (identifiertostring $ extractCSTstrict $ parse identifiergrammar "y Y u 7 i 8") )
  print $ "-- Parse some numbers "
  ok $ (0 == (numbertointeger $ extractCSTstrict $ parse numbergrammar "0") )
    && (13489760203 == (numbertointeger $ extractCSTstrict $ parse numbergrammar "1 3 4 8 9 7 6 0 2 0 3") )
  print $ "-- Parse some strings "
  ok $ ("0" == (stringtostring $ extractCSTstrict $ parse stringgrammar "\" 0 \"") )
    && ("13489760203yYu7i8" == (stringtostring $ extractCSTstrict $ parse stringgrammar "\" 1 3 4 8 9 7 6 0 2 0 3 y Y u 7 i 8 \"") )
    && ("a1\230p89a\8364\223\128545\128569\8505\8465" == (stringtostring $ extractCSTstrict $ parse stringgrammar "\" a 1 æ p 8 9 a € ß 😡 😹 ℹ ℑ \"") )


