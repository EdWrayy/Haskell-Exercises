-- Author: EdwardWray
-- Copyright (c) 2024, University of Southampton

-- Your imports here
import Data.Maybe (fromJust, isNothing)
import Debug.Trace (trace)
import Control.Applicative
import Data.Char
import Distribution.Compat.Lens (_1)
import Data.List (nub, isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set




--Hutton's Parsing Module
newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

--End of Hutton's Parsing Module



-- DO NOT MODIFY THESE DATA TYPES
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)


-- Parses a string into a LamMacroExpr if valid
parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro input =
    case parse lamMacroExprParser (removeWhitespace input) of
        [(result, "")] -> Just result
        _ -> Nothing


-- Removes all whitespace from a string
removeWhitespace :: String -> String
removeWhitespace = filter (not . isSpace)


-- Parser for LamMacroExpr, which includes macro definitions and an expression
lamMacroExprParser :: Parser LamMacroExpr
lamMacroExprParser = do
    defs <- many lamDefParser 
    expr <- lamExprParser defs
    if hasDuplicateNames defs || not (allMacrosClosed defs)
        then empty
    else return (LamDef defs expr)


-- Checks if macro definitions have duplicate names
hasDuplicateNames :: [(String, LamExpr)] -> Bool
hasDuplicateNames defs = length (nub names) /= length names
    where names = map fst defs


-- Checks if all macro definitions are closed (no free variables)
allMacrosClosed :: [(String, LamExpr)] -> Bool
allMacrosClosed = all (isClosed . snd)


-- Checks if a single term is closed (no free variables)
isClosed :: LamExpr -> Bool
isClosed = null . freeVars Set.empty


-- Calculates the free variables in a lambda expression
freeVars :: Set Int -> LamExpr -> Set Int
freeVars boundVarSet (LamVar n) = if n `Set.member` boundVarSet then Set.empty else Set.singleton n
freeVars boundVarSet (LamAbs n expr) = freeVars (Set.insert n boundVarSet) expr
freeVars boundVarSet (LamApp e1 e2) = freeVars boundVarSet e1 `Set.union` freeVars boundVarSet e2
freeVars boundVarSet (LamMacro _) = Set.empty


-- Parses a single macro definition
lamDefParser :: Parser (String, LamExpr)
lamDefParser = do
    _    <- string "def"
    name <- macroName
    _    <- char '='
    expr <- lamExprParser []
    _    <- string "in"
    return (name, expr)


--Parses multiple uppercase characters for macro names
macroName :: Parser String
macroName = some upper


-- Parses a general lambda expression
lamExprParser :: [(String, LamExpr)] -> Parser LamExpr
lamExprParser defs = lamAppParser defs <|> lamAtomicExpr defs

-- Parses an atomic lambda expression
lamAtomicExpr :: [(String, LamExpr)] -> Parser LamExpr
lamAtomicExpr defs = lamMacroParser defs <|>  lamVarParser <|> lamAbsParser defs <|> parenthesizedExpr defs
    

-- Parses a parenthesized lambda expression
parenthesizedExpr :: [(String, LamExpr)] -> Parser LamExpr
parenthesizedExpr defs = do
    char '('
    expr <- lamExprParser defs
    char ')'
    return expr


-- Parses a macro name or multiple if they are consecutive
lamMacroParser :: [(String, LamExpr)] -> Parser LamExpr
lamMacroParser defs = do
    s <- some upper
    case splitMacros defs s of
        Just names -> return $ foldl1 LamApp (map LamMacro names)
        Nothing -> return (LamMacro s)  -- If can't split, just use the whole string


--Attempts to split a macro string into substrings of macro definitions
splitMacros :: [(String, LamExpr)] -> String -> Maybe [String]
splitMacros defs str = go str []
  where
    go "" acc = Just (reverse acc)
    go str acc = 
        case [name | name <- map fst defs, isPrefixOf name str] of
            [] -> Nothing
            names -> case maximum names of
                name -> go (drop (length name) str) (name:acc)

-- Parses a lambda application
lamAppParser :: [(String, LamExpr)] -> Parser LamExpr
lamAppParser defs = do
    terms <- some (lamAtomicExpr defs)
    return $ foldl1 LamApp terms


-- Parses a lambda abstraction
lamAbsParser :: [(String, LamExpr)] -> Parser LamExpr
lamAbsParser defs = do
    lambda <- char 'λ'
    _      <- char 'x'
    n      <- int
    _      <- string "→"
    body   <- lamExprParser defs
    return(LamAbs n body)


-- Parses variables: x<n>
lamVarParser :: Parser LamExpr
lamVarParser = do
    _ <- char 'x'
    n <- int
    return (LamVar n)




