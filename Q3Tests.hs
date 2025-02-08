-- Author: EdwardWray
-- Copyright (c) 2024, University of Southampton

-- Your imports here
import Data.List (isInfixOf)
import qualified Data.Text as T
import Debug.Trace (trace)


assertEqual :: (Eq a, Ord a, Show a) => String -> [a] -> [a] -> IO ()
assertEqual testName expected actual =
    if expected == actual
        then putStrLn $ testName ++ " passed."
        else putStrLn $ testName ++ " failed. Expected: " ++ show expected ++ ", but got: " ++ show actual


testQuestion3::IO()
testQuestion3 = do
    question3Test1
    question3Test2
    question3Test3
    question3Test4
    question3Test5
    question3Test6
    question3Test7


--Simple variable
question3Test1 :: IO ()
question3Test1 = do
    let unparsedExpression = LamDef [] (LamVar 0)
    let expected = "x0"
    let result = unparse unparsedExpression
    assertEqual "Test 1: Simple variable - " expected result


--Lambda Abstraction
question3Test2 :: IO ()
question3Test2 = do
    let unparsedExpression = LamDef [] (LamAbs 1 (LamVar 1))
    let expected = "\955x1\8594x1"
    let result = unparse unparsedExpression
    assertEqual "Test 2: Simple Lambda Abstraction - " expected result

--Lambda Application
question3Test3 :: IO ()
question3Test3 = do
    let unparsedExpression = LamDef [] (LamApp (LamVar 1)(LamAbs 1 (LamVar 1)))
    let expected = "x1\955x1\8594x1"
    let result = unparse unparsedExpression
    assertEqual "Test 3: Simple Lambda Application - " expected result


--Bracketed Application
question3Test4 :: IO ()
question3Test4 = do
    let unparsedExpression = LamDef [] (LamApp (LamAbs 1 (LamVar 1))(LamVar 1))
    let expected = "(\955x1\8594x1)x1"
    let result = unparse unparsedExpression
    assertEqual "Test 4: Bracketed Lambda Application - " expected result


--Singular macro definition
question3Test5 :: IO ()
question3Test5 = do
    let unparsedExpression = LamDef [("FST", LamAbs 1 (LamAbs 2 (LamVar 1)))] (LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamAbs 1 (LamVar 1)) (LamVar 4)))
    let expected = "defFST=\955x1\8594\955x2\8594x1inFSTx3(\955x1\8594x1)x4"
    let result = unparse unparsedExpression
    assertEqual "Test 5: Macro Definition of FST   - " expected result


--Multiple macro definitions
question3Test6 :: IO ()
question3Test6 = do
    let unparsedExpression = LamDef [ ("ID",LamAbs 1 (LamVar 1)) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))) ) (LamMacro "ID") )
    let expected = "defID=\955x1\8594x1indefSND=\955x1\8594\955x2\8594x2inSND(\955x1\8594x1x1)\955x1\8594x1x1ID"
    let result = unparse unparsedExpression
    assertEqual "Test 6 : ID and SND Macros   - " expected result


-- Multiple Consecutive Macro Expression
question3Test7 :: IO ()
question3Test7 = do
    let unparsedExpression = LamDef [("ID",LamAbs 1 (LamVar 1)) , ("DUP",LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) ] (LamApp (LamApp (LamMacro "ID") (LamMacro "DUP")) (LamVar 1))
    let expected = "defID=λx1→x1indefDUP=λx1→x1x1inIDDUPx1"
    let result = unparse unparsedExpression
    assertEqual "Test 7: Multiple Consecutive Macro Expression - " expected result





--Code from Question 3

-- DO NOT MODIFY THESE DATATYPES
-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-- Converts a LamMacroExpr into a readable string
unparse :: LamMacroExpr -> String
unparse (LamDef macros expression) =
    let
        macroDefinitions = concatMap unparseMacroDefinitions macros
        mainExpression = unparseWithMacros macros expression
    in
        macroDefinitions ++ mainExpression

-- Converts a macro definition into a string representation
unparseMacroDefinitions :: (String, LamExpr) -> String
unparseMacroDefinitions (name, expression) = "def" ++ name ++ "=" ++ unparseExpression expression ++ "in"

unparseWithMacros :: [(String, LamExpr)] -> LamExpr -> String
unparseWithMacros macros expr = unparseExpression (substituteMacros macros expr)

-- Converts a LamExpr into a string representation
unparseExpression :: LamExpr -> String
unparseExpression (LamMacro name)    = name
unparseExpression (LamApp (LamAbs param e1) e2)     = "(" ++ unparseExpression (LamAbs param e1) ++ ")" ++ unparseExpression e2 -- Brackets necessary for left-side lambda abstraction
unparseExpression (LamApp e1 e2) = unparseExpression e1 ++ unparseExpression e2
unparseExpression (LamAbs param e)   = "\955" ++ "x" ++ show param ++ "\8594" ++ unparseExpression e
unparseExpression (LamVar v)         = "x" ++ show v



-- Substitute macros in the expression tree
substituteMacros :: [(String, LamExpr)] -> LamExpr -> LamExpr
substituteMacros macros expr =
    -- 1) Recursively transform child nodes first:
    let newExpr = case expr of
          LamVar v       -> LamVar v
          LamAbs param e -> LamAbs param (substituteMacros macros e)
          LamApp e1 e2   -> LamApp (substituteMacros macros e1)
                                   (substituteMacros macros e2)
          LamMacro m     -> LamMacro m    -- Keep an existing LamMacro as is
    in
    -- 2) Now see if newExpr as a whole matches any macro definition
    case firstMacroMatch macros newExpr of
        Just macroName -> LamMacro macroName
        Nothing        -> newExpr
  where
    -- Look up the first macro whose definition equals `expr`.
    firstMacroMatch [] _ = Nothing
    firstMacroMatch ((name, def):rest) candidate =
      if def == candidate
         then Just name
         else firstMacroMatch rest candidate