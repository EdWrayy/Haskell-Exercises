-- Author: Edward Wray
-- Copyright (c) 2024, University of Southampton
--How to use: In order to run these tests, first load the question being tested into ghci, then copy and paste the code for the relevant section using :{ }: for multiline pasting
--The main function testQuestionX where X is the number question can then be called from the terminal to run all the relevant tests at once


------------------------------------ Tests for Question 1 ------------------------------------


--Implementing a custom assertEqual function for testing
assertEqual :: (Eq a, Ord a, Show a) => String -> [a] -> [a] -> IO ()
assertEqual testName expected actual =
    if sort expected == sort actual
        then putStrLn $ testName ++ " passed."
        else putStrLn $ testName ++ " failed. Expected: " ++ show (sort expected) ++ ", but got: " ++ show (sort actual)



testQuestion1::IO()
testQuestion1 = do
    question1Test1
    question1Test2
    question1Test3
    question1Test4

-- Test case 1: Basic functionality with no atoms
question1Test1 :: IO ()
question1Test1 = do
    let gridSize = 3
    let atoms = [] -- No atoms
    let result = calcInteractions gridSize atoms
    let expected = [(EP East 1 L,EP West 1 R),(EP East 1 R,EP South 1 L),(EP East 2 L,EP West 2 R),(EP East 2 R,EP South 2 L),(EP East 3 L,EP West 3 R),(EP East 3 R,EP South 3 L),(EP West 1 L,EP South 3 R),(EP West 1 R,EP East 1 L),(EP West 2 L,EP South 2 R),(EP West 2 R,EP East 2 L),(EP West 3 L,EP South 1 R),(EP West 3 R,EP East 3 L),(EP South 1 L,EP East 1 R),(EP South 1 R,EP West 3 L),(EP South 2 L,EP East 2 R),(EP South 2 R,EP West 2 L),(EP South 3 L,EP East 3 R),(EP South 3 R,EP West 1 L)]
    assertEqual "Test 1: No atoms - " expected result



-- Test case 2: Single atom functionality
question1Test2 :: IO ()
question1Test2 = do
    let gridSize = 3
    let atoms = [(1,1)] -- Singular atom
    let result = calcInteractions gridSize atoms
    let expected = [(EP East 1 L,EP East 1 R),(EP East 1 R,EP East 1 L),(EP East 2 L,EP West 2 R),(EP East 2 R,EP South 2 L),(EP East 3 L,EP West 3 R),(EP East 3 R,EP South 3 L),(EP West 1 L,EP West 1 R),(EP West 1 R,EP West 1 L),(EP West 2 L,EP South 2 R),(EP West 2 R,EP East 2 L),(EP West 3 L,EP South 1 R),(EP West 3 R,EP East 3 L),(EP South 1 L,EP South 3 R),(EP South 1 R,EP West 3 L),(EP South 2 L,EP East 2 R),(EP South 2 R,EP West 2 L),(EP South 3 L,EP East 3 R),(EP South 3 R,EP South 1 L)]
    assertEqual "Test 2: Singular atom - " expected result



--Test case 3: All atoms, only reflections
question1Test3 :: IO ()
question1Test3 = do
    let gridSize = 3
    let atoms = [(1,1),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3),(3,4),(3,5)] -- All atoms
    let result = calcInteractions gridSize atoms
    let expected = [(EP East 1 L,EP East 1 R),(EP East 1 R,EP East 1 L),(EP East 2 L,EP East 2 R),(EP East 2 R,EP East 2 L),(EP East 3 L,EP East 3 R),(EP East 3 R,EP East 3 L),(EP West 1 L,EP West 1 R),(EP West 1 R,EP West 1 L),(EP West 2 L,EP West 2 R),(EP West 2 R,EP West 2 L),(EP West 3 L,EP West 3 R),(EP West 3 R,EP West 3 L),(EP South 1 L,EP South 1 R),(EP South 1 R,EP South 1 L),(EP South 2 L,EP South 2 R),(EP South 2 R,EP South 2 L),(EP South 3 L,EP South 3 R),(EP South 3 R,EP South 3 L)]
    assertEqual "Test 3: All atoms - " expected result



--Test case 4: Complex case (large triangle with numerous atoms)
question1Test4 :: IO ()
question1Test4 = do
    let gridSize = 10 --Large size
    let atoms = [(7,10),(3,3),(5,1),(9,17),(6,8)] -- 5 atoms
    let result = calcInteractions gridSize atoms
    let expected = [(EP East 1 L,EP West 1 R),(EP East 1 R,EP East 5 L),(EP East 2 L,EP West 2 R),(EP East 2 R,EP East 3 L),(EP East 3 L,EP East 2 R),(EP East 3 R,EP South 3 L),(EP East 4 L,EP West 4 R),(EP East 4 R,EP South 2 L),(EP East 5 L,EP East 1 R),(EP East 5 R,EP East 6 L),(EP East 6 L,EP East 5 R),(EP East 6 R,EP South 6 L),(EP East 7 L,EP South 9 R),(EP East 7 R,EP South 7 L),(EP East 8 L,EP West 8 R),(EP East 8 R,EP South 8 L),(EP East 9 L,EP East 9 R),(EP East 9 R,EP East 9 L),(EP East 10 L,EP West 10 R),(EP East 10 R,EP South 10 L),(EP West 1 L,EP West 9 R),(EP West 1 R,EP East 1 L),(EP West 2 L,EP West 3 R),(EP West 2 R,EP East 2 L),(EP West 3 L,EP South 8 R),(EP West 3 R,EP West 2 L),(EP West 4 L,EP South 7 R),(EP West 4 R,EP East 4 L),(EP West 5 L,EP West 5 R),(EP West 5 R,EP West 5 L),(EP West 6 L,EP South 5 R),(EP West 6 R,EP South 4 L),(EP West 7 L,EP South 4 R),(EP West 7 R,EP South 5 L),(EP West 8 L,EP South 3 R),(EP West 8 R,EP East 8 L),(EP West 9 L,EP South 2 R),(EP West 9 R,EP West 1 L),(EP West 10 L,EP South 1 R),(EP West 10 R,EP East 10 L),(EP South 1 L,EP South 6 R),(EP South 1 R,EP West 10 L),(EP South 2 L,EP East 4 R),(EP South 2 R,EP West 9 L),(EP South 3 L,EP East 3 R),(EP South 3 R,EP West 8 L),(EP South 4 L,EP West 6 R),(EP South 4 R,EP West 7 L),(EP South 5 L,EP West 7 R),(EP South 5 R,EP West 6 L),(EP South 6 L,EP East 6 R),(EP South 6 R,EP South 1 L),(EP South 7 L,EP East 7 R),(EP South 7 R,EP West 4 L),(EP South 8 L,EP East 8 R),(EP South 8 R,EP West 3 L),(EP South 9 L,EP South 10 R),(EP South 9 R,EP East 7 L),(EP South 10 L,EP East 10 R),(EP South 10 R,EP South 9 L)]
    assertEqual "Test 4: Complex Case - " expected result


------------------------------------ Tests for Question 2 ------------------------------------


assertEqual :: (Eq a, Ord a, Show a) => String -> [a] -> [a] -> IO ()
assertEqual testName expected actual =
    if sort expected == sort actual
        then putStrLn $ testName ++ " passed."
        else putStrLn $ testName ++ " failed. Expected: " ++ show (sort expected) ++ ", but got: " ++ show (sort actual)

testQuestion2::IO()
testQuestion2 = do
    question2Test1
    question2Test2
    question2Test3
    question2Test4
    question2Test5
    question2Test6
   

-- Test case 1: Size 3, No Atoms
question2Test1 :: IO ()
question2Test1 = do
    let observedRays = [(EP East 1 L,EP West 1 R),(EP East 1 R,EP South 1 L),(EP East 2 L,EP West 2 R),(EP East 2 R,EP South 2 L),(EP East 3 L,EP West 3 R),(EP East 3 R,EP South 3 L),(EP West 1 L,EP South 3 R),(EP West 1 R,EP East 1 L),(EP West 2 L,EP South 2 R),(EP West 2 R,EP East 2 L),(EP West 3 L,EP South 1 R),(EP West 3 R,EP East 3 L),(EP South 1 L,EP East 1 R),(EP South 1 R,EP West 3 L),(EP South 2 L,EP East 2 R),(EP South 2 R,EP West 2 L),(EP South 3 L,EP East 3 R),(EP South 3 R,EP West 1 L)]
    let result = solveTBB 0 observedRays
    let expected = [] -- No atoms
    assertEqual "Test 1: No atoms - " expected result




-- Test case 2: Size 3, One Atom
question2Test2 :: IO ()
question2Test2 = do
    let observedRays = [(EP East 1 L,EP East 1 R),(EP East 1 R,EP East 1 L),(EP East 2 L,EP West 2 R),(EP East 2 R,EP South 2 L),(EP East 3 L,EP West 3 R),(EP East 3 R,EP South 3 L),(EP West 1 L,EP West 1 R),(EP West 1 R,EP West 1 L),(EP West 2 L,EP South 2 R),(EP West 2 R,EP East 2 L),(EP West 3 L,EP South 1 R),(EP West 3 R,EP East 3 L),(EP South 1 L,EP South 3 R),(EP South 1 R,EP West 3 L),(EP South 2 L,EP East 2 R),(EP South 2 R,EP West 2 L),(EP South 3 L,EP East 3 R),(EP South 3 R,EP South 1 L)]
    let result = solveTBB 1 observedRays
    let expected = [(1,1)] -- Singular atom
    assertEqual "Test 2: One atom - " expected result


-- Test case 3: Large Size - Size 10, One Atom
question2Test3 :: IO ()
question2Test3 = do
    let observedRays = [(EP East 1 L,EP West 1 R),(EP East 1 R,EP South 1 L),(EP East 2 L,EP West 2 R),(EP East 2 R,EP South 2 L),(EP East 3 L,EP West 3 R),(EP East 3 R,EP West 2 L),(EP East 4 L,EP West 4 R),(EP East 4 R,EP South 4 L),(EP East 5 L,EP South 9 R),(EP East 5 R,EP South 5 L),(EP East 6 L,EP West 6 R),(EP East 6 R,EP South 6 L),(EP East 7 L,EP West 7 R),(EP East 7 R,EP South 7 L),(EP East 8 L,EP West 8 R),(EP East 8 R,EP South 8 L),(EP East 9 L,EP West 9 R),(EP East 9 R,EP South 9 L),(EP East 10 L,EP West 10 R),(EP East 10 R,EP South 10 L),(EP West 1 L,EP South 10 R),(EP West 1 R,EP East 1 L),(EP West 2 L,EP East 3 R),(EP West 2 R,EP East 2 L),(EP West 3 L,EP South 8 R),(EP West 3 R,EP East 3 L),(EP West 4 L,EP South 7 R),(EP West 4 R,EP East 4 L),(EP West 5 L,EP South 6 R),(EP West 5 R,EP South 3 L),(EP West 6 L,EP South 5 R),(EP West 6 R,EP East 6 L),(EP West 7 L,EP South 4 R),(EP West 7 R,EP East 7 L),(EP West 8 L,EP South 3 R),(EP West 8 R,EP East 8 L),(EP West 9 L,EP South 2 R),(EP West 9 R,EP East 9 L),(EP West 10 L,EP South 1 R),(EP West 10 R,EP East 10 L),(EP South 1 L,EP East 1 R),(EP South 1 R,EP West 10 L),(EP South 2 L,EP East 2 R),(EP South 2 R,EP West 9 L),(EP South 3 L,EP West 5 R),(EP South 3 R,EP West 8 L),(EP South 4 L,EP East 4 R),(EP South 4 R,EP West 7 L),(EP South 5 L,EP East 5 R),(EP South 5 R,EP West 6 L),(EP South 6 L,EP East 6 R),(EP South 6 R,EP West 5 L),(EP South 7 L,EP East 7 R),(EP South 7 R,EP West 4 L),(EP South 8 L,EP East 8 R),(EP South 8 R,EP West 3 L),(EP South 9 L,EP East 9 R),(EP South 9 R,EP East 5 L),(EP South 10 L,EP East 10 R),(EP South 10 R,EP West 1 L)]
    let result = solveTBB 1 observedRays
    let expected = [(5,6)] -- Singular atom
    assertEqual "Test 3: Large size - " expected result


-- Test case 4: Large number of atoms - Size 5, 5 atoms
question2Test4 :: IO ()
question2Test4 = do
    let observedRays = [(EP East 1 L,EP East 1 R),(EP East 1 R,EP East 1 L),(EP East 2 L,EP East 2 R),(EP East 2 R,EP East 2 L),(EP East 3 L,EP South 5 R),(EP East 3 R,EP South 2 L),(EP East 4 L,EP South 3 R),(EP East 4 R,EP South 4 L),(EP East 5 L,EP South 4 R),(EP East 5 R,EP South 5 L),(EP West 1 L,EP West 1 R),(EP West 1 R,EP West 1 L),(EP West 2 L,EP West 3 R),(EP West 2 R,EP West 3 L),(EP West 3 L,EP West 2 R),(EP West 3 R,EP West 2 L),(EP West 4 L,EP South 2 R),(EP West 4 R,EP South 1 L),(EP West 5 L,EP South 1 R),(EP West 5 R,EP South 3 L),(EP South 1 L,EP West 4 R),(EP South 1 R,EP West 5 L),(EP South 2 L,EP East 3 R),(EP South 2 R,EP West 4 L),(EP South 3 L,EP West 5 R),(EP South 3 R,EP East 4 L),(EP South 4 L,EP East 4 R),(EP South 4 R,EP East 5 L),(EP South 5 L,EP East 5 R),(EP South 5 R,EP East 3 L)]
    let result = solveTBB 5 observedRays
    let expected = [(5,6), (2,3), (3,3), (4,2), (1,1) ] -- 5 atoms
    assertEqual "Test 4: 5 Atoms - " expected result



-- Test case 5: Complex Case - Size 10, 5 atoms
question2Test5 :: IO ()
question2Test5 = do
    let observedRays = [(EP East 1 L,EP West 1 R),(EP East 1 R,EP West 1 L),(EP East 2 L,EP West 3 R),(EP East 2 R,EP West 3 L),(EP East 3 L,EP East 3 R),(EP East 3 R,EP East 3 L),(EP East 4 L,EP West 4 R),(EP East 4 R,EP South 4 L),(EP East 5 L,EP West 7 R),(EP East 5 R,EP East 7 L),(EP East 6 L,EP West 6 R),(EP East 6 R,EP South 6 L),(EP East 7 L,EP East 5 R),(EP East 7 R,EP South 7 L),(EP East 8 L,EP West 8 R),(EP East 8 R,EP South 8 L),(EP East 9 L,EP West 5 R),(EP East 9 R,EP South 9 L),(EP East 10 L,EP West 10 R),(EP East 10 R,EP South 10 L),(EP West 1 L,EP East 1 R),(EP West 1 R,EP East 1 L),(EP West 2 L,EP South 9 R),(EP West 2 R,EP South 1 L),(EP West 3 L,EP East 2 R),(EP West 3 R,EP East 2 L),(EP West 4 L,EP South 7 R),(EP West 4 R,EP East 4 L),(EP West 5 L,EP South 6 R),(EP West 5 R,EP East 9 L),(EP West 6 L,EP South 5 R),(EP West 6 R,EP East 6 L),(EP West 7 L,EP South 4 R),(EP West 7 R,EP East 5 L),(EP West 8 L,EP West 9 R),(EP West 8 R,EP East 8 L),(EP West 9 L,EP South 2 R),(EP West 9 R,EP West 8 L),(EP West 10 L,EP South 1 R),(EP West 10 R,EP East 10 L),(EP South 1 L,EP West 2 R),(EP South 1 R,EP West 10 L),(EP South 2 L,EP South 3 R),(EP South 2 R,EP West 9 L),(EP South 3 L,EP South 10 R),(EP South 3 R,EP South 2 L),(EP South 4 L,EP East 4 R),(EP South 4 R,EP West 7 L),(EP South 5 L,EP South 8 R),(EP South 5 R,EP West 6 L),(EP South 6 L,EP East 6 R),(EP South 6 R,EP West 5 L),(EP South 7 L,EP East 7 R),(EP South 7 R,EP West 4 L),(EP South 8 L,EP East 8 R),(EP South 8 R,EP South 5 L),(EP South 9 L,EP East 9 R),(EP South 9 R,EP West 2 L),(EP South 10 L,EP East 10 R),(EP South 10 R,EP South 3 L)]
    let result = solveTBB 5 observedRays
    let expected = [(2,2),(3,5),(5,4),(7,9),(9,3)] 
    assertEqual "Test 5: Complex Case - " expected result



-- Test case 6: Size 3, Full bottom row of atoms
question2Test6 :: IO ()
question2Test6 = do
    let observedRays = [(EP East 1 L,EP West 1 R),(EP East 1 R,EP West 2 L),(EP East 2 L,EP West 2 R),(EP East 2 R,EP West 1 L),(EP East 3 L,EP East 3 R),(EP East 3 R,EP East 3 L),(EP East 4 L,EP West 4 R),(EP East 4 R,EP South 4 L),(EP East 5 L,EP West 5 R),(EP East 5 R,EP South 5 L),(EP West 1 L,EP East 2 R),(EP West 1 R,EP East 1 L),(EP West 2 L,EP East 1 R),(EP West 2 R,EP East 2 L),(EP West 3 L,EP West 3 R),(EP West 3 R,EP West 3 L),(EP West 4 L,EP South 2 R),(EP West 4 R,EP East 4 L),(EP West 5 L,EP South 1 R),(EP West 5 R,EP East 5 L),(EP South 1 L,EP South 3 R),(EP South 1 R,EP West 5 L),(EP South 2 L,EP South 4 R),(EP South 2 R,EP West 4 L),(EP South 3 L,EP South 5 R),(EP South 3 R,EP South 1 L),(EP South 4 L,EP East 4 R),(EP South 4 R,EP South 2 L),(EP South 5 L,EP East 5 R),(EP South 5 R,EP South 3 L)]
    let result = solveTBB 5 observedRays
    let expected = [(3,1),(3,2),(3,3),(3,4),(3,5)] -- Bottom row full
    assertEqual "Test 6: Large size - " expected result


------------------------------------ Tests for Question 3 ------------------------------------


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


------------------------------------ Tests for Question 4 ------------------------------------


assertEqual :: String -> LamMacroExpr -> Maybe LamMacroExpr -> IO ()
assertEqual testName expected actual =
    case actual of
        Nothing ->
            putStrLn $ testName ++ " failed. Expected: " ++ show expected ++ ", but got: Nothing"
        Just value ->
            if expected == value
                then putStrLn $ testName ++ " passed."
                else putStrLn $ testName ++ " failed. Expected: " ++ show expected ++ ", but got: " ++ show actual



testQuestion4::IO()
testQuestion4 = do
    question4Test1
    question4Test2
    question4Test3
    question4Test4
    question4Test5
    question4Test6
    question4Test7
    question4Test8
    question4Test9

--Simple variable
question4Test1 :: IO ()
question4Test1 = do
    let stringExpression = "x0"
    let expected = LamDef[] (LamVar 0)
    let result = parseLamMacro stringExpression
    assertEqual "Test 1: Simple variable - " expected result


--Simple Lambda Abstraction
question4Test2 :: IO ()
question4Test2 = do
    let stringExpression = "λx1 → x1"
    let expected = LamDef [] (LamAbs 1 (LamVar 1))
    let result = parseLamMacro stringExpression
    assertEqual "Test 2: Simple Lambda Abstraction- " expected result



--Lambda Application
question4Test3 :: IO ()
question4Test3 = do
    let stringExpression = "x1 λx1 → x1"
    let expected = LamDef [] (LamApp (LamVar 1)(LamAbs 1 (LamVar 1)))
    let result = parseLamMacro stringExpression
    assertEqual "Test 3: Lambda Application - " expected result


--Bracketed Lambda Application
question4Test4 :: IO ()
question4Test4 = do
    let stringExpression = "(λx1 → x1)x1"
    let expected = LamDef [] (LamApp (LamAbs 1 (LamVar 1))(LamVar 1))
    let result = parseLamMacro stringExpression
    assertEqual "Test 4: Bracketed Lambda Application - " expected result



--Singular Macro Expression
question4Test5 :: IO ()
question4Test5 = do
    let stringExpression = "def F = λx1 → x1 in F"
    let expected = LamDef [("F", LamAbs 1 (LamVar 1))] (LamMacro "F")
    let result = parseLamMacro stringExpression
    assertEqual "Test 5: Singular Macro Expression - " expected result



--Two letter Macro Name
question4Test6 :: IO ()
question4Test6 = do
    let stringExpression = "def ID = λx1 → x1 in ID"
    let expected = LamDef [("ID", LamAbs 1 (LamVar 1))] (LamMacro "ID")
    let result = parseLamMacro stringExpression
    assertEqual "Test 6: Two letter macro name - " expected result



-- More Complex Macro Definition
question4Test7 :: IO ()
question4Test7 = do
    let stringExpression = "def SND = λx1 → λx2 → x2 in SND x1 x2"
    let expected = LamDef [("SND", LamAbs 1 (LamAbs 2 (LamVar 2)))] (LamApp (LamApp (LamMacro "SND")(LamVar 1)) (LamVar 2))
    let result = parseLamMacro stringExpression
    let result = parseLamMacro stringExpression
    assertEqual "Test 7: SND - " expected result


-- Multiple Macro Expressions
question4Test8 :: IO ()
question4Test8 = do
    let stringExpression = "def ID = λx1 → x1 in def SND = λx1 → λx2 → x2 in SND x1 x2 ID"
    let expected = LamDef [ ("ID",LamAbs 1 (LamVar 1)) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ] (LamApp (LamApp (LamApp (LamMacro "SND") (LamVar 1)) (LamVar 2)) (LamMacro "ID"))
    let result = parseLamMacro stringExpression
    assertEqual "Test 8: Multiple Macro Expressions - " expected result



-- Multiple Consecutive Macro Expression
question4Test9 :: IO ()
question4Test9 = do
    let stringExpression = "def ID = λx1 → x1 in def DUP = λx1 → x1x1 in ID DUP x1 "
    let expected = LamDef [("ID",LamAbs 1 (LamVar 1)) , ("DUP",LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) ] (LamApp (LamApp (LamMacro "ID") (LamMacro "DUP")) (LamVar 1))
    let result = parseLamMacro stringExpression
    assertEqual "Test 9: Two letter macro name again - " expected result


------------------------------------ Tests for Question 5 ------------------------------------


assertEqual :: String -> LamMacroExpr -> LamMacroExpr -> IO ()
assertEqual testName expected actual =
    if expected == actual
        then putStrLn $ testName ++ " passed."
        else putStrLn $ testName ++ " failed. Expected: " ++ show expected ++ ", but got: " ++ show actual

testQuestion5:: IO ()
testQuestion5 = do
    question5Test1
    question5Test2
    question5Test3
    question5Test4
    question5Test5

--Simple variable
question5Test1 :: IO ()
question5Test1 = do
    let normalForm = LamDef [] (LamVar 1)
    let expected = LamDef [] (LamAbs 0 (LamApp (LamVar 0) (LamVar 1)))
    let result = cpsTransform normalForm
    assertEqual "Test 1: Simple variable - " expected result


--Simple Lambda Abstraction
question5Test2 :: IO ()
question5Test2 = do
    let normalForm = LamDef [] (LamAbs 1 (LamVar 1))
    let expected = LamDef [] (LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamAbs 0 (LamApp (LamVar 0) (LamVar 1))))))
    let result = cpsTransform normalForm
    assertEqual "Test 2: Simple Lambda Abstraction - " expected result


--Simple Lambda Application
question5Test3 :: IO ()
question5Test3 = do
    let normalForm = LamDef [] (LamApp (LamVar 1) (LamVar 2))
    let expected = LamDef [] (LamAbs 0 (LamApp (LamAbs 0 (LamApp (LamVar 0) (LamVar 1))) (LamAbs 3 (LamApp (LamAbs 0 (LamApp (LamVar 0) (LamVar 2))) (LamAbs 4 (LamApp (LamApp (LamVar 3) (LamVar 4)) (LamVar 0)))))))
    let result = cpsTransform normalForm
    assertEqual "Test 3: Simple Application - " expected result


--Simple Macro Usage
question5Test4 :: IO ()
question5Test4 = do
    let normalForm = LamDef [("F", LamAbs 1 (LamVar 1))] (LamMacro "F")
    let expected = LamDef [("F",LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamAbs 0 (LamApp (LamVar 0) (LamVar 1))))))] (LamMacro "F")
    let result = cpsTransform normalForm
    assertEqual "Test 4: Simple Macro - " expected result



--Multiple Macros
question5Test5 :: IO ()
question5Test5 = do
    let normalForm = LamDef [("ID",LamAbs 1 (LamVar 1)) , ("DUP",LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) ] (LamApp (LamApp (LamMacro "ID") (LamMacro "DUP")) (LamVar 1))
    let expected =  LamDef [("ID",LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamAbs 0 (LamApp (LamVar 0) (LamVar 1)))))),("DUP",LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamAbs 0 (LamApp (LamAbs 0 (LamApp (LamVar 0) (LamVar 1))) (LamAbs 2 (LamApp (LamAbs 0 (LamApp (LamVar 0) (LamVar 1))) (LamAbs 3 (LamApp (LamApp (LamVar 2) (LamVar 3)) (LamVar 0))))))))))] (LamAbs 0 (LamApp (LamAbs 0 (LamApp (LamMacro "ID") (LamAbs 1 (LamApp (LamMacro "DUP") (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 0))))))) (LamAbs 2 (LamApp (LamAbs 0 (LamApp (LamVar 0) (LamVar 1))) (LamAbs 3 (LamApp (LamApp (LamVar 2) (LamVar 3)) (LamVar 0)))))))
    let result = cpsTransform normalForm
    assertEqual "Test 5: Multiple Complex Macros - " expected result


------------------------------------ Tests for Question 6 ------------------------------------

assertEqual :: String -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int) -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int) -> IO ()
assertEqual testName expected actual =
    if expected == actual
        then putStrLn $ testName ++ " passed."
        else putStrLn $ testName ++ " failed. Expected: " ++ show expected ++ ", but got: " ++ show actual



testQuestion6:: IO ()
testQuestion6 = do
    question6Test1
    question6Test2
    question6Test3
    question6Test4
    question6Test5
    question6Test6 
    question6Test7
    question6Test8




--Simple identity lambda application
question6Test1 :: IO ()
question6Test1 = do
    let expression = LamDef [] (LamApp (LamAbs 1 (LamVar 1))(LamVar 2))
    let limit = 9
    let expected = (Just 1,Just 1,Just 8,Just 8)
    let result = compareInnerOuter expression limit
    assertEqual "Test 1: Simple identity application - " expected result


--Testing limiting edge cases
question6Test2 :: IO ()
question6Test2 = do
    let expression = LamDef [] (LamApp (LamAbs 1 (LamVar 1))(LamVar 2))
    let limit = 0
    let expected = (Nothing,Nothing,Nothing,Nothing)
    let result = compareInnerOuter expression limit
    assertEqual "Test 2: Limit (0) Edge Case - " expected result

--Testing limiting edge cases
question6Test3 :: IO ()
question6Test3 = do
    let expression = LamDef [] (LamApp (LamAbs 1 (LamVar 1))(LamVar 2))
    let limit = 1
    let expected = (Just 1,Just 1,Nothing,Nothing)
    let result = compareInnerOuter expression limit
    assertEqual "Test 3: Limit Edge Case (1) - " expected result


--Testing limiting edge cases
question6Test4 :: IO ()
question6Test4 = do
    let expression = LamDef [] (LamApp (LamAbs 1 (LamVar 1))(LamVar 2))
    let limit = 7
    let expected = (Just 1,Just 1,Nothing,Nothing)
    let result = compareInnerOuter expression limit
    assertEqual "Test 4: Limit Edge Case (7) - " expected result


--Testing limiting edge cases
question6Test5 :: IO ()
question6Test5 = do
    let expression = LamDef [] (LamApp (LamAbs 1 (LamVar 1))(LamVar 2))
    let limit = 8
    let expected = (Just 1,Just 1, Just 8, Just 8)
    let result = compareInnerOuter expression limit
    assertEqual "Test 5: Limit Edge Case (8) - " expected result



--Singular Macro
question6Test6 :: IO ()
question6Test6 = do
    let expression = LamDef [("ID", LamAbs 1 (LamVar 1))] (LamApp (LamMacro "ID")(LamVar 1))
    let limit = 100
    let expected = (Just 2,Just 2, Just 9, Just 9)
    let result = compareInnerOuter expression limit
    assertEqual "Test 6: Idendity Macro Applied to a singular variable - " expected result


-- Complex Bound Test
question6Test7 :: IO ()
question6Test7 = do
    let expression = LamDef [] (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 2 (LamApp (LamVar 2) (LamVar 2))))
    let limit = 5
    let expected = (Nothing, Nothing, Nothing, Nothing)
    let result = compareInnerOuter expression limit
    assertEqual "Test 7: Bound exceeded with complex expression - " expected result


-- 8. CPS Stress Test: Macro in Body + Nested Lambdas
question6Test8 :: IO ()
question6Test8 = do
    let expression =  LamDef [("F", LamAbs 1 (LamAbs 2 (LamVar 1)))] (LamApp (LamApp (LamMacro "F") (LamAbs 10 (LamVar 10))) (LamAbs 11 (LamApp (LamVar 11) (LamVar 11))))
    let limit = 100
    let expected = (Just 3, Just 3, Just 19, Just 15)
    let result = compareInnerOuter expression limit
    assertEqual "Test 8: CPS Stress Test - " expected result


