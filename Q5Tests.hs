-- Author: EdwardWray
-- Copyright (c) 2024, University of Southampton

-- Your imports here
import Data.Set (Set)
import qualified Data.Set as Set



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



--Code from Question 5
-- DO NOT MODIFY THESE DATATYPES
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)


-- Transforms a LamMacroExpr into its CPS equivalent
cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform (LamDef defs mainExpr) = 
    let cpsDefs = map cpsMacroDefs defs
        cpsMainExpr = cpsExpr defs mainExpr
    in LamDef cpsDefs cpsMainExpr


-- Transforms macro definitions into their CPS equivalent
cpsMacroDefs :: (String, LamExpr) -> (String, LamExpr)
cpsMacroDefs (name, expr) = (name,  cpsExpr [] expr)


-- Transforms a LamExpr into its CPS equivalent
cpsExpr :: [(String, LamExpr)] -> LamExpr -> LamExpr
cpsExpr defs expr = case expr of
    -- Variable case: Wrap in a continuation
    LamVar x -> 
        LamAbs k (LamApp (LamVar k) (LamVar x))
        where k = freshVar (freeVars defs expr)
    
    -- Abstraction case: Wrap body in a continuation 
    LamAbs x e -> 
        LamAbs k (LamApp (LamVar k) (LamAbs x (cpsExpr defs e)))
        where k = freshVar (freeVars defs expr)
    
    -- Application case: Transform both operands and chain their continuations
    LamApp e1 e2 ->     
        LamAbs k (LamApp (cpsExpr defs e1) (LamAbs f (LamApp (cpsExpr defs e2) (LamAbs e (LamApp (LamApp (LamVar f) (LamVar e)) (LamVar k))))))
        where vars = Set.union (freeVars defs e1) (freeVars defs e2)
              k = freshVar vars
              f = freshVar (Set.insert k vars)
              e = freshVar (Set.insert f (Set.insert k vars))
    
    -- Macro case: Leave unchanged
    LamMacro x -> LamMacro x


-- Finds a fresh variable not in the given set of variables
freshVar :: Set Int -> Int
freshVar vars = head [x | x <- [0..], not (Set.member x vars)]


-- Computes the set of free variables in a lambda expression
freeVars :: [(String, LamExpr)] -> LamExpr -> Set Int
freeVars defs (LamVar x) = Set.singleton x
freeVars defs (LamApp e1 e2) = Set.union (freeVars defs e1) (freeVars defs e2)
freeVars defs (LamAbs x e) = Set.delete x (freeVars defs e)
freeVars defs (LamMacro name) = case lookup name defs of
    Just expr -> freeVars defs expr
    Nothing -> Set.empty



