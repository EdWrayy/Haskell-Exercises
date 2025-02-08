-- Author: EdwardWray
-- Copyright (c) 2024, University of Southampton

import Data.Set
import qualified Data.Set as Set


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



--Code from Question 6

-- Author: EdwardWray
-- Copyright (c) 2024, University of Southampton


-- DO NOT MODIFY THESE DATA TYPES
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)


-- Compares reduction lengths for inner and outer strategies, including CPS-transformed expressions
compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter expr@(LamDef defs body) bound =
    let inner = numberOfReductions innerRedn1 expr bound
        outer = numberOfReductions outerRedn1 expr bound
        LamDef cpsDefs cpsExpr = cpsTransform expr
        cpsTransformed = LamDef cpsDefs (LamApp cpsExpr (LamAbs 0 (LamVar 0))) --Apply to the identity continuation
        cpsInner = numberOfReductions innerRedn1 cpsTransformed bound
        cpsOuter= numberOfReductions outerRedn1 cpsTransformed bound
    in (inner, outer, cpsInner, cpsOuter)


-- Counts the number of reductions using a given strategy
numberOfReductions :: (LamMacroExpr -> Maybe LamMacroExpr) -> LamMacroExpr -> Int -> Maybe Int
numberOfReductions reductionFunc expr bound
    | bound < 0 = Nothing
    | otherwise
    = case reductionFunc expr of
        Nothing -> Just 0
        Just expr' -> case numberOfReductions reductionFunc expr' (bound - 1) of
            Nothing -> Nothing
            Just n -> Just (n + 1)


-- Performs one inner reduction if possible
innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 lamMacroExpr@(LamDef defs expr) =
    -- First try to find any beta-reductions in the expression
    case findInnerMostBetaRedex lamMacroExpr of
        Just reduced -> Just reduced
        -- Only if no beta-reductions are possible, try macro expansion
        Nothing -> 
                if not (Prelude.null defs)
                then Just $ macroExpand lamMacroExpr (last defs)
                else Nothing




-- Helper function to find the innermost beta redex
findInnerMostBetaRedex :: LamMacroExpr -> Maybe LamMacroExpr
findInnerMostBetaRedex (LamDef defs expr) =
    -- Recurse into e1 first, then e2
    case expr of
        LamApp e1 e2 ->
            case findInnerMostBetaRedex (LamDef defs e1) of
                Just (LamDef defs' e1') -> Just $ LamDef defs' (LamApp e1' e2)
                Nothing -> case e1 of
                    LamAbs x body -> Just $ LamDef defs (betaReduce (LamApp e1 e2))
                    _ -> case findInnerMostBetaRedex (LamDef defs e2) of
                        Just (LamDef defs' e2') -> Just $ LamDef defs' (LamApp e1 e2')
                        Nothing -> Nothing
        -- Recurse into the body of the lambda abstraction
        LamAbs x body ->
            case findInnerMostBetaRedex (LamDef defs body) of
                Just (LamDef defs' body') -> Just $ LamDef defs' (LamAbs x body')
                Nothing -> Nothing
      
        -- Not a redex
        _ -> Nothing



--Performs one outer reduction if possible
outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 lamMacroExpr@(LamDef defs expr)
    -- If there are any macros, expand them first as these are always the outermost redexes.
    | not (Prelude.null defs) = Just $ macroExpand lamMacroExpr (head defs)
    | otherwise =
        case expr of
            --If we have a lambda application we can check if it is a redex, if not recursively search for one
            LamApp e1 e2 ->
                case e1 of
                    LamAbs x body -> Just $ LamDef defs (betaReduce (LamApp e1 e2))
                    _ -> case outerRedn1 (LamDef defs e1) of
                            Just (LamDef defs' e1') -> Just $ LamDef defs' (LamApp e1' e2)
                            Nothing -> case outerRedn1 (LamDef defs e2) of
                                Just (LamDef defs' e2') -> Just $ LamDef defs' (LamApp e1 e2')
                                Nothing -> Nothing
          
            --If we have a lambda abstraction we can recursively check for a redex
            LamAbs x body ->
                case outerRedn1 (LamDef defs body) of
                    Just (LamDef defs' body') -> Just $ LamDef defs' (LamAbs x body')
                    Nothing -> Nothing
            
            --Variables cannot be reduced
            _ -> Nothing



--Performs a single reduction
betaReduce :: LamExpr -> LamExpr
betaReduce (LamApp (LamAbs x body) arg) = substitute x arg body -- A redex is an application of a lambda abstraction to an argument
betaReduce expr = expr


-- Substitutes a variable with an expression within another expression
substitute :: Int -> LamExpr -> LamExpr -> LamExpr
substitute targetVar sub expr = case expr of
    LamVar x | x == targetVar -> sub
             | otherwise -> LamVar x
    LamAbs x body | x == targetVar -> LamAbs x body
                  | otherwise -> LamAbs x (substitute targetVar sub body)
    LamApp e1 e2 -> LamApp (substitute targetVar sub e1) (substitute targetVar sub e2)
    LamMacro name -> LamMacro name


--Replace all instances of a macro name with its definition, and remove the definition as per the macro expansion rule def X = E in ME ---→ ME [ E / X ]
macroExpand :: LamMacroExpr -> (String, LamExpr) -> LamMacroExpr
macroExpand (LamDef defs expr) (name, def) =
    let 
        -- Filter out the definition we are expanding, so it is removed
        newDefs  = Prelude.filter (\(n,_) -> n /= name) defs
        -- Recursively replace occurrences of the macro in the main expr
        newExpr  = expandInExpr expr (name, def)
    in 
        LamDef newDefs newExpr


-- Replaces the instances of a LamMacro in the main expression with its definition
expandInExpr :: LamExpr -> (String, LamExpr) -> LamExpr
expandInExpr expr (name, def) =
  case expr of
    LamMacro macroName
      | macroName == name  -> def
      | otherwise          -> LamMacro macroName

    LamApp e1 e2 -> 
      LamApp (expandInExpr e1 (name, def)) (expandInExpr e2 (name, def))

    LamAbs x body -> 
      LamAbs x (expandInExpr body (name, def))

    LamVar x -> LamVar x



-- cpsTransform code from Question 5

-- Transforms a LamMacroExpr into its CPS equivalent
cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform (LamDef defs mainExpr) = 
    let cpsDefs = Prelude.map cpsMacroDefs defs
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



