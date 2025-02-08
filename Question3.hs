-- Author: EdwardWray
-- Copyright (c) 2024, University of Southampton

-- Your imports here
import Data.List (isInfixOf)
import qualified Data.Text as T
import Debug.Trace (trace)

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
    -- Recursively transform child nodes first:
    let newExpr = case expr of
            LamVar v       -> LamVar v
            LamAbs param e -> LamAbs param (substituteMacros macros e)
            LamApp e1 e2   -> LamApp (substituteMacros macros e1) (substituteMacros macros e2)
            LamMacro m     -> LamMacro m    -- Keep an existing LamMacro as is
    in
    -- Now see if newExpr as a whole matches any macro definition
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
