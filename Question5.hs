-- Author: EdwardWray
-- Copyright (c) 2024, University of Southampton

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (lookup)

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



