-- Author: EdwardWray
-- Copyright (c) 2024, University of Southampton


import Data.Tuple (swap)
import Data.List (sort)


--Implementing a custom assertEqual function for testing
assertEqual :: (Eq a, Ord a, Show a) => String -> [a] -> [a] -> IO ()
assertEqual testName expected actual =
    if sort expected == sort actual
        then putStrLn $ testName ++ " passed."
        else putStrLn $ testName ++ " failed. Expected: " ++ show (sort expected) ++ ", but got: " ++ show (sort actual)


------------------------------------ Tests for Question 1 ------------------------------------

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






--Code from Question 1
-- DO NOT MODIFY THESE DATATYPES OTHER THAN TO ADD TO THE DERIVING LIST
data EdgeDir = L | R deriving (Eq,Show,Ord)
data Face = East | West | South deriving (Eq,Show,Ord)
data EdgePoint = EP Face Int EdgeDir deriving (Eq,Show,Ord)
type Atom = (Int,Int)
--This is a data type representing the direction in which the ray is moving 
--(e.g: Right-Up(RU), Left-Up(LU), Down-Right (DR), Right(RR), ect.)
data MovingDirection = RU | LU | DR | DL | RR | LL deriving (Eq,Show)

-- Main function to calculate ray interactions with the grid
calcInteractions :: Int -> [Atom] -> [(EdgePoint,EdgePoint)]
calcInteractions n atoms =
    let
        swappedAtoms = map swap atoms --Swapping (row, offset) to (offset, row) as the program views it the other way
        edgePoints = allEdgePoints n
        exitEdges = map (findExitEdge n swappedAtoms) edgePoints
    in zip edgePoints exitEdges


-- Generates all edge points for the grid of the given size
allEdgePoints :: Int -> [EdgePoint]
allEdgePoints n =
    [ EP East i d   | i <- [1..n], d <- [L,R]] ++
    [ EP West i d   | i <- [1..n], d <- [L,R]] ++
    [ EP South i d  | i <- [1..n], d <- [L,R]]


-- Determines the exit edge for a ray entering at a given edge point
findExitEdge :: Int -> [Atom] -> EdgePoint -> EdgePoint
findExitEdge size atoms (EP face index direction)
  | face == South && direction == L
  = if isAtomPresent atoms (maximumOffset index, size) then
        EP South index R --If the initial triangle is an atom, we can immediately return the reflected ray
    else
        moveRightUp (maximumOffset index, size) size atoms --Otherwise call the appropriate movement function
  | face == South && direction == R
  = if isAtomPresent atoms (maximumOffset index, size) then
        EP South index L
    else
        moveLeftUp (maximumOffset index, size) size atoms
  | face == East && direction == L
  = if isAtomPresent atoms (maximumOffset index, index) then
        EP East index R
    else
        moveLeft (maximumOffset index, index) size atoms
  | face == East && direction == R
  = if isAtomPresent atoms (maximumOffset index, index) then
        EP East index L
    else
        moveDownLeft (maximumOffset index, index) size atoms
  | face == West && direction == L
  = if isAtomPresent atoms (1, index) then
        EP West index R
    else
        moveDownRight (1, index) size atoms
  | face == West && direction == R
  = if isAtomPresent atoms (1, index) then
        EP West index L
    else
        moveRight (1, index) size atoms
  | otherwise = error "Not a valid direction"


-- Calculate the maximum offset for a given row
maximumOffset:: Int -> Int
maximumOffset n = 2 * n - 1

-- Converts an offset on the south edge to the corresponding edge number
offSetToEdge :: Int -> Int
offSetToEdge n = (n + 1) `div` 2




-- Moves the ray based on its direction, using discrete triangle coordinates
moveRightUp :: (Int, Int) -> Int -> [Atom] -> EdgePoint
moveRightUp (offset, row) size atoms
  --First move right
  | isOutside (offset + 1, row) size
  --If the new triangle is outside of the grid, then we can find which edge the ray left and return this
  = triangleToExitEdge RU (offset, row)
  --If the new triangle is an atom, we must reflect the ray
  | isAtomPresent atoms (offset + 1, row)
  = reflectRay (offset, row) (offset + 1, row) size RU atoms
  --Now move up and repeat the same process
  | isOutside (offset, row - 1) size
  = triangleToExitEdge RU (offset + 1, row)
  | isAtomPresent atoms (offset, row - 1)
  = reflectRay (offset + 1, row) (offset, row - 1) size RU atoms
  --If we have not hit an atom or left the grid after moving right then up, we can continue moving in the same direction
  | otherwise = moveRightUp (offset, row - 1) size atoms


-- The same format is followed for all movement functions
moveLeftUp :: (Int, Int) -> Int -> [Atom] -> EdgePoint
moveLeftUp (offset, row) size atoms
  | isOutside (offset - 1, row) size
  = triangleToExitEdge LU (offset, row)
  | isAtomPresent atoms (offset - 1, row)
  = reflectRay (offset, row) (offset - 1, row) size LU atoms
  | isOutside (offset - 2, row - 1) size
  = triangleToExitEdge LU (offset - 1, row)
  | isAtomPresent atoms (offset - 2, row - 1)
  = reflectRay (offset - 1, row) (offset - 2, row - 1) size LU atoms
  | otherwise = moveLeftUp (offset - 2, row - 1) size atoms


moveDownRight :: (Int, Int) -> Int -> [Atom] -> EdgePoint
moveDownRight (offset, row) size atoms
  | isOutside   (offset + 1, row + 1) size
  = triangleToExitEdge DR (offset, row)
  | isAtomPresent atoms (offset + 1, row + 1)
  = reflectRay (offset, row) (offset + 1, row + 1) size DR atoms
  | isOutside (offset + 2, row + 1) size
  = triangleToExitEdge DR (offset + 1, row + 1)
  | isAtomPresent atoms (offset + 2, row + 1)
  = reflectRay (offset + 1, row + 1) (offset + 2, row + 1) size DR atoms
  | otherwise = moveDownRight (offset + 2, row + 1) size atoms



moveDownLeft :: (Int, Int) -> Int -> [Atom] -> EdgePoint
moveDownLeft (offset, row) size atoms
  | isOutside (offset + 1, row + 1) size
  = triangleToExitEdge DL (offset, row)
  | isAtomPresent atoms (offset + 1, row + 1)
  = reflectRay (offset, row) (offset + 1, row + 1) size DL atoms
  | isOutside (offset, row + 1) size
  = triangleToExitEdge DL (offset + 1, row + 1)
  | isAtomPresent atoms (offset, row + 1)
  = reflectRay (offset + 1, row + 1) (offset, row + 1) size DL atoms
  | otherwise = moveDownLeft (offset, row + 1) size atoms

moveRight :: (Int, Int) -> Int -> [Atom] -> EdgePoint
moveRight (offset, row) size atoms
  | isOutside (offset + 1, row) size
  = triangleToExitEdge RR (offset, row)
  | isAtomPresent atoms (offset + 1, row)
  = reflectRay (offset, row) (offset + 1, row) size RR atoms
  | otherwise = moveRight (offset + 1, row) size atoms

moveLeft :: (Int, Int) -> Int -> [Atom] -> EdgePoint
moveLeft (offset, row) size atoms
  | isOutside (offset - 1, row) size
  = triangleToExitEdge LL (offset, row)
  | isAtomPresent atoms (offset - 1, row)
  = reflectRay (offset, row) (offset - 1, row) size LL atoms
  | otherwise = moveLeft (offset - 1, row) size atoms





--Returns true if a given position x and y matches the position of an atom
isAtomPresent :: [Atom] -> (Int,Int) -> Bool
isAtomPresent xs (x, y) = (x, y) `elem` xs


-- Reflects the ray when it hits an atom
reflectRay :: (Int, Int) -> (Int, Int) -> Int -> MovingDirection -> [Atom] -> EdgePoint
reflectRay (x,y) (x1,y1) size dir atoms =
    case dir of
        RU | y == y1    -> moveLeft (x, y) size atoms
           | otherwise  -> moveDownRight (x1, y1) size atoms
        LU | y == y1    -> moveRight (x, y) size atoms
           | otherwise  -> moveDownLeft (x1, y1) size atoms
        DR | y == y1    -> moveLeft (x, y) size atoms
           | otherwise  -> moveRightUp (x, y) size atoms
        DL | y == y1    -> moveRight (x, y) size atoms
           | otherwise  -> moveLeftUp (x, y) size atoms
        RR | odd x1     -> moveLeftUp (x1,y1) size atoms
           | even x1    -> moveDownLeft (x, y) size atoms
        LL | odd x1     -> moveRightUp (x1, y1) size atoms
           | even x1    -> moveDownRight (x, y) size atoms

-- Checks if a position is outside the grid
isOutside :: (Int, Int) -> Int -> Bool
isOutside (x,y) size
    | x > maximumOffset y   = True
    | y > size              = True
    | x < 1                 = True
    | y < 1                 = True
    |otherwise              = False


-- Calculates the exit edge for a ray leaving the grid
triangleToExitEdge :: MovingDirection -> (Int, Int) -> EdgePoint
triangleToExitEdge dir (x,y) =
    case dir of
        RU -> EP East y R
        LU -> EP West y L
        DR -> EP South (offSetToEdge x) R
        DL -> EP South (offSetToEdge x) L
        RR -> EP East y L
        LL -> EP West y R
