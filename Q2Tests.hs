-- Author: EdwardWray
-- Copyright (c) 2024, University of Southampton

import Data.Tuple (swap)
import Data.List ((\\), sort, nub, sortBy, sortOn, intersect)
import Debug.Trace(trace)
import qualified Data.Set as Set
import Data.Ord (comparing, Down(..))


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


--Code from Question 2

-- DO NOT MODIFY THESE DATA TYPES
data EdgeDir = L| R deriving (Eq,Show,Ord)
data Face = East | West | South deriving (Eq,Show,Ord)
data EdgePoint = EP Face Int EdgeDir deriving (Eq,Show,Ord)
type Atom = (Int,Int)


-- Solves the TBB problem and returns atom locations
solveTBB :: Int -> [(EdgePoint,EdgePoint)] -> [Atom]
solveTBB numberOfAtoms observedRays =
        let
            size = findSizeOfGrid observedRays
            candidateAtoms = [[(x, y)] | y <- [1..size], x <- [1..maximumOffset y]]
        in
            map swap (pruningSearch size numberOfAtoms observedRays candidateAtoms)
             --Solution must be swapped as my program views the grid as (offset, row) whereas question wants (row,offset)


-- Determines the size of the grid based on observed rays
findSizeOfGrid :: [(EdgePoint, EdgePoint)] -> Int
findSizeOfGrid edgePairs = maximum [index | (EP _ index _, _) <- edgePairs]


-- Performs a search with pruning to identify valid atom configurations
pruningSearch :: Int -> Int -> [(EdgePoint,EdgePoint)] -> [[Atom]] -> [Atom]
pruningSearch size numberOfAtoms observedRays candidateAtomConfigurations =
    let solution = checkAtomsMatch size observedRays candidateAtomConfigurations
    in if not (null solution) && length solution == numberOfAtoms
           then solution  -- Return the valid solution if found
        else if  null candidateAtomConfigurations || length (head candidateAtomConfigurations) == numberOfAtoms then [] --Check that the number of atoms has not exceeded the limit, or that the candidates list is empty
        else
            let
                prunedCandidates = takeBest size candidateAtomConfigurations observedRays
                newCandidates = concatMap (addNewAtom size) prunedCandidates
            in
                pruningSearch size numberOfAtoms observedRays newCandidates



-- Adds a new atom to each candidate configuration
addNewAtom :: Int  -> [Atom] -> [[Atom]]
addNewAtom size  currentAtoms=
    let possibleNewAtomPositions = [(x, y) | y <- [1..size], x <- [1..maximumOffset y], (x,y) `notElem` currentAtoms]
    in [newAtom : currentAtoms | newAtom <- possibleNewAtomPositions]


-- Checks to see if the list of configurations contains a valid solution
checkAtomsMatch :: Int -> [(EdgePoint,EdgePoint)] -> [[Atom]] -> [Atom]
checkAtomsMatch _ _ [] = []
checkAtomsMatch size observed (candidate:xs) =
    if testEquality (calcInteractions size (map swap candidate)) observed then candidate
    else
        checkAtomsMatch size observed xs


-- Validates whether a single candidate configuration matches observed rays
testEquality :: [(EdgePoint,EdgePoint)] -> [(EdgePoint,EdgePoint)] -> Bool
testEquality observed candidates
    | sort observed == sort candidates = True
    | otherwise = False


-- Prunes the list of candidates to retain the most promising ones
takeBest :: Int -> [[Atom]] -> [(EdgePoint,EdgePoint)] -> [[Atom]]
takeBest size atomConfigs observedRays =  take 10 $ map snd $ sortOn (Down . fst) (scoreCorrectness size atomConfigs observedRays)


-- Scores candidate configurations based on how well they match observed rays
scoreCorrectness :: Int -> [[Atom]] -> [(EdgePoint,EdgePoint)] -> [(Int, [Atom])]
scoreCorrectness _ [] _ = []
scoreCorrectness size (atomConfig:xs) observedRays = (countMatches size atomConfig observedRays, atomConfig) : scoreCorrectness size xs observedRays


-- Counts the number of matching rays for a given atom configuration
countMatches :: Int -> [Atom] -> [(EdgePoint,EdgePoint)] -> Int
countMatches size atomLocations observedRays =
    let generatedRays = calcInteractions size (map swap atomLocations)
    in length (generatedRays `intersect` observedRays)


-- CODE FROM QUESTION 1 --





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
