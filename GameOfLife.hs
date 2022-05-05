import Data.List ()
import System.IO ()

-- Conway's "Game of Life" Haskell implementation by Justin Karp

{-

Example formatting for grid printing:
['#' is living cell; ' ' is dead]

|#| | |#|#|#|#| | | |
| | |#| | | |#|#| | |
| | |#| |#|#|#|#| | |
|#|#|#|#|#| |#|#|#|#|
| |#| |#|#|#|#|#| |#|
|#| |#| |#|#|#|#|#|#|
|#| |#|#| |#| |#| | |
|#| |#|#|#|#|#|#|#|#|
| | |#|#| |#|#|#|#|#|
|#| |#|#|#|#|#|#|#|#|
|#| | | | |#|#| |#| |

Algorithm to calculate state of a cell:
1) find cell position
2) check state of all 8 surrounding cells, and count total amount of living cells
3) use count of living cells to determine new generation state of original center cell
    3a) If center cell is living, >3 live cells or <2 live cells around it means death.
    3b) Otherwise, it continues to live.
    3c) If center cell is dead, exactly 3 live cells around it means it becomes alive

-}

-- Grid will be an array of booleans where TRUE is living cell and FALSE is dead cell
type Grid = [Bool]

-- Square dimensions of life grid
dimensions :: Int
dimensions = 4

-- Example Grid of cells (3x3)
{-
|#|#| | |
|#| |#|#|
|#|#| |#|
| |#| | |
-}
example :: Grid
example = [True, True, False, False,    True, False, True, True,    True, True, False, True,    False, True, False, False]


-- Printing function to display Grid to console properly
printGrid 0 = do
    putStrLn ""
    return ()

printGrid n = do
    if n `rem` dimensions == 0
        then putStrLn ""
    else putStr "|"
    printGrid (n-1)


{-

1) Find new state of cell (check 8 cells around, store amount of living cells to variable)
2) Append new state Bool to a new Grid list
3) Output new Grid list once all states have been determined and appended

>have main function doing the recursion/iteration, in some way tracking which index you're at
>have smaller subfunction that takes whole grid input, current index, 

-}

-- Takes in Grid array and desired index, and converts True/False state of cell to 1s and 0s
boolToInt :: Grid -> Int -> Int
boolToInt grid idx = do
    if idx < 0 || idx > (dimensions*dimensions - 1)
        then 0
    else if grid !! idx
        then 1
    else 0

-- Four functions to check out-of-bounds index locations in reference to the "Grid" layout
leftOverlap :: Int -> Int
leftOverlap idx = do
    if idx `rem` dimensions == 0
        then 0
    else 1

rightOverlap :: Int -> Int
rightOverlap idx = do
    if idx `rem` dimensions == (dimensions-1)
        then 0
    else 1

topOverlap :: Int -> Int
topOverlap idx = do
    if (idx - dimensions) < 0
        then 0
    else 1

bottomOverlap :: Int -> Int
bottomOverlap idx = do
    if (idx + dimensions) > (dimensions*dimensions) -- essentially Grid array length
        then 0
    else 1

-- Takes current cell state at specified index and outputs next-gen living state
determineCellState :: Grid -> Int -> Bool
determineCellState grid i = do
    let numOfTopNeighbors = (boolToInt grid (i-dimensions-1))*(topOverlap i)*(leftOverlap i) + (boolToInt grid (i-dimensions))*(topOverlap i) + (boolToInt grid (i-dimensions+1))*(topOverlap i)*(rightOverlap i)
    let numOfSideNeighbors = (boolToInt grid (i-1))*(leftOverlap i) + (boolToInt grid (i+1))*(rightOverlap i)
    let numOfBottomNeighbors = (boolToInt grid (i+dimensions-1))*(bottomOverlap i)*(leftOverlap i) + (boolToInt grid (i+dimensions))*(bottomOverlap i) + (boolToInt grid (i+dimensions+1))*(bottomOverlap i)*(rightOverlap i)
    let numOfLivingNeighbors = numOfTopNeighbors + numOfSideNeighbors + numOfBottomNeighbors
    if grid !! i -- if cell is alive
        then not (numOfLivingNeighbors < 2 || numOfLivingNeighbors > 3) -- if <2 or >3 neighbors, returns False (dead) state for the cell
    else numOfLivingNeighbors == 3 -- if cell is dead, and has exactly 3 neighbors, then returns True (alive) state for cell



