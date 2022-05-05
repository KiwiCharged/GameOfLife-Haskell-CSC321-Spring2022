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
dimensions = 3

-- Example Grid of cells (3x3)
{-
|#|#| | |
|#| |#|#|
|#|#| |#|
| |#| | |
-}
example :: Grid
example = [True, True, False, True, False, True, True, True, False]


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

-- Takes in Grid array and desired index, and outputs True/False state of cell as 1 or 0
boolToInt :: Grid -> Int -> Int
boolToInt grid idx = do
    if grid !! idx
        then 1
    else 0

-- Takes current cell state at specified index and outputs next-gen livingstate
determineCellState :: Grid -> Int -> Bool 
determineCellState grid i = do
    let numOfTopNeighbors = boolToInt grid (i-dimensions-1) + boolToInt grid (i-dimensions) + boolToInt grid (i-dimensions+1)
    let numOfSideNeighbors = boolToInt grid (i-1) + boolToInt grid (i+1)
    let numOfBottomNeighbors = boolToInt grid (i+dimensions-1) + boolToInt grid (i+dimensions) + boolToInt grid (i+dimensions+1)
    let numOfLivingNeighbors = numOfTopNeighbors + numOfSideNeighbors + numOfBottomNeighbors
    not (numOfLivingNeighbors < 2 || numOfLivingNeighbors > 3) -- modify this to check dead/liveness of cell







---------------------------------------------------------------
-- Test stuff below, irrelevant to main program...

-- addEx :: Int
-- addEx = 5 + 4

-- Input x,y coordinate to return the associated index in a 1D array, based on "dimensions"
-- getIndexFromCoordinate :: Int -> Int -> Int
-- getIndexFromCoordinate x y = x + y -- don't think we need to actually do this...