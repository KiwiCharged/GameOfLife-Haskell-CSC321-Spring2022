import Data.List ()
import System.IO ()
import System.Random (randoms, mkStdGen)

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

-- Square dimension (length of square side) of life grid
dimensions :: Int
dimensions = 4

-- Test Grid of cells (3x3)
{-
|#|#| | |
|#| |#|#|
|#|#| |#|
| |#| | |
-}
example :: Grid
example = [True, True, False, False,    True, False, True, True,    False, True, False, True,    False, True, False, False]
example = [1, 1, 0, 0,    1, 0, 1, 1,    0, 1, 0, 1,    0, 1, 0, 0]

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
determineCellState :: Int -> Grid -> Bool
determineCellState i grid = do
    let numOfTopNeighbors = (boolToInt grid (i-dimensions-1))*(topOverlap i)*(leftOverlap i) + (boolToInt grid (i-dimensions))*(topOverlap i) + (boolToInt grid (i-dimensions+1))*(topOverlap i)*(rightOverlap i)
    let numOfSideNeighbors = (boolToInt grid (i-1))*(leftOverlap i) + (boolToInt grid (i+1))*(rightOverlap i)
    let numOfBottomNeighbors = (boolToInt grid (i+dimensions-1))*(bottomOverlap i)*(leftOverlap i) + (boolToInt grid (i+dimensions))*(bottomOverlap i) + (boolToInt grid (i+dimensions+1))*(bottomOverlap i)*(rightOverlap i)
    let numOfLivingNeighbors = numOfTopNeighbors + numOfSideNeighbors + numOfBottomNeighbors
    if grid !! i -- if cell is alive
        then not (numOfLivingNeighbors < 2 || numOfLivingNeighbors > 3) -- if <2 or >3 neighbors, returns False (dead) state for the cell
    else numOfLivingNeighbors == 3 -- if cell is dead, and has exactly 3 neighbors, then returns True (alive) state for cell

-- ------------------------------------------------------------------------------------

-- core function
-- traverse each cell in the grid, 16 cells in total, to re-evaluate the cell value.
evolution y = [ determineCellState idx y | idx <- [0..((dimensions*dimensions)-1)]]

-- Display the grid 4 items in a row
mkGrid :: Grid -> String
mkGrid [] = ""
mkGrid (x1:x2:x3:x4:xs) =  "|" ++ boolToSymbol x1 ++ "|" ++ boolToSymbol x2 ++ "|" ++ boolToSymbol x3 ++ "|" ++ boolToSymbol x4 ++ "|" ++ "\n" ++ mkGrid xs


boolToSymbol :: Bool -> String
boolToSymbol bool = do
    if bool
        then "#"
    else " "

-- print grid
prtGrid y = putStrLn (mkGrid y)

-- Pauses for specified amount of milliseconds between each evolution
wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n*1000]]

-- loop in main()
playGame :: Int -> Grid -> IO()
playGame n grid
    | n == 0  = putStrLn "Game over."
    | null grid = error "No data"
    | n > 0  = do 
        prtGrid grid 
        wait 800
        playGame (n-1) (evolution grid)

main :: IO()
main =  do
    -- putStr "Enter an integer number as the random seed: "
    -- x <- getLine
    putStr "Using example starting grid: \n"
    let initGrid = example
    prtGrid initGrid 

    putStr "Enter how many evolutions you want to play: "
    x <- getLine
    let n = read x::Int
    playGame n (evolution initGrid)














-- Printing function to display Grid to console properly [IN PROGRESS]
-- printGrid 0 = do
--     putStrLn ""
--     return ()

-- printGrid n = do
--     if n `rem` dimensions == 0
--         then putStrLn ""
--     else putStr "|"
--     printGrid (n-1)



-- Recursive evolution function to check all cell states and append new state to new list [IN PROGRESS]
--nextGeneration :: IndexedGrid -> IndexedGrid
--nextGeneration = 

-- Main program
-- main :: IO()
-- main = do
--     putStr "Enter size of grid (measured by length of one side): "
--     userInput <- getLine
--     let dimensions = read userInput :: Int
--     determineCellState example 5
