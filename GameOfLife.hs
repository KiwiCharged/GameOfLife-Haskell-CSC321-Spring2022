import Data.List ()
import System.IO ()
import System.Random (randoms, mkStdGen)

-- Conway's "Game of Life" Haskell implementation by Justin Karp
-- This Game of Life implementation supports infinite grid/canvas sizes!

{-
Example formatting for grid printing:
['#' is living cell; ' ' is dead]

|#| | |#|#|#|#| |
| | |#| | | |#|#|
| | |#| |#|#|#|#|
|#|#|#|#|#| |#|#|
| |#| |#|#|#|#|#|
|#| |#| |#|#|#|#|
|#| |#|#| |#| |#|
|#| |#|#|#|#|#|#|
| | |#|#| |#|#|#|

Algorithm to calculate state of a cell:
1) find cell position
2) check state of all 8 surrounding cells, and count total amount of living cells
3) use count of living cells to determine new generation state of original center cell
    3a) If center cell is living, >3 live cells or <2 live cells around it means death.
    3b) Otherwise, it continues to live.
    3c) If center cell is dead, exactly 3 live cells around it means it becomes alive
-}

-- Square dimension (length of square side) of life grid
-- MODIFY THIS TO SET THE SIZE OF THE GAME OF LIFE GRID.
dimensions :: Int
dimensions = 10

-- Determines delay between each new evolution, to be able to watch the process in real time.
-- MODIFY THIS TO CHANGE DELAY.
delayBetweenEvolutions :: Int
delayBetweenEvolutions = 500


-------- DO NOT MODIFY ANY OTHER CODE BELOW, ALL ELSE IS NEEDED FOR PROPER FUNCTIONING --------


-- Total cell count, determined from dictated dimensions
totalCells :: Int
totalCells = dimensions*dimensions

-- Grid will be an array of Integers where 1 is living cell and 0 is dead cell
type Grid = [Int]

-- Test Grid of cells (4x4) [USED FOR DEBUGGING. DO NOT USE OTHERWISE.]
{-
|#|#| | |
|#| |#|#|
| |#| |#|
| |#| | |
-}
example :: Grid
example = [1, 1, 0, 0,    1, 0, 1, 1,    0, 1, 0, 1,    0, 1, 0, 0]

-- Generate n number of random 0s and 1s
-- The function uses the following built-in function
-- randoms :: (Random a, RandomGen g) => g -> [a]
genRand01:: Int -> [Int]
genRand01 n =
    map ((`mod` 2) . abs) (take totalCells randInfList)
    where
      randInfList = randoms (mkStdGen n) :: [Int]

-- Returns state of cell at index, with check to make sure no outOfBounds indexes are used
getCell :: Grid -> Int -> Int
getCell grid idx = do
    if idx < 0 || idx > (totalCells - 1)
        then 0
    else grid !! idx

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
    if (idx + dimensions) > totalCells -- essentially Grid array length
        then 0
    else 1

-- Takes current cell state at specified index and outputs next-gen living state
determineCellState :: Int -> Grid -> Int
determineCellState i grid = do
    let numOfTopNeighbors = (getCell grid (i-dimensions-1))*(topOverlap i)*(leftOverlap i) + (getCell grid (i-dimensions))*(topOverlap i) + (getCell grid (i-dimensions+1))*(topOverlap i)*(rightOverlap i)
    let numOfSideNeighbors = (getCell grid (i-1))*(leftOverlap i) + (getCell grid (i+1))*(rightOverlap i)
    let numOfBottomNeighbors = (getCell grid (i+dimensions-1))*(bottomOverlap i)*(leftOverlap i) + (getCell grid (i+dimensions))*(bottomOverlap i) + (getCell grid (i+dimensions+1))*(bottomOverlap i)*(rightOverlap i)
    let numOfLivingNeighbors = numOfTopNeighbors + numOfSideNeighbors + numOfBottomNeighbors
    if grid !! i == 1 -- if cell is alive
        then if numOfLivingNeighbors < 2 || numOfLivingNeighbors > 3 -- if <2 or >3 neighbors, returns 0 (dead) state for the cell
            then 0
        else 1
    else if numOfLivingNeighbors == 3 -- if cell is dead, and has exactly 3 neighbors, then returns 1 (alive) state for cell
        then 1
    else 0

-- core function
-- traverse each cell in the grid, 16 cells in total, to re-evaluate the cell value.
evolution y = [ determineCellState idx y | idx <- [0..(totalCells-1)]]

-- Converts state of cell (1 or 0) to appropriate symbol ('#' or ' ')
intToSymbol :: Int -> String
intToSymbol int = do
    if int == 1
        then "#"
    else " "

-- Call recursive function and print resulting grid
printGrid y = putStrLn (listToPrettyGridString totalCells y "")

-- Converts list of integers to a pretty grid with symbols and seperators for the "cells"
listToPrettyGridString :: Int -> Grid -> String -> String
listToPrettyGridString n grid gridString
    | n == 0 = gridString
    | n > 0 = do
        let cellAsSymbol = intToSymbol (grid !! (totalCells-n))
        let newString = gridString ++ "|" ++ cellAsSymbol
        if (n-1) `mod` dimensions == 0
            then listToPrettyGridString (n-1) grid (newString ++ "|\n")
        else listToPrettyGridString (n-1) grid newString

-- Pauses for specified amount of milliseconds between each evolution
wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n*1000]]

-- loop in main()
playGame :: Int -> Grid -> IO()
playGame n grid
    | n == 0  = putStrLn "Game over. No more iterations left."
    | all (==0) grid = do
        printGrid grid -- print grid -- change back to prtGrid grid
        putStrLn "Everyone died. The end."
    | n > 0  = do 
        printGrid grid -- print grid -- change back to prtGrid grid
        wait delayBetweenEvolutions
        playGame (n-1) (evolution grid)

main :: IO()
main =  do
    -- putStr "Using example starting grid: \n"
    -- let initGrid = example
    -- prtGrid initGrid 
    putStr "Enter an integer number as the random seed: "
    x <- getLine
    let initGrid = genRand01 (read x :: Int)
    putStr "\nInitial grid: \n"
    printGrid initGrid -- print initGrid -- change back to prtGrid grid

    putStr "Enter how many evolutions you want to play: "
    x <- getLine
    let iterations = read x::Int
    playGame iterations (evolution initGrid)