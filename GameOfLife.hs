import Data.List ()
import System.IO ()

-- Conway's "Game of Life" Haskell implementation by Justin Karp

{-

Example formatting for grid printing:
[# is living cell; empty \space\ is dead]

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

-- Creating a coordinate type which contains an (x,y) position in the grid
type Position = (Int, Int)

-- Creating a grid type with contains an 1D array of positions
type Grid = [Position]

-- Square dimensions of life grid
dimensions :: Int 
dimensions = 10

-- Input x,y coordinate to return the associated index in a 1D array, based on "dimensions"
getIndexFromCoordinate :: Int -> Int -> Int
getIndexFromCoordinate x y = x + y



---------------------------------------------------------------
-- Test stuff below, irrelevant to main program...

addEx :: Int
addEx = 5 + 4

