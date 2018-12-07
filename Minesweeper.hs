module Minesweeper where
import System.Random
import Data.List

{-
Definitions for developers benefit

A minesweeper board size is set by the user, n. A board is therefore defined as being n tiles wide and n tiles high

A tile can take multiple forms.
  - A user can "flag" a tile, this means they believe that there is a mine at this tiles location.
  - A user can select to clear a tile, if the tile is revealed to be a mine, the user loses the game.
    However, if it is not a mine, the number of mines in the surrounding tiles are shown at the selected tiles location.
    If there are not bombs in the local area, the tile must be clear.
    Surrounding tiles must then also be checked if they are clear of bombs and their surrounding tiles must be checks so on and so forth.

Mines are placed at random locations around the board at the start and are not revealed until the end of the game.

The user wins the game if they have selected all tiles that are not mines
  (NOT if they have flagged all mine locations, otherwise flagging all locations would cause the user to win)
-}

type Flag = String
type Board = [[Flag]]
type Pos = (Int, Int) --column then row

data Minesweeper = Minesweeper { board :: Board, boardSize :: Int, mines :: [Pos]}
  deriving Show

-- Test Data
exampleBoard :: Board
exampleBoard = [["[]","[]","[]","[]","[]","[]","[]","[]"," 2"]
               ,["[]","[]","[]","[]","[]","[]","[]"," 5","[]"]
               ,["[]","[]","[]","[]","[]","[]","[]","[]"," 9"]
               ,["[]","[]","[]","[]","[]","[]","[]","[]","[]"]
               ,["[]","[]","[]"," 4"," 1","[]","[]","[]","[]"]
               ,["[]","[]","[]","[]"," 2","[]","[]","[]","[]"]
               ,["[]","[]","[]","[]","[]","[]","[]","[]","[]"]
               ,["[]","[]","[]","[]","[]","[]","[]","[]","[]"]
               ,["[]","[]","[]","[]","[]","[]","[]","[]","[]"]]

exampleBoardSize :: Int
exampleBoardSize = 9

exampleMines :: [Pos]
exampleMines = [(0,0),(1,0),(5,6),(7,2),(2,4),(5,1),(3,3)] -- 7 mines in total

example = Minesweeper exampleBoard exampleBoardSize exampleMines

hasHitMine :: Minesweeper -> Pos -> Bool
hasHitMine m p = p `elem` (mines m)

------------------------------------------------------------------------------------------------------------------------
-- SETTERS -------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Update a given list by replacing the item at the specified location with the new specified item
-- | This was taken from the Sudoku assignment as the required implementation is exactly the same
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] (_,x) = [x]
(!!=) xs (i,x) = (take i xs) ++ [x] ++ (drop (i+1) xs)

-- | Set a board position to a new specified value
setValue :: Minesweeper -> Pos -> String -> Minesweeper
setValue m (x,y) string = m { board = (b !!= (y , (b !! y) !!= (x,string)))}
  where b = board m

------------------------------------------------------------------------------------------------------------------------
-- GETTERS -------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Get a value from a minesweeper board at a specified position
getValue :: Minesweeper -> Pos -> String
getValue m (c,r) = ((board m) !! r) !! c

getSquarePositions :: Minesweeper -> Pos -> [Pos]
getSquarePositions m (x2,y2)
  | y3 >= bsize && x3 >= bsize = [(x1,y1),(x2,y1),(x1,y2),(x2,y2)]
  | y1 < 0 && x1 < 0           = [(x2,y2),(x3,y2),(x2,y3),(x3,y3)]
  | y3 >= bsize && x1 < 0      = [(x2,y1),(x3,y1),(x2,y2),(x3,y2)]
  | y1 < 0 && x3 >= bsize      = [(x1,y2),(x2,y2),(x1,y3),(x2,y3)]
  | otherwise                  = [(x1,y1),(x2,y1),(x3,y1),(x1,y2),(x2,y2),(x3,y2),(x1,y3),(x2,y3),(x3,y3)]
  where bsize = boardSize m
        (x1,x3,y1,y3) = (x2-1,x2+1,y2-1,y2+1)

------------------------------------------------------------------------------------------------------------------------
-- MINESWEEPER CREATION ------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Make a new clear board of a specified size
mkBoard :: Int -> Board
mkBoard bsize = replicate bsize $ replicate bsize "[]"

-- | Make new random mine locations.
mkMineLocations :: Int -> Int -> StdGen -> [Pos]
mkMineLocations bsize numPos g = nub $ getLocations bsize numPos g
  where getLocations :: Int -> Int -> StdGen -> [Pos]
        getLocations bsize numPos g
          | numPos == 0 = []
          | otherwise   = (x,y) : getLocations bsize (numPos - 1) g2
          where (x,g1) = randomR (0, bsize -1) g
                (y,g2) = randomR (0, bsize -1) g1

-- | Make a new clear minesweeper board with randomised mine locations
mkMineSweeper :: Int -> Int -> StdGen -> Minesweeper
mkMineSweeper bsize numMines g = Minesweeper (mkBoard bsize) bsize (mkMineLocations bsize numMines g)

{-
TODO

Create Minesweeper data type
create show instance for minesweeper board
create cell data type
create test cases isMinesweeper etc
create randomise bomb locations (choose difficulty to select number of bombs?)


-}

