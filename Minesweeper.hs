module Minesweeper where
import System.Random
import Data.List
import Test.QuickCheck

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

------------------------------------------------------------------------------------------------------------------------
-- DATA TYPES ----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

type Pos = (Int, Int) --row then column

data Flag = Unselected | Flagged | Clear | Mine | Numeric Int
  deriving Eq

newtype Row = Row {flags :: [Flag]}
  deriving Eq

newtype Board = Board {rows :: [Row]}
  deriving Eq

data Minesweeper = Minesweeper { board :: Board, boardSize :: Int, mines :: [Pos]}

------------------------------------------------------------------------------------------------------------------------
-- TYPE CHECKS ---------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Check if a flag is valid
isFlag :: Flag -> Bool
isFlag (Numeric n) = n >= 1 && n <= 8
isFlag _           = True

-- | Check if a row is valid
isRow :: Row -> Bool
isRow (Row [])     = True
isRow (Row (f:fs)) = isFlag f && isRow (Row fs)

-- | Check if a board is valid
isBoard :: Board -> Bool
isBoard (Board []) = True
isBoard (Board rs) = isBoard' rs l
  where l = length rs

-- | Help function to check that the board is a square
isBoard' :: [Row] -> Int -> Bool
isBoard' []     _ = True
isBoard' (r:rs) l = rl == l && isRow r && isBoard' rs l
  where rl = length (flags r)

-- | Check if a minesweeper is valid
isMinesweeper :: Minesweeper -> Bool
isMinesweeper (Minesweeper board size mines) = isBoard board && actualSize == size
                                               && mineAmount == length (nub mines) && checkMines mines size
  where actualSize = length (rows board)
        mineAmount = length mines

-- | Check if the mine placements are valid
checkMines :: [Pos] -> Int -> Bool
checkMines []     _    = True
checkMines ((y,x):ms) size = y >= 0 && y <= (size - 1) && x >= 0 && x <= (size - 1) && checkMines ms size

------------------------------------------------------------------------------------------------------------------------
-- GENERATORS ----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Generate a random flag with equal amount of probablility for any valid value
flag :: Gen Flag
flag = frequency [(9, rNumericFlag), (4, rNonNumericFlag)]

-- | Generate a random valid numeric flag
rNumericFlag :: Gen Flag
rNumericFlag = elements [Numeric n | n <- [1..8]]

-- | Generate a random valid non-numeric flag
rNonNumericFlag :: Gen Flag
rNonNumericFlag = elements [Unselected, Flagged, Clear, Mine]

------------------------------------------------------------------------------------------------------------------------
-- INSTANCES -----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Print a flag
instance Show Flag where
  show (Numeric n) = " " ++ show n ++ " "
  show Unselected  = "[ ]"
  show Flagged     = " F "
  show Clear       = "   "
  show Mine        = " * "

-- | Prints a row, with all its flags
instance Show Row where
  show r | null (flags r) = ""
         | otherwise      = show x ++ show (Row xs)
           where (x:xs) = flags r

-- | Prints a board, with all its flags in their rows
instance Show Board where
  show b | null (rows b) = ""
         | otherwise     = show x ++ "\n" ++ show (Board xs)
           where (x:xs) = rows b

-- | Prints the board, the size and the mine. (Should probably be updated to show all the values, like a cheat)
instance Show Minesweeper where
  show (Minesweeper b s m)
    = show b ++ "\nBoard Size: " ++ show s ++ "\nMines: " ++ show m

------------------------------------------------------------------------------------------------------------------------
-- EXAMPLE TEST DATA ---------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- Test Data
exampleBoard :: Board
exampleBoard = Board [r [ u , u , u , u , u , u , u , u , u ]
                     ,r [ u , u , u , u , u , u , u , u , u ]
                     ,r [ u , u , u , u , u , u , u , u , u ]
                     ,r [ u , u , u , u , u , u , u , u , u ]
                     ,r [ u , u , u , f , u , u , u , u , u ]
                     ,r [ u , u , u , u , u , u , u , u , u ]
                     ,r [ u , u , u , u , u , u , u , u , u ]
                     ,r [ u , u , u , u , u , u , u , u , u ]
                     ,r [ u , u , u , u , u , u , u , u , u ]]
                       where r = Row
                             n = Numeric
                             u = Unselected
                             f = Flagged

exampleBoard2 :: Board
exampleBoard2 = Board [r [ u , u , u ]
                      ,r [ u , u , u ]
                      ,r [ u , u , u ]]
                      where r = Row
                            n = Numeric
                            u = Unselected

exampleBoardSize :: Int
exampleBoardSize = 9

exampleMines :: [Pos]
exampleMines = [(0,0),(1,0),(5,6),(7,2),(2,4),(5,1),(3,3),(5,8)] -- 7 mines in total

example = Minesweeper exampleBoard exampleBoardSize exampleMines
example2 = Minesweeper exampleBoard2 3 []

------------------------------------------------------------------------------------------------------------------------
-- SQUARE CLEARING -----------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

cl :: Minesweeper -> Pos -> [Minesweeper]
cl m p = map (clearSquare m) (getUncheckedPositions m p)

clearSquare :: Minesweeper -> Pos -> Minesweeper
clearSquare m p
  | hasHitMine m p = setValue m p Mine --set value to mine
  | n /= 0         = setValue m p (Numeric n) --show the number of mines in the local square
  | otherwise      = cleared m p
  where b = board m
        n = numMinesInSquare m p

-- | Basically only makes a list, of size 1, of the position given and sends it to cleared'
cleared :: Minesweeper -> Pos -> Minesweeper
cleared m p
  | getValue m p == Unselected
    || getValue m p == Flagged = cleared' m [p]
  | otherwise                  = m

-- | Unveils the cells (and their neighbours if clear)
cleared' :: Minesweeper -> [Pos] -> Minesweeper
cleared' m [] = m
cleared' m ((y,x):ps)
  | getValue m p /= Unselected
    && getValue m p /= Flagged = cleared' m ps
  | n == 0                     = cleared' (setValue m p Clear) (nub (ps ++ getUncheckedPositions m p))
  | otherwise                  = cleared' (setValue m p (Numeric n)) ps
  where n = numMinesInSquare m p
        p = (y,x)

getUncheckedPositions :: Minesweeper -> Pos -> [Pos]
getUncheckedPositions m p = getUncheckedPositions' m (getSquarePositions m p)

getUncheckedPositions' :: Minesweeper -> [Pos] -> [Pos]
getUncheckedPositions' m [] = []
getUncheckedPositions' m (p:ps)
  | getValue m p == Unselected = p : getUncheckedPositions' m ps
  | otherwise                  = getUncheckedPositions' m ps


setFlagged :: Minesweeper -> Pos -> Minesweeper
setFlagged m p = setValue m p Flagged

------------------------------------------------------------------------------------------------------------------------
-- GET BOARD POSITIONS -------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Returns all positions which cells contain the character code for mine
getPosMine :: Minesweeper -> [Pos]
getPosMine = getPositions (== Mine)

-- | Returns all positions which cells contain the character code for clear
getPosClear :: Minesweeper -> [Pos]
getPosClear = getPositions (== Clear)

-- | Returns all positions which cells contain the character code for unselected
getPosUnselected :: Minesweeper -> [Pos]
getPosUnselected = getPositions (== Unselected)

-- | Return all positions which cells contain the character code for flagged
getPosFlagged :: Minesweeper -> [Pos]
getPosFlagged = getPositions (== Flagged)

-- | Get all positions of cells that match the given function
getPositions :: (Flag -> Bool) -> Minesweeper -> [Pos]
getPositions f m = [ (rowNum,colNum) | (rowNum,row) <- zip [0 .. ] bo, (colNum,value) <- zip [0 .. ] (flags row), f value ]
  where bsize = boardSize m
        bo = rows (board m)

------------------------------------------------------------------------------------------------------------------------
-- GAME RUNTIME CHECKS -------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Determine if a given position is the same position as a mine
hasHitMine :: Minesweeper -> Pos -> Bool
hasHitMine m p = p `elem` mines m

-- | Given a position, find the number of mines in the local square
numMinesInSquare :: Minesweeper -> Pos -> Int
numMinesInSquare m p = length [ x | x <- getSquarePositions m p, x `elem` mines m ]

-- | Returns True if the only remaining cells that are either flagged or unselected match the positions of all the mines, False otherwise
checkWin :: Minesweeper -> Bool
checkWin m =  sort (getPosFlagged m ++ getPosUnselected m) == sort (mines m)

-- | Check if any cell in the minesweeper matches the mine character code (has been selected and set to a mine)
checkLose :: Minesweeper -> Bool
checkLose m = getPosMine m /= []

------------------------------------------------------------------------------------------------------------------------
-- UPDATE  -------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Update a given list by replacing the item at the specified location with the new specified item
-- | This was taken from the Sudoku assignment as the required implementation is exactly the same
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] (_,x) = [x]
(!!=) xs (i,x) = take i xs ++ [x] ++ drop (i+1) xs

-- | Set a board position to a new specified value
setValue :: Minesweeper -> Pos -> Flag -> Minesweeper
setValue m (y,x) flag = m { board = Board (rs !!= (y , Row (flags (rs !! y) !!= (x,flag))))}
  where rs = rows (board m)

------------------------------------------------------------------------------------------------------------------------
-- GETTERS -------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Get a value from a minesweeper board at a specified position
getValue :: Minesweeper -> Pos -> Flag
getValue m (r,c) = flags (rows (board m) !! r) !! c

getSquarePositions :: Minesweeper -> Pos -> [Pos]
getSquarePositions m (y,x) = delete (y,x) [ (a,b) | b <- [(x - 1) .. (x + 1)], a <- [(y - 1) .. (y + 1)], b >= 0 && b < bsize, a >= 0 && a < bsize ]
  where bsize = boardSize m

------------------------------------------------------------------------------------------------------------------------
-- MINESWEEPER CREATION ------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Make a new clear board of a specified size
mkBoard :: Int -> Board
mkBoard bsize = Board (replicate bsize $ Row (replicate bsize Unselected))

-- | Make new random mine locations.
mkMineLocations :: Int -> Int -> StdGen -> [Pos]
mkMineLocations bsize numPos g = nub $ getLocations bsize numPos g
  where getLocations :: Int -> Int -> StdGen -> [Pos]
        getLocations bsize numPos g
          | numPos == 0 = []
          | otherwise   = (y,x) : getLocations bsize (numPos - 1) g2
          where (x,g1) = randomR (0, bsize -1) g
                (y,g2) = randomR (0, bsize -1) g1

-- | Make a new clear minesweeper board with randomised mine locations
mkMinesweeper :: Int -> Int -> StdGen -> Minesweeper
mkMinesweeper bsize numMines g = Minesweeper (mkBoard bsize) bsize (mkMineLocations bsize numMines g)

------------------------------------------------------------------------------------------------------------------------
-- Property based testing ----------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
