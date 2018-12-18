module Minesweeper where
import System.Random
import Data.List
import Data.Char
import Test.QuickCheck

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
isMinesweeper (Minesweeper b s m) = isBoard b && actualSize == s
                                    && mineAmount == length (nub m) && checkMines (Minesweeper b s m)
  where actualSize = length (rows b)
        mineAmount = length m

-- | Check if the mine placements are valid
checkMines :: Minesweeper -> Bool
checkMines (Minesweeper _ _ [])         = True
checkMines (Minesweeper b s ((y,x):ms)) = y >= 0 && y <= (s - 1) && x >= 0 && x <= (s - 1)
                                          && (value == Unselected || value == Flagged || value == Mine)
                                          && checkMines (Minesweeper b s ms)
                                            where value = flags (rows b !! y) !! x

------------------------------------------------------------------------------------------------------------------------
-- VALIDATION CHECKS ---------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Check if the flag in a position of a minesweeper has a valid value
isFlagValid :: Minesweeper -> Pos -> Bool
isFlagValid m p -- Check if a square has a valid value
  | v == Clear && n == 0 = True
  | v == Numeric n       = True
  | v == Mine            = hasHitMine m p
  | otherwise            = False
    where n = numMinesInSquare m p
          v = getValue m p

-- | Check if a minesweeper, when revealed, contains only valid values
isValidMinesweeper :: Minesweeper -> Bool
isValidMinesweeper m = isValidMinesweeper' (reveal m) (0,0) -- Reveals the minesweeper

isValidMinesweeper' :: Minesweeper -> Pos -> Bool
isValidMinesweeper' m (y,x) -- Check if each square has a valid value
  | x /= maxIndex = isValid && isValidMinesweeper' m (y,x+1)
  | y /= maxIndex = isValid && isValidMinesweeper' m (y+1,0)
  | otherwise     = isValid
    where maxIndex  = boardSize m - 1
          isValid   = isFlagValid m (y,x)

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

exampleBoardSize :: Int
exampleBoardSize = 9

exampleMines :: [Pos]
exampleMines = [(0,0),(1,0),(5,6),(7,2),(2,4),(5,1),(3,3),(5,8)] -- 7 mines in total

example = Minesweeper exampleBoard exampleBoardSize exampleMines

------------------------------------------------------------------------------------------------------------------------
-- SQUARE CLEARING -----------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Given a position on the board, attempt to clear it. If theres mines in its immediate square, set the position to show the number of mines.
--   If the position matches an element within the list of mines, set it to show that the user has hit a mine.
--   Otherwise, set it to blanks and recursively check all the other immediate squares for blanks
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
  | v == Unselected || v == Flagged = cleared' m [p]
  | otherwise                       = m
    where v = getValue m p

-- | Unveils the cells (and their neighbours if clear)
cleared' :: Minesweeper -> [Pos] -> Minesweeper
cleared' m [] = m
cleared' m ((y,x):ps)
  | n == 0                          = cleared' (setValue m p Clear) (nub (ps ++ getUncheckedPositions m p))
  | otherwise                       = cleared' (setValue m p (Numeric n)) ps
    where v = getValue m p
          n = numMinesInSquare m p
          p = (y,x)

-- | Get a list of the positions in a given positions immediate area on the condition that they are flagged or unselected
getUncheckedPositions :: Minesweeper -> Pos -> [Pos]
getUncheckedPositions m p = getUncheckedPositions' m (getSquarePositions m p)

getUncheckedPositions' :: Minesweeper -> [Pos] -> [Pos]
getUncheckedPositions' m [] = []
getUncheckedPositions' m (p:ps)
  | v == Unselected || v == Flagged = p : getUncheckedPositions' m ps
  | otherwise                       = getUncheckedPositions' m ps
    where v = getValue m p

-- | Set a position as flagged
setFlagged :: Minesweeper -> Pos -> Minesweeper
setFlagged m p = setValue m p Flagged

-- | Set a position as unselected
setUnselected :: Minesweeper -> Pos -> Minesweeper
setUnselected m p = setValue m p Unselected

-- | Reveals the values of an minesweeper
reveal :: Minesweeper -> Minesweeper
reveal m = reveal' m (0,0)

reveal' :: Minesweeper -> Pos -> Minesweeper
reveal' m (y,x)
  | x /= maxIndex = reveal' clearNext (y,x+1)
  | y /= maxIndex = reveal' clearNext (y+1,0)
  | otherwise     = clearNext
    where maxIndex  = boardSize m - 1
          clearNext = clearSquare m (y,x)

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

-- | Get the list of positions of the cells in a given positions immediate area, not including the given position
getSquarePositions :: Minesweeper -> Pos -> [Pos]
getSquarePositions m (y,x) = delete (y,x) [ (a,b) | b <- [(x - 1) .. (x + 1)], a <- [(y - 1) .. (y + 1)], b >= 0 && b < bsize, a >= 0 && a < bsize ]
  where bsize = boardSize m

-- | Get a list for the mines' positions in a given board
getMines :: Board -> [Pos]
getMines b = getMines' (rows b) 0

-- | Get a list for the mines' positions in given rows
getMines' :: [Row] -> Int -> [Pos]
getMines' []     _ = []
getMines' (r:rs) i
  | mineAmount > 0 = zip (replicate mineAmount i) xPoss ++ next
  | otherwise      = next
  where xPoss      = getMines'' (flags r) 0
        mineAmount = length xPoss
        next       = getMines' rs (i+1)

-- | Get a list for the mines' indexes in a given row
getMines'' :: [Flag] -> Int -> [Int]
getMines'' []     _ = []
getMines'' (f:fs) i
  | f == Mine = i : next
  | otherwise = next
  where next = getMines'' fs (i+1)

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
-- INSTANCES -----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Generate an arbitrary flag
instance Arbitrary Flag where
  arbitrary = rFlag

-- | Generate an arbitrary row
instance Arbitrary Row where
  arbitrary = rRow

-- | Generate an arbitrary board
instance Arbitrary Board where
  arbitrary = rBoard

-- | Generate an arbitrary minesweeper
instance Arbitrary Minesweeper where
  arbitrary = rMinesweeper

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

-- | Prints the board, the size and the mine
instance Show Minesweeper where
  show (Minesweeper b s m)
    = show b ++ "\nBoard Size: " ++ show s ++ "\nMines: " ++ show m

------------------------------------------------------------------------------------------------------------------------
-- GENERATORS ----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Generate a random flag with equal amount of probablility for any valid value
rFlag :: Gen Flag
rFlag = frequency [(9, rNumericFlag), (4, rNonNumericFlag)]

-- | Generate a random valid numeric flag
rNumericFlag :: Gen Flag
rNumericFlag = elements [Numeric n | n <- [1..8]]

-- | Generate a random valid non-numeric flag
rNonNumericFlag :: Gen Flag
rNonNumericFlag = elements [Unselected, Flagged, Clear, Mine]

-- | Generate a random list of flags, of a specified length
rFlags :: Int -> Gen [Flag]
rFlags i = vectorOf i rFlag

-- | Genereate a random Minesweeper size between 3 and 20
rSize :: Gen Int
rSize = elements [3..20]

-- | Generate a random row (not necessarily valid for the game)
rRow :: Gen Row
rRow = do i <- rSize
          fs <- rFlags i
          return (Row fs)

-- | Generate a random row, of specified size (not necessarily valid for the game)
rRow' :: Int -> Gen Row
rRow' i = do fs <- rFlags i
             return (Row fs)

-- | Generate a random list of rows, of a specified size
rRows :: Int -> Gen [Row]
rRows i = vectorOf i (rRow' i)

-- | Generate a random board (not necessarily valid for the game)
rBoard :: Gen Board
rBoard = do i <- rSize
            rs <- rRows i
            return (Board rs)

-- | Generate a random minesweeper (not necessarily valid for the game)
rMinesweeper :: Gen Minesweeper
rMinesweeper = do b <- rBoard
                  let s = length (rows b)
                  return (Minesweeper b s (getMines b))

------------------------------------------------------------------------------------------------------------------------
-- PROPERTY BASED TESTS ------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Check if given minesweeper is a minesweeper
prop_minesweeper :: Minesweeper -> Bool
prop_minesweeper = isMinesweeper

-- | Check if clearSquare clears squares, in random minesweeper, in valid manner
prop_clear_square :: Int -> Int -> Int -> Bool
prop_clear_square 0 0 g = prop_clear_square 1 0 g
prop_clear_square a b g
  |  i > j    = prop_clear_square' i j g -- Make sure the bigger value is the board size
  | otherwise = prop_clear_square' j i g
    where i = abs a -- Makes sure that no negative values appears for board size and mine amount
          j = abs b

prop_clear_square' :: Int -> Int -> Int -> Bool
prop_clear_square' bsize numMines g = isValidMinesweeper m
  where m = mkMinesweeper bsize numMines (mkStdGen g) -- Generates a random minesweeper

fewerChecks prop = quickCheckWith stdArgs{ maxSuccess = 30 } prop
