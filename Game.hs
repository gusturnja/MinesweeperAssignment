module Game where
import Minesweeper
import System.Random
import Data.List
import System.Exit

-- | Create a new minesweeper and being the game loop
main = do
  putStrLn "Welcome To MineSweeper!"
  main'

main' = do
  g <- newStdGen
  putStrLn "Do you want to load a saved game? (y/n)"
  load <- getLine
  case load of
    "y" -> do
      putStrLn "Please enter filepath:"
      fp <- getLine
      m <- readMinesweeper fp
      loadSave fp m
    "n" -> do
      putStrLn "Please enter desired board size:"
      r_bsize <- getLine
      let bsize = read r_bsize :: Int
      putStrLn $ "Selected " ++ show bsize ++ " as board size"
      putStrLn "Please enter the number of mines"
      r_msize <- getLine
      let msize = read r_msize :: Int
          m = mkMinesweeper bsize msize g
      gameLoop m
    _ -> do
      putStrLn "I'm sorry, please enter either y or n"
      main'
  putStrLn "Thanks for playing!"

-- | Loads a minesweeper
loadSave :: String -> Maybe Minesweeper -> IO ()
loadSave fp Nothing = do
  putStrLn ("File: \"" ++ fp ++ "\", does not exist!")
  main'
loadSave _ (Just m)
  | isMinesweeper m = gameLoop m
  | otherwise = putStrLn "Invalid minesweeper, quitting..."

-- | Loop the game until the user either wins, loses or exits the game
gameLoop :: Minesweeper -> IO ()
gameLoop m = do
              putStrLn ""
              printMinesweeper m
              if checkWin m
              then
                putStrLn "Congrats, you win!"
              else
                if checkLose m
                then do
                  printMinesweeper (reveal m)
                  putStrLn "You lose!"
                else do
                  putStrLn "c -    Clear a Cell"
                  putStrLn "f -     Flag a Cell"
                  putStrLn "u -   Remove a Flag"
                  putStrLn "save - Save Progress"
                  putStrLn "exit -         Quit"
                  putStrLn "Please enter option"
                  input <- getLine
                  case input of
                    "exit" ->
                      exitSuccess
                    "save" -> do
                      putStrLn "Enter save name:"
                      fp <- getLine
                      writeMinesweeper fp m
                      putStrLn ("Game saved as: \"" ++ fp ++ "\"")
                      gameLoop m
                    "c" -> do
                      p <- inputCoord
                      let newm = clearSquare m p
                      gameLoop newm
                    "f" -> do
                      p <- inputCoord
                      if getValue m p /= Unselected
                      then do
                        putStrLn "Cannot set flag here"
                        gameLoop m
                      else do
                        let newm = setFlagged m p
                        gameLoop newm
                    "u" -> do
                      p <- inputCoord
                      if getValue m p /= Flagged
                      then do
                        putStrLn "There is no flag to unflag here"
                        gameLoop m
                      else do
                        let newm = setUnselected m p
                        gameLoop newm
                    _ -> do
                      putStrLn "I'm sorry, please enter either c, f , help or exit"
                      gameLoop m

-- | Take input of a coordinate, must be formatted as a position -> (y,x)
inputCoord :: IO Pos
inputCoord = do
              putStrLn "Please enter the coordinates (row,column)"
              r_input <- getLine
              return (read r_input :: Pos)


------------------------------------------------------------------------------------------------------------------------
-- READ/WRITE FILE -----------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Transfer a given string into a valid row
parseToRow :: String -> Row
parseToRow s = Row (parseToRow' s)

parseToRow' :: String -> [Flag]
parseToRow' [] = []
parseToRow' (a : (b : (c :cs)))
  | f == show Unselected  = Unselected             : parseToRow' cs
  | f == show Flagged     = Flagged                : parseToRow' cs
  | f == show Clear       = Clear                  : parseToRow' cs
  | f == show Mine        = Mine                   : parseToRow' cs
  | otherwise             = Numeric (digitToInt b) : parseToRow' cs
    where f = [a, b, c]

-- | Transfer a given string into a valid list of mine positions
parseToPos :: String -> [Pos]
parseToPos []         = []
parseToPos (y:(x:is)) = (digitToInt y, digitToInt x) : parseToPos is

-- | Read a minesweeper from a given filepath
readMinesweeper :: FilePath -> IO (Maybe Minesweeper)
readMinesweeper fp = do
  exists <- fileExist fp
  if exists then do
    contents <- readFile fp
    let ls = lines contents
        len   = length ls - 1
        rows  = take len ls
        mines = ls !! len
    return (Just (Minesweeper (Board (map parseToRow rows)) (len - 1) (parseToPos mines)))
  else
    return Nothing

-- | Transfer a given list of mine positions to a string
posToString :: [Pos] -> String
posToString []         = []
posToString ((y,x):is) = [intToDigit y, intToDigit x] ++ posToString is

-- | Write a minesweeper to a given filepath
writeMinesweeper :: FilePath -> Minesweeper -> IO ()
writeMinesweeper fp (Minesweeper b _ m) = writeFile fp contents
  where contents = show b ++ "\n" ++ posToString m

------------------------------------------------------------------------------------------------------------------------
-- PRINTING FUNCTIONS --------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Print a given minesweeper board with index values across the top and left side of the board
printMinesweeper :: Minesweeper -> IO ()
printMinesweeper m = do
                        putStrLn $ printTopRow range ++ " | "
                        putStrLn (concat (replicate bsize "---") ++ "-|--")
                        putStrLn $ printBoard $ zip range (rows b)
                        where
                          b = board m
                          bsize = boardSize m
                          range = [0 .. (bsize - 1)]

-- | Print the index, a seperator and then the board row, repeat for all rows
printBoard :: [(Int,Row)] -> String
printBoard [] = ""
printBoard ((x,r):rs) = show r ++ " | " ++ show x ++ "\n" ++ printBoard rs

-- | Print the top row of indexes
printTopRow :: [Int] -> String
printTopRow [] = ""
printTopRow (x:xs) = pref  ++ show x ++ " " ++ printTopRow xs
  where pref = replicate (1 - div x 10) ' ' -- Make sure the digits required to write the column index matches board
