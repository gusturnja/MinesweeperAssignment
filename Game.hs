module Game where
import Minesweeper
import System.Random
import Data.List
import System.Exit

-- | Create a new minesweeper and being the game loop
main = do
  g <- newStdGen
  putStrLn "Welcome To MineSweeper!"
  putStrLn "Please enter desired board size:"
  r_bsize <- getLine
  let bsize = read r_bsize :: Int
  putStrLn $ "Selected " ++ show bsize ++ " as board size"
  putStrLn "Please enter the number of mines"
  r_msize <- getLine
  let msize = read r_msize :: Int
      m = mkMinesweeper bsize msize g
  gameLoop m
  putStrLn "Thanks for playing!"

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
                then
                  putStrLn "You lose!"
                else do
                  putStrLn "c -    Clear a Cell"
                  putStrLn "f -     Flag a Cell"
                  putStrLn "exit -         Quit"
                  putStrLn "Please enter option"
                  input <- getLine
                  case input of
                    "exit" ->
                      exitSuccess
                    "c" -> do
                      p <- inputCoord
                      let newm = clearSquare m p
                      gameLoop newm
                    "f" -> do
                      p <- inputCoord
                      let newm = setFlagged m p
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
-- PRINTING FUNCTIONS --------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Print a given minesweeper board with index values across the top and left side of the board
printMinesweeper :: Minesweeper -> IO ()
printMinesweeper m = do
                        putStrLn $ "  | " ++ printTopRow range
                        putStrLn ("--|-" ++ concat (replicate bsize "---"))
                        putStrLn $ printBoard $ zip range (rows b)
                        where
                          b = board m
                          bsize = boardSize m
                          range = [0 .. (bsize - 1)]

-- | Print the index, a seperator and then the board row, repeat for all rows
printBoard :: [(Int,Row)] -> String
printBoard [] = ""
printBoard ((x,r):rs) = show x ++ " | " ++ show r ++ "\n" ++ printBoard rs

-- | Print the top row of indexes
printTopRow :: [Int] -> String
printTopRow [] = ""
printTopRow (x:xs) = " " ++ show x ++ " " ++ printTopRow xs
