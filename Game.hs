module Game where
import Minesweeper
import System.Random
import Data.List

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
  printBoard (board m)
  putStrLn "Thanks for playing!"



------------------------------------------------------------------------------------------------------------------------
-- PRINTING FUNCTIONS --------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

printMinesweeper :: Minesweeper -> IO ()
printMinesweeper m = printBoard (board m)

printBoard :: Board -> IO ()
printBoard board = putStrLn (show board)
