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
  putStrLn "Thanks for playing!"

------------------------------------------------------------------------------------------------------------------------
-- PRINTING FUNCTIONS --------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

printBoard :: Board -> IO ()
printBoard board = putStrLn (boardToString board)

boardToString :: Board -> String
boardToString [] = ""
boardToString (x:xs) = unwords x ++ "\n" ++ boardToString xs