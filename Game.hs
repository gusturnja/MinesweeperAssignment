module Game where
import Minesweeper
import System.Random
import Data.List
import System.Exit

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

gameLoop :: Minesweeper -> IO ()
gameLoop m = do
              printMinesweeper m
              if checkWin m
              then do
                putStrLn "Congrats, you win!"
              else do
                if checkLose m
                then do
                  putStrLn "You lose!"
                else do
                  putStrLn "c -  Clear a Cell"
                  putStrLn "f -   Flag a Cell"
                  putStrLn "exit - Force Quit"
                  putStrLn "Please enter option"
                  input <- getLine
                  case input of
                    "exit" -> do
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
                      putStrLn "I'm sorry, please enter either c, f or exit"
                      gameLoop m

inputCoord :: IO Pos
inputCoord = do
              putStrLn "Please enter the coordinates (row,column)"
              r_input <- getLine
              return (read r_input :: Pos)

------------------------------------------------------------------------------------------------------------------------
-- PRINTING FUNCTIONS --------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

printMinesweeper :: Minesweeper -> IO ()
printMinesweeper m = printBoard (board m)

printBoard :: Board -> IO ()
printBoard board = putStrLn (show board)
