module Minesweeper where

main :: IO ()
main = do
        putStrLn "I do nothing yet!"

{-
Definitions for developers benefit

A minesweeper board size is set by the user, n. A board is therefore defined as being n tiles wide and n tiles high
A tile can take multiple forms.
  - A user can "flag" a tile, this means they believe that there is a mine at this tiles location.
  - A user can select to clear a tile, if the tile is revealed to be a mine, the user loses the game.
    However, if it is not a mine, the number of mines in the surrounding tiles are shown at the selected tiles location.
    If there are not bombs in the local area, the tile must be clear.
    Surrounding tiles must then also be checked if they are clear of bombs and their surrounding tiles must be checks so on and so forth.
Mines are placed at random locations around the board and are not revealed until the end of the game.

-}

type Flag = Char -- Placeholder here for now.

type Board = [[Char]]

data Minesweeper = Minesweeper { board :: Board, boardSize :: Int }


{-
TODO

Create Minesweeper data type
create show instance for minesweeper board
create cell data type
create test cases isMinesweeper etc


-}
