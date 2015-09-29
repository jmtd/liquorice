-- HWadC - Haskell Wad Compiler
-- we want to write something like

-- main = do
--  movestep 32 32
--  thing
--  interpret "outfile.wad"

-- so we need some kind of state monad that tracks
-- pen state
--      x,y pos
--      up/down
--      orientation
-- "ink" state
--      current thing
--      current bot/mid/top
--      current floor/ceil
--      etc.

module WadC
( Orientation
, Pen
, left
, right
, step
, start
) where

data Orientation = North | South | East | West deriving Show

left North = West
left West = South
left South = East
left East = North

right x = iterate left x !! 3

data Nib = Up | Down deriving Show

data Pen = Pen { orientation :: Orientation
               , location :: (Int, Int)
               , nib :: Nib
} deriving (Show)

step :: Pen -> (Int, Int) -> Pen
step (Pen o (x1,y1) n) (x2, y2) = Pen o (x1 + x2, y1 + y2) n

rotleft  (Pen o l n) = Pen (left o) l n
rotright (Pen o l n) = Pen (right o) l n

start = Pen North (0,0) Down

main = do
    putStrLn "hello world"
