module Main where

import Data.Bifunctor (bimap)

newtype Queen = Queen {queen :: Int} deriving (Show)
newtype Square = Square {square :: Maybe Queen} deriving (Show)
newtype Board = Board {board :: [[Square]]} deriving (Show)

newtype Row = Row {row :: Int} deriving (Show)
newtype Column = Column {column :: Int} deriving (Show)
newtype Coordinate = Coordinate {coordinate :: (Row, Column)} deriving (Show)

coordinateValue :: Coordinate -> (Int, Int)
coordinateValue = bimap row column . coordinate
rowValue :: Coordinate -> Int
rowValue = row . fst . coordinate
columnValue :: Coordinate -> Int
columnValue = column . snd . coordinate

initializaNBoard :: Int -> [[Square]]
initializaNBoard n = initializaNBoard' [] n
 where
  initializaNBoard' acc n
    | length acc == n = acc
    | otherwise = initializaNBoard' (inializaNRow [] n : acc) n
  inializaNRow :: [Square] -> Int -> [Square]
  inializaNRow acc n
    | length acc == n = acc
    | otherwise = inializaNRow (Square Nothing : acc) n

main :: IO ()
main = do
  -- putStrLn "Hello, Haskell!"
  let board = initializaNBoard 10
   in print $ length board
