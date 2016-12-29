
module Data
  (Position(..), Color(..), Control(..), Card(..), Player(..),
   Direction(..), Kind(..), Treasure(..), Tile(..), XTile(..), Board(..))
   where

import qualified Data.Map.Strict as Map
import Data.List (sort)

-- Basic positions
newtype Position = Ps (Int,Int)
                   deriving (Eq, Ord)

-- Player data
data Color = Yellow | Red | Blue | Green deriving (Show)
data Control = Human | AI deriving (Show)
newtype Card = Cd Int -- The ID of the treasure to collect
               deriving (Eq)
data Player = Player {
                color :: Color,
                control :: Control,
                position :: Position,
                start :: Position,
                cards :: [Card] }

-- Board data
data Direction = N | E | S | W deriving (Show, Read, Eq, Enum)
data Kind = L | T | I deriving (Show)
newtype Treasure = Tr Int -- The ID of the treasure
                   deriving (Eq)
data Tile = Tile {
              kind :: Kind,
              direction :: Direction,
              treasure :: Treasure }
data XTile = XTile {
              xkind :: Kind,
              xtreasure :: Treasure }
data Board = Board {
              xtile :: XTile,
              bmap :: Map.Map Position Tile }

instance Show Board where
  show (Board xtile bmap) =
      "XTile: "
      ++ show xtile
      ++ "\nBoard:\n"
      ++ sep ++ "\n"
      ++ (concat $ map pprint (sort keys))
    where
      sep = replicate 55 '-'
      keys = Map.keys bmap
      pprint :: Position -> String
      pprint (Ps (row, col))
        | col == 7 = (show $ bmap Map.! Ps (row,col)) ++ "\n" ++ sep ++ "\n"
        | otherwise = (show $ bmap Map.! Ps (row,col)) ++ "|"

instance Show XTile where
  show (XTile L t) = " ^ >" ++ show t
  show (XTile T t) = "< v>" ++ show t
  show (XTile I t) = " ^v " ++ show t

instance Show Tile where
  show (Tile L N t) = " ^ >" ++ show t
  show (Tile L E t) = "  v>" ++ show t
  show (Tile L S t) = "< v " ++ show t
  show (Tile L W t) = "<^  " ++ show t
  show (Tile T N t) = "< v>" ++ show t
  show (Tile T E t) = "<^v " ++ show t
  show (Tile T S t) = "<^ >" ++ show t
  show (Tile T W t) = " ^v>" ++ show t
  show (Tile I N t) = " ^v " ++ show t
  show (Tile I E t) = "<  >" ++ show t
  show (Tile I S t) = " ^v " ++ show t
  show (Tile I W t) = "<  >" ++ show t

instance Show Treasure where
  show (Tr 0) = "   "
  show (Tr x)
    | x < 10 = "T0" ++ show x
    | x >= 10 = "T" ++ show x

instance Show Card where
  show (Cd x)
    | x < 10 = "C0" ++ show x
    | x >= 10 = "C" ++ show x

instance Show Position where
  show (Ps (row,col)) =
    show (row,col)

instance Show Player where
  show (Player color control position start cards) =
    show color ++ " " ++ show control ++ " " ++ show position
    ++ " " ++ show start ++ " " ++ show cards
