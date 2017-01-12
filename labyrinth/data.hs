
module Data
  (Position(..), Color(..), Control(..), Card(..), Player(..),
   Direction(..), Kind(..), Treasure(..), Tile(..), XTile(..), Board(..),
   LabyrintError(..), AskMonad(..), Show(..), Serialize(..))
   where

import Control.Monad.Trans.Except

import qualified Data.Map.Strict as Map
import Data.List (sort)

-- Basic positions --
newtype Position = Ps (Int,Int)
                   deriving (Eq, Ord)

-- Player data --
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

-- Board data --
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

-- Error handling --
data LabyrintError = InvalidChoice
                   | InvalidInput
                   | InvalidMove

-- IO Monad wrapped in Except Monad where the Error
-- type is LabyrintError
type AskMonad = ExceptT LabyrintError IO

-- Serialize instances --
-- Used to serialize the game
class Serialize a where
  serialize :: a -> String

instance Serialize Player where
  serialize (Player color control position start cards) =
    show color ++ " "
    ++ show control ++ " "
    ++ show position ++ " "
    ++ show start ++ " "
    ++ show cards

instance Serialize Board where
  serialize (Board xtile bmap) =
      serialize xtile ++
      "\n[" ++ (concat $ map (serializePos bmap) (sort keys)) ++ "]"
    where
      keys = Map.keys bmap

serializePos :: Map.Map Position Tile -> Position -> String
serializePos bmap (Ps (r,c))
  | c == 7 && r == 7 = serialize $ bmap Map.! Ps (r,c)
  | otherwise = (serialize $ bmap Map.! Ps (r,c)) ++ ","

instance Serialize Tile where
  serialize (Tile L N t) = "LN" ++ serialize t
  serialize (Tile L E t) = "LE" ++ serialize t
  serialize (Tile L S t) = "LS" ++ serialize t
  serialize (Tile L W t) = "LW" ++ serialize t
  serialize (Tile T N t) = "TN" ++ serialize t
  serialize (Tile T E t) = "TE" ++ serialize t
  serialize (Tile T S t) = "TS" ++ serialize t
  serialize (Tile T W t) = "TW" ++ serialize t
  serialize (Tile I N t) = "IN" ++ serialize t
  serialize (Tile I E t) = "IE" ++ serialize t
  serialize (Tile I S t) = "IS" ++ serialize t
  serialize (Tile I W t) = "IW" ++ serialize t

instance Serialize XTile where
  serialize (XTile L t) = "L" ++ serialize t
  serialize (XTile T t) = "T" ++ serialize t
  serialize (XTile I t) = "I" ++ serialize t

instance Serialize Treasure where
  serialize (Tr x) = "T" ++ show x

-- Show instances --
-- Used to show the game to the user
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

instance Show Player where
  show (Player color control position _ cards) =
    "Player " ++ show color ++ " (" ++ show control ++ ") is at "
    ++ show position ++ " and has yet to collect "
    ++ (show $ length cards) ++ " cards"

instance Show Card where
  show (Cd x) = "C" ++ show x

instance Show Position where
  show (Ps (row,col)) =
    show (row,col)

instance Show LabyrintError where
  show InvalidChoice = "ERROR - This is not a valid choice!"
  show InvalidInput = "ERROR - This is not valid input!"
  show InvalidMove = "ERROR - This causes a player to fall off the board!"
