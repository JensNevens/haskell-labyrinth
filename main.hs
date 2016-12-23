
import System.Environment (getArgs)
import System.Random
import System.IO

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

import Data.List (permutations, zipWith5, sort)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.Char (toLower)

-- Basic positions
newtype Position = Position (Int,Int)
                   deriving (Show, Eq, Ord)

-- Player data
data Color = Yellow | Red | Blue | Green deriving (Show)
data Control = Human | AI deriving (Show)
newtype Card = Card Int -- The ID of the treasure to collect
               deriving (Show)
data Player = Player {
                color :: Color,
                control :: Control,
                position :: Position,
                start :: Position,
                cards :: [Card]
              } deriving (Show)

-- Board data
data Direction = N | E | S | W deriving (Show, Read)
data Kind = L | T | I deriving (Show)
newtype Treasure = Treasure Int -- The ID of the treasure
                   deriving (Show)
data Tile = Tile {
              kind :: Kind,
              direction :: Direction,
              treasure :: Treasure
            }
data XTile = XTile {
              xkind :: Kind,
              xdirection :: Direction
             }
data Board = Board {
              xtile :: XTile,
              bmap :: Map.Map Position Tile
             }

main :: IO ()
main = do
  args <- getArgs
  putStrLn "Welcome to Haskell Labyrinth!"
  case args of
    ["-new"] -> newGame
    ["-load", filename] -> do putStrLn $ "I will load: " ++ filename
    _ -> do putStrLn "Usage: '-new' for a new game, '-load filename' to load a game"

newGame :: IO ()
newGame = do
  putStrLn "How many players will be playing today? (1-4)"
  input <- getLine
  let numHumans = read input :: Int
  players <- mkPlayers numHumans
  board <- mkBoard
  gameLoop players board

mkPlayers :: Int -> IO [Player]
mkPlayers numHumans =
    shuffle cards >>= (\shuffld ->
      return $ zipWith5 (Player) colors controls positions positions (chunksOf 6 shuffld))
  where
    cards = [ Card i | i <- [1..24] ]
    colors = [Yellow, Red, Blue, Green]
    controls = [ c | i <- [0..3], let c = if i < numHumans then Human else AI ]
    positions = [Position (1,1), Position (1,7), Position (7,1), Position (7,7)]

mkBoard :: IO Board
mkBoard = do
    sKinds <- shuffle kinds
    sDirs <- shuffle dirs
    sTreasures <- shuffle treasures
    let xtile = XTile (head sKinds) (head sDirs)
        tiles = zipWith3 (Tile) (tail sKinds) (tail sDirs) sTreasures
        rightmap = fixedTiles
        leftmap = Map.fromList $ zip positions tiles
    return $ Board xtile (Map.union leftmap rightmap)
  where
    kinds = replicate 16 L ++ replicate 6 T ++ replicate 12 L
    dirs = replicate 9 N ++ replicate 9 E ++ replicate 8 S ++ replicate 8 W
    treasures = [ Treasure i | i <- [1..24] ] ++ (replicate 9 $ Treasure 0)
    positions = [ Position (r,c) | r <- [1..7], c <- [1..7], even c || even r]

fixedTiles :: Map.Map Position Tile
fixedTiles =
    Map.fromList $ zip positions (zipWith3 (Tile) kinds dirs treasures)
  where
    positions = [ Position (r,c) | r <- [1..7], c <- [1..7], not (even c) && not (even r)]
    kinds = [L, T, T, L,
             T, T, T, T,
             T, T, T, T,
             L, T, T, L]
    dirs =  [E, N, N, S,
             W, W, N, E,
             W, S, E, E,
             N, S, S, W]
    treasures = replicate 16 $ Treasure 0

-- Each player has access to:
--  the board (tiles, xtile, treasures, other pawns locations)
--  the players own treasure cards
--  the number of cards left for other players (why use this?)

-- One move consists of:
--  use xtile to shift a row/column
--  (Map.mapKeys :: (k1 -> k2) -> Map k1 a -> Map k2 a)
--    -> make sure no other player falls off!
--    -> first create new xtile, than shift, than insert old xtile
--  get a new xtile as result
--  gather all treasures reachable from position
--    -> allPaths :: Position -> Player -> Board -> Path -> Visited -> CollectedTreasures
--       allPaths cur player board path visited treasures =
--        suc <- allNeighbours cur
--        guard (not visisted suc, can visit suc)
--        if (treasure at suc == treasure from player)
--          append to treasures
--        append cur to visited
--        append suc to path
--        loop
--  move to a reachable tile
gameLoop :: [Player] -> Board -> IO ()
gameLoop (me:others) board = do
  putStrLn $ "Player " ++ (show $ color me) ++ " can move!"
  putStrLn $ "In what direction do you want to insert the XTile? Choose N, E, S or W"
  xtiledirection <- getLine
  let xtiledir = read xtiledirection :: Direction
  putStrLn $ "Do you want to move a row or column? Type 'row' or 'column'"
  move <- getLine
  putStrLn $ "Give the index of the " ++ move ++ " you want to move"
  idxInput <- getLine
  let idx = read idxInput :: Int
  if move == "row"
  then do putStrLn $ "Do you want to move this row from left to right (1) or right to left (2)?"
          dirInput <- getLine
          let dir = read dirInput :: Int
          putStrLn $ xtiledirection ++ move ++ idxInput ++ dirInput
  else do putStrLn $ "Do you want to move this column from up to down (1) or down to up (2)?"
          dirInput <- getLine
          let dir = read dirInput :: Int
          putStrLn $ xtiledirection ++ move ++ idxInput ++ dirInput

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  randomPos <- getStdRandom (randomR (0, length xs - 1))
  let (left, (a:right)) = splitAt randomPos xs
  fmap (a:) (shuffle (left ++ right))

instance Show Board where
  show (Board xtile bmap) =
      show xtile ++ "\n" ++ (foldl func "Board:\n" (sort keys))
    where
      keys = Map.keys bmap
      func acc (Position (row, col))
        | col == 7 = acc ++ (show $ bmap Map.! (Position (row,col))) ++ "\n"
        | otherwise = acc ++ (show $ bmap Map.! (Position (row,col))) ++ " "

instance Show XTile where
  show (XTile xkind xdirection) =
    "XTile: " ++ show xkind ++ (map toLower $ show xdirection)

instance Show Tile where
  show (Tile kind direction treasure) =
    show kind ++ (map toLower $ show direction)
