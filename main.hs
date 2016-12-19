
import System.Environment (getArgs)
import System.Random

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

import Data.List (permutations, zipWith4)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map

-- Basic positions
newtype Position = Position (Int,Int)
                   deriving (Show, Eq, Ord)

-- Player data
data Color = Ylw | Red | Blu | Grn deriving (Show)
data Control = Human | AI deriving (Show)
newtype Card = Card Int -- The ID of the treasure to collect
               deriving (Show)
data Player = Player {
                color :: Color,
                control :: Control,
                position :: Position,
                cards :: [Card]
              } deriving (Show)

-- Board data
data Direction = N | E | S | W deriving (Show)
data Kind = L | T | I deriving (Show)
newtype Treasure = Treasure Int -- The ID of the treasure
                   deriving (Show)
data Tile = Tile {
              kind :: Kind,
              direction :: Direction,
              treasure :: Treasure
            } deriving (Show)
data XTile = XTile {
              xkind :: Kind,
              xdirection :: Direction
             } deriving (Show)
data Board = Board {
              xtile :: XTile,
              bmap :: (Map.Map Position Tile)
             } deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  putStrLn "Welcome to Haskell Labyrinth!"
  case args of
    ["-new"] -> newGame
    ["-load", filename] -> do putStrLn $ "I will load: " ++ filename
    _ -> do putStrLn "Usage: -new for a new game, -load filename to load a game"

newGame :: IO ()
newGame = do
  putStrLn "How many players will be playing today? (1-4)"
  input <- getLine
  let numPlayers = read input :: Int
  players <- mkPlayers numPlayers
  board <- mkBoard
  gameLoop players board

mkPlayers :: Int -> IO [Player]
mkPlayers numPlayers =
    shuffle cards >>= (\shuffld ->
      return $ zipWith4 (Player) colors controls positions (chunksOf 6 shuffld))
  where
    cards = [ Card i | i <- [1..24] ]
    colors = [Ylw, Red, Blu, Grn]
    controls = [ c | i <- [0..3], let c = if i < numPlayers then Human else AI ]
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
    dirs = replicate 9 N ++ replicate 8 E ++ replicate 8 S ++ replicate 8 W
    treasures = [ Treasure i | i <- [1..24] ] ++ (replicate 9 $ Treasure 0)
    positions = [ Position (r,c) | r <- [1..7], c <- [1..7],
                                 (not (even r) && not (even c)
                                 && r/=1 && r/=7 && c/=1 && c/=7)
                                 || even r
                                 || even c ]

fixedTiles :: Map.Map Position Tile
fixedTiles =
    Map.fromList $ zip positions (zipWith3 (Tile) kinds dirs treasures)
  where
    positions = [ Position (r,c) | r <- [1..7], c <- [1..7],
                                 not (even c) && not (even r)
                                 && (r==1||r==7||c==1||c==7)]
    kinds = [L, T, T, L, T, T, T, T, L, T, T, L]
    dirs =  [E, N, N, S, W, E, W, E, N, S, S, W]
    treasures = replicate 12 $ Treasure 0

-- type MonadLabyrinth a = ReaderT Board (ExceptT String (StateT Board IO)) a
-- runGame :: Board -> MonadLabyrinth a -> IO (Either String a, Board)
-- runGame board comp =
--   runStateT (runExceptT (runReaderT comp board)) board
-- gameLoop :: Board -> MonadLabyrinth Board
-- gameLoop board = do
--   return board

-- type LabyrinthT m = ExceptT String (StateT Board m)
-- type LabyrinthIO = LabyrinthT IO
-- type LabyrinthSTM = LabyrinthT STM

gameLoop :: [Player] -> Board -> IO ()
gameLoop players board = do
  putStrLn "The End"

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  randomPos <- getStdRandom (randomR (0, length xs - 1))
  let (left, (a:right)) = splitAt randomPos xs
  fmap (a:) (shuffle (left ++ right))
