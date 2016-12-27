
import System.Environment (getArgs)
import System.Random
import System.IO
import System.Process (callCommand)

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

import Data.List (permutations, zipWith5, sort, nub, (\\))
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.Char (toLower)

-- Basic positions
newtype Position = P (Int,Int)
                   deriving (Eq, Ord)

-- Player data
data Color = Yellow | Red | Blue | Green deriving (Show)
data Control = Human | AI deriving (Show)
newtype Card = Card Int -- The ID of the treasure to collect
               deriving (Show, Eq)
data Player = Player {
                color :: Color,
                control :: Control,
                position :: Position,
                start :: Position,
                cards :: [Card]
              } deriving (Show)

-- Board data
data Direction = N | E | S | W deriving (Show, Read, Eq)
data Kind = L | T | I deriving (Show)
newtype Treasure = Treasure Int -- The ID of the treasure
                   deriving (Show, Eq)
data Tile = Tile {
              kind :: Kind,
              direction :: Direction,
              treasure :: Treasure
            }
data XTile = XTile {
              xkind :: Kind,
              xtreasure :: Treasure
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
    positions = [P (1,1), P (1,7), P (7,1), P (7,7)]

mkBoard :: IO Board
mkBoard = do
    sKinds <- shuffle kinds
    sDirs <- shuffle dirs
    sTreasures <- shuffle treasures
    let xtile = XTile (head sKinds) (head sTreasures)
        tiles = zipWith3 (Tile) (tail sKinds) sDirs (tail sTreasures)
        rightmap = fixedTiles
        leftmap = Map.fromList $ zip positions tiles
    return $ Board xtile (Map.union leftmap rightmap)
  where
    kinds = replicate 16 L ++ replicate 6 T ++ replicate 12 I
    dirs = replicate 9 N ++ replicate 8 E ++ replicate 8 S ++ replicate 8 W
    treasures = [ Treasure i | i <- [1..24] ] ++ (replicate 10 $ Treasure 0)
    positions = [ P (r,c) | r <- [1..7], c <- [1..7], even c || even r]

fixedTiles :: Map.Map Position Tile
fixedTiles =
    Map.fromList $ zip positions (zipWith3 (Tile) kinds dirs treasures)
  where
    positions = [ P (r,c) | r <- [1..7], c <- [1..7], not (even c) && not (even r)]
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
--  get a new xtile as result
--  gather all treasures reachable from position
--  move to a reachable tile
gameLoop :: [Player] -> Board -> IO ()
gameLoop players board = do
  putStrLn $ show board
  move <- selectMove players
  let (players', board') = doMove move players board
      (cards, visited) = gatherCards (head players') board'
      firstPlayer = removeCards (head players') cards
  putStrLn $ show board'
  putStrLn $ show visited
  putStrLn $ show cards
  firstPlayer' <- movePlayer firstPlayer visited
  if isWinner firstPlayer'
  then putStrLn "The End"
  else callCommand "clear" >> gameLoop ((tail players')++[firstPlayer']) board'

-- showInfo :: [Player] -> IO ()
-- showInfo (me:others) = do
--   putStrLn $ "Player " ++ (show color) ++ " can move!"
--   putStrLn $ "You are at " ++ (show p)

isWinner :: Player -> Bool
isWinner (Player col con position start []) =
  position == start
isWinner (Player col con position start cards) =
  False

askPosition :: [Position] -> IO Position
-- Ask the player where to move to
askPosition visited = do
  input <- getLine
  let pair = read input :: (Int,Int)
  if (P pair) `elem` visited
  then return (P pair)
  else putStrLn "This is not a choice. Retry..." >> askPosition visited

movePlayer :: Player -> [Position] -> IO Player
-- Move the player to a new tile
movePlayer (Player color control position start cards) visited = do
  putStrLn "Where do you want to move? You can reach the following tiles:"
  putStrLn $ show visited
  newPos <- askPosition visited
  return (Player color control newPos start cards)

removeCards :: Player -> [Card] -> Player
-- Remove the cards this player has collected
removeCards (Player col con pos start cards) collectedCards =
  (Player col con pos start (cards \\ collectedCards))

gatherCards :: Player -> Board -> ([Card],[Position])
gatherCards player board =
    allPaths player (bmap board) [[(position player)]] [] []

allPaths :: Player -> Map.Map Position Tile -> [[Position]] -> [Position] -> [Card] -> ([Card],[Position])
-- Search through all reachable tiles and gather the treasure cards
allPaths player bmap frontier visited cards
    | null sucFrontier = (cards, visited++curFrontier)
    | otherwise = allPaths player bmap sucFrontier (visited++curFrontier) collected
  where
    curFrontier = map head frontier
    sucFrontier = [ (suc:cur) | cur <- frontier,
                                suc <- neighbours (head cur) bmap,
                                not $ suc `elem` visited ]
    collected = foldl (collectCards bmap player) cards sucFrontier

collectCards :: Map.Map Position Tile -> Player -> [Card] -> [Position] -> [Card]
-- Check the treasure on the current tile and check
-- if you have the card associated with it
collectCards bmap player cardAcc (suc:rest) =
    cardAcc ++ card
  where
    tile = bmap Map.! suc
    card = filter (bingo $ treasure tile) (cards player)
    bingo :: Treasure -> Card -> Bool
    bingo (Treasure x) (Card y) = x == y

canVisit :: Map.Map Position Tile -> Position -> Position -> Bool
-- Check if you can go from tile 1 to tile 2
canVisit bmap (P (r1,c1)) (P (r2,c2)) =
    case (rd, cd) of
      (-1,0) -> N `elem` fromLinks && S `elem` toLinks -- to is above from
      (0,1)  -> E `elem` fromLinks && W `elem` toLinks -- to is right of from
      (1,0)  -> S `elem` fromLinks && N `elem` toLinks -- to is below from
      (0,-1) -> W `elem` fromLinks && E `elem` toLinks -- to is left of from
  where
    (rd, cd) = (r2-r1, c2-c1)
    fromLinks = links $ bmap Map.! (P (r1,c1))
    toLinks = links $ bmap Map.! (P (r2,c2))

links :: Tile -> [Direction]
-- In which direction can you enter/exit a tile
links (Tile L N _) = [N,E]
links (Tile L E _) = [E,S]
links (Tile L S _) = [S,W]
links (Tile L W _) = [W,N]
links (Tile T N _) = [E,S,W]
links (Tile T E _) = [N,S,W]
links (Tile T S _) = [N,E,W]
links (Tile T W _) = [N,E,S]
links (Tile I N _) = [N,S]
links (Tile I E _) = [E,W]
links (Tile I S _) = [N,S]
links (Tile I W _) = [E,W]

neighbours :: Position -> Map.Map Position Tile -> [Position]
-- Determine the neighbours of a position that you are able
-- to visit. This depends on the kind and direction of both tiles
neighbours pos bmap =
    filter (canVisit bmap pos) $ filter inBounds $ allNeighbours pos
  where
    inBounds (P (row,col)) = row >= 1 && row <= 7 && col >= 1 && col <= 7
    allNeighbours (P (row,col)) = [P (row-1,col), P (row+1,col),
                                   P (row,col-1), P (row,col+1)]

movePlayers :: [Player] -> Position -> [Player]
-- If a player is on the row/column that is going to shift
-- adjust it's position. Else, do nothing
movePlayers players entry =
    map go players
  where
    go :: Player -> Player
    go (Player color control position start cards) =
      (Player color control (shift position entry) start cards)

shift :: Position  -> Position -> Position
-- Shift a position depending on the entry position
shift (P (row,col)) (P (1,ecol))
  | col == ecol = P (row+1,col)
  | otherwise = P (row,col)
shift (P (row,col)) (P (7,ecol))
  | col == ecol = P (row-1,col)
  | otherwise = P (row,col)
shift (P (row,col)) (P (erow,1))
  | row == erow = P (row,col+1)
  | otherwise = P (row,col)
shift (P (row,col)) (P (erow,7))
  | row == erow = P (row,col-1)
  | otherwise = P (row,col)

moveBoard :: Board -> Direction -> Position -> Board
-- Get the new XTile from one end
-- Shift the tiles in the board
-- Insert the old XTile on the other end
moveBoard (Board xtile bmap) dir entry =
    Board newXTile bmap'
  where
    exitPos = exit entry
    exitTile = bmap Map.! exitPos
    newXTile = XTile (kind exitTile) (treasure exitTile)
    newTile = Tile (xkind xtile) dir (xtreasure xtile)
    go :: Position -> Position
    go key = shift key entry
    bmap' = bmap -: Map.delete exitPos
                 -: Map.mapKeys go
                 -: Map.insert entry newTile

doMove :: (Direction, Position) -> [Player] -> Board -> ([Player],Board)
-- First, move the players' positions if they are on the shifted row/col
-- then, adjust the board bmap and get the new xtile
doMove (direction, entry) players board =
  let players' = movePlayers players entry
      board' = moveBoard board direction entry
  in (players', board')

askDirection :: IO Direction
-- Ask the direction to insert the XTile
askDirection = do
  putStrLn "In what direction do you want to insert the XTile? Choose N, E, S or W"
  input <- getLine
  let xtiledir = read input :: Direction
  return xtiledir

askEntry :: IO Position
-- Ask the position where to insert the XTile
askEntry = do
    putStrLn "Where do you want to insert the XTile?"
    putStrLn "Give a row and column number in the format (row,column)."
    input <- getLine
    let (row,col) = read input :: (Int,Int)
    if (row,col) `elem` movable
    then return $ P (row,col)
    else putStrLn "You cannot insert a tile here!" >> askEntry
  where
    movable = [(r,c) | r<-[1..7], c<-[1..7], (r==1 || r==7 || c==1 || c==7)
                                             && (even c || even r)]

moveAllowed :: Position -> [Player] -> Bool
-- Check if a move does not cause another player
-- to fall of the board
moveAllowed entry players =
    not $ fallsOff `elem` pawnPoss
  where
    fallsOff = exit entry
    pawnPoss = map position players

selectMove :: [Player] -> IO (Direction, Position)
-- Select a move.
-- For a human player: ask where to insert the XTile
-- For an AI player: find the best place to insert the XTile
selectMove ((Player color Human p s c):others) = do
  direction <- askDirection
  entry <- askEntry
  if moveAllowed entry ((Player color Human p s c):others)
  then return (direction, entry)
  else putStrLn "This move causes a player to fall off the board! Retry..."
       >> selectMove ((Player color Human p s c):others)

selectMove ((Player color AI _ _ _):others) = return (N,P (1,2))

shuffle :: [a] -> IO [a]
-- shuffle a list
shuffle [] = return []
shuffle xs = do
  randomPos <- getStdRandom (randomR (0, length xs - 1))
  let (left, (a:right)) = splitAt randomPos xs
  fmap (a:) (shuffle (left ++ right))

(-:) :: a -> (a -> b) -> b
x -: f = f x

exit :: Position -> Position
-- Give the exit, given the entry
exit (P (1,col)) = P (7,col)
exit (P (7,col)) = P (1,col)
exit (P (row,1)) = P (row,7)
exit (P (row,7)) = P (row,1)

instance Show Board where
  show (Board xtile bmap) =
      show xtile
      ++ "\nBoard:\n"
      ++ replicate 34 '-' ++ "\n"
      ++ (foldl func "" (sort keys))
    where
      keys = Map.keys bmap
      func :: String -> Position -> String
      func acc (P (row, col))
        | col == 7 = acc
                     ++ (show $ bmap Map.! P (row,col))
                     ++ "\n" ++ replicate 34 '-' ++ "\n"
        | otherwise = acc
                      ++ (show $ bmap Map.! P (row,col))
                      ++ "|"

instance Show XTile where
  show (XTile xkind _) =
    "XTile: " ++ show xkind

instance Show Tile where
  -- show (Tile kind direction _) =
  --   show kind ++ (map toLower $ show direction)
  show (Tile L N _) = " ^ >"
  show (Tile L E _) = "  v>"
  show (Tile L S _) = "< v "
  show (Tile L W _) = "<^  "
  show (Tile T N _) = "< v>"
  show (Tile T E _) = "<^v "
  show (Tile T S _) = "<^ >"
  show (Tile T W _) = " v^>"
  show (Tile I N _) = " ^v "
  show (Tile I E _) = "<  >"
  show (Tile I S _) = " ^v "
  show (Tile I W _) = "<  >"

instance Show Position where
  show (P (row,col)) =
    show (row,col)
