
import System.Environment (getArgs)
import System.Random
import System.IO
import System.Process (callCommand)
import System.FilePath.Posix ((-<.>))

import Data.List (zipWith5, sort, (\\))
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map

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

main :: IO ()
main = do
  args <- getArgs
  putStrLn "Welcome to Haskell Labyrinth!"
  case args of
    ["-new"] -> newGame
    ["-load", filename] -> load filename
    otherwise -> do putStrLn "Usage: '-new' for a new game, '-load filename' to load a game"

newGame :: IO ()
newGame = do
  putStrLn "How many players will be playing today? (1-4)"
  input <- getLine
  let numHumans = read input :: Int
  players <- mkPlayers numHumans
  board <- mkBoard
  loop players board

mkPlayers :: Int -> IO [Player]
mkPlayers numHumans =
    shuffle cards >>= (\shuffled ->
      return $ zipWith5 (Player) colors controls starts starts (chunksOf 6 shuffled))
  where
    cards = [ Cd i | i <- [1..24] ]
    colors = [Yellow, Red, Blue, Green]
    controls = [ c | i <- [0..3], let c = if i < numHumans then Human else AI ]
    starts = [Ps (1,1), Ps (1,7), Ps (7,1), Ps (7,7)]

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
    treasures = [ Tr i | i <- [1..24] ] ++ (replicate 10 $ Tr 0)
    positions = [ Ps (r,c) | r <- [1..7], c <- [1..7], even c || even r]

fixedTiles :: Map.Map Position Tile
fixedTiles =
    Map.fromList $ zip positions (zipWith3 (Tile) kinds dirs treasures)
  where
    positions = [ Ps (r,c) | r <- [1..7], c <- [1..7], not (even c) && not (even r)]
    kinds = [L, T, T, L,
             T, T, T, T,
             T, T, T, T,
             L, T, T, L]
    dirs =  [E, N, N, S,
             W, W, N, E,
             W, S, E, E,
             N, S, S, W]
    treasures = replicate 16 $ Tr 0

load :: FilePath -> IO ()
load filename = do
  let posixFilename = filename -<.> ".txt"
  putStrLn $ "I will load: " ++ posixFilename
  input <- readFile posixFilename
  -- (players, board) <- parse input
  -- loop players board
  putStrLn input

askFilePath :: IO FilePath
askFilePath = do
  putStrLn "Please enter the name of the file to save your game"
  input <- getLine
  return $ input -<.> ".txt"

save :: [Player] -> Board -> IO ()
save players (Board xtile bmap) = do
    filepath <- askFilePath
    let playerData = foldl (\acc p -> (acc ++ show p ++ "\n")) "" players
        boardData = printXTile xtile ++ "\n" ++ (foldl printBoard "" (sort keys))
    writeFile filepath $ playerData ++ boardData
    putStrLn $ "Game saved in " ++ filepath
  where
    keys = Map.keys bmap
    printBoard :: String -> Position -> String
    printBoard acc (Ps (row, col))
      | col == 7 = acc ++ (printTile $ bmap Map.! Ps (row,col)) ++ "\n"
      | otherwise = acc ++ (printTile $ bmap Map.! Ps (row,col)) ++ ","
    printTile :: Tile -> String
    printTile (Tile L N t) = "LN-" ++ show t
    printTile (Tile L E t) = "LE-" ++ show t
    printTile (Tile L S t) = "LS-" ++ show t
    printTile (Tile L W t) = "LW-" ++ show t
    printTile (Tile T N t) = "TN-" ++ show t
    printTile (Tile T E t) = "TE-" ++ show t
    printTile (Tile T S t) = "TS-" ++ show t
    printTile (Tile T W t) = "TW-" ++ show t
    printTile (Tile I N t) = "IN-" ++ show t
    printTile (Tile I E t) = "IE-" ++ show t
    printTile (Tile I S t) = "IS-" ++ show t
    printTile (Tile I W t) = "IW-" ++ show t
    printXTile :: XTile -> String
    printXTile (XTile L t) = "L-" ++ show t
    printXTile (XTile T t) = "T-" ++ show t
    printXTile (XTile I t) = "I-" ++ show t

-- Each player has access to:
--  the board (tiles, xtile, treasures, other pawns locations)
--  the players own treasure cards
--  the number of cards left for other players (why use this?)

-- One move consists of:
--  use xtile to shift a row/column
--  get a new xtile as result
--  gather all treasures reachable from position
--  move to a reachable tile
loop :: [Player] -> Board -> IO ()
loop players board = do
  putStrLn "Enter '-save' to save the game. Press any other key to continue playing"
  input <- getLine
  case input of
    "-save" -> save players board
    otherwise -> play players board

play :: [Player] -> Board -> IO ()
play players board = do
  prePrint players board
  move <- selectMove players $ xtile board
  let (players', board') = doMove move players board
      (cards, visited) = gatherCards (head players') board'
      firstPlayer = removeCards (head players') cards
  firstPlayer' <- movePlayer firstPlayer visited
  let nextPlayers = tail players' ++ [firstPlayer']
  postPrint nextPlayers visited cards
  if isWinner firstPlayer'
  then putStrLn $ "Player " ++ (show $ color firstPlayer') ++ " has won!"
  else callCommand "clear" >> loop nextPlayers board'

-- Turn of the prePrint and postPrint for AI players
-- by using pattern matching on the first player's control
prePrint :: [Player] -> Board -> IO ()
prePrint (me:others) board = do
    putStrLn $ "Player " ++ (show $ color me) ++ " can move!"
    putStrLn $ (show $ color me) ++ " is at " ++ (show $ position me)
    putStrLn $ (show $ color me) ++ " has these cards: " ++ (show $ cards me)
    putStrLn $ foldl pprint "" others
    putStrLn $ show board
  where
    pprint :: String -> Player -> String
    pprint acc (Player color control position _ cards) =
      acc ++ "Player " ++ show color ++ " (" ++ show control ++ ") is at "
      ++ show position ++ " and has yet to collect " ++ (show $ length cards) ++ " cards\n"

postPrint :: [Player] -> [Position] -> [Card] -> IO ()
postPrint (me:others) visited collectedCards = do
    putStrLn "Your move is over"
    putStrLn $ "You are now at " ++ (show $ position me)
    putStrLn $ foldl pprint "" others
    putStrLn $ "You visited these positions: " ++ show visited
    putStrLn $ "You collected these cards: " ++ show collectedCards
    putStrLn "Press any key to continue"
    input <- getLine
    return ()
  where
    pprint :: String -> Player -> String
    pprint acc (Player color control position _ _) =
      acc ++ "Player " ++ show color ++ " (" ++ show control ++ ") is now at "
      ++ show position ++ "\n"

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
  if (Ps pair) `elem` visited
  then return (Ps pair)
  else putStrLn "This is not a valid choice. Retry..." >> askPosition visited

movePlayer :: Player -> [Position] -> IO Player
-- Move the player to a new tile
movePlayer (Player color Human position start cards) visited = do
  putStrLn "Where do you want to move? You can reach the following tiles:"
  putStrLn $ show visited
  newPos <- askPosition visited
  return (Player color Human newPos start cards)

movePlayer (Player co AI _ s ca) visited =
  return (Player co AI (last visited) s ca)

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
    bingo (Tr x) (Cd y) = x == y

canVisit :: Map.Map Position Tile -> Position -> Position -> Bool
-- Check if you can go from tile 1 to tile 2
canVisit bmap (Ps (r1,c1)) (Ps (r2,c2)) =
    case (rd, cd) of
      (-1,0) -> N `elem` fromLinks && S `elem` toLinks -- to is above from
      (0,1)  -> E `elem` fromLinks && W `elem` toLinks -- to is right of from
      (1,0)  -> S `elem` fromLinks && N `elem` toLinks -- to is below from
      (0,-1) -> W `elem` fromLinks && E `elem` toLinks -- to is left of from
  where
    (rd, cd) = (r2-r1, c2-c1)
    fromLinks = links $ bmap Map.! (Ps (r1,c1))
    toLinks = links $ bmap Map.! (Ps (r2,c2))

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
    inBounds (Ps (row,col)) = row >= 1 && row <= 7 && col >= 1 && col <= 7
    allNeighbours (Ps (row,col)) = [Ps (row-1,col), Ps (row+1,col),
                                    Ps (row,col-1), Ps (row,col+1)]

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
shift (Ps (row,col)) (Ps (1,ecol))
  | col == ecol = Ps (row+1,col)
  | otherwise = Ps (row,col)
shift (Ps (row,col)) (Ps (7,ecol))
  | col == ecol = Ps (row-1,col)
  | otherwise = Ps (row,col)
shift (Ps (row,col)) (Ps (erow,1))
  | row == erow = Ps (row,col+1)
  | otherwise = Ps (row,col)
shift (Ps (row,col)) (Ps (erow,7))
  | row == erow = Ps (row,col-1)
  | otherwise = Ps (row,col)

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

askDirection :: XTile -> IO Direction
-- Ask the direction to insert the XTile
askDirection (XTile kind _) = do
  putStrLn "In what direction do you want to insert the XTile?"
  putStrLn $ "Choose " ++ show (Tile kind N (Tr 0)) ++ "(1), "
                       ++ show (Tile kind E (Tr 0)) ++ "(2), "
                       ++ show (Tile kind S (Tr 0)) ++ "(3), "
                       ++ show (Tile kind W (Tr 0)) ++ "(4)"
  input <- getLine
  let xtiledir = read input :: Int
  return $ toEnum xtiledir

askEntry :: IO Position
-- Ask the position where to insert the XTile
askEntry = do
    putStrLn "Where do you want to insert the XTile?"
    putStrLn "You can only insert the XTile in even rows and columns on the edge of the board."
    putStrLn "Give a row and column number in the format (row,column)."
    input <- getLine
    let (row,col) = read input :: (Int,Int)
    if (row,col) `elem` movable
    then return $ Ps (row,col)
    else putStrLn "You cannot insert a tile here!" >> askEntry
  where
    movable = [(r,c) | r<-[1..7], c<-[1..7],
                       (r==1 || r==7 || c==1 || c==7)
                       && (even c || even r)]

moveAllowed :: Position -> [Player] -> Bool
-- Check if a move does not cause another player
-- to fall of the board
moveAllowed entry players =
    not $ fallsOff `elem` pawnPoss
  where
    fallsOff = exit entry
    pawnPoss = map position players

selectMove :: [Player] -> XTile -> IO (Direction, Position)
-- Select a move.
-- For a human player: ask where to insert the XTile
-- For an AI player: find the best place to insert the XTile
selectMove ((Player color Human p s c):others) xtile = do
  direction <- askDirection xtile
  entry <- askEntry
  if moveAllowed entry ((Player color Human p s c):others)
  then return (direction, entry)
  else putStrLn "This move causes a player to fall off the board! Retry..."
       >> selectMove ((Player color Human p s c):others) xtile

selectMove ((Player color AI _ _ _):others) xtile = return (N,Ps (1,2))

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
exit (Ps (1,col)) = Ps (7,col)
exit (Ps (7,col)) = Ps (1,col)
exit (Ps (row,1)) = Ps (row,7)
exit (Ps (row,7)) = Ps (row,1)

instance Show Board where
  show (Board xtile bmap) =
      "XTile: " ++ show xtile
      ++ "\nBoard:\n"
      ++ sep ++ "\n"
      ++ (foldl pprint "" (sort keys))
    where
      sep = replicate 55 '-'
      keys = Map.keys bmap
      pprint :: String -> Position -> String
      pprint acc (Ps (row, col))
        | col == 7 = acc ++ (show $ bmap Map.! Ps (row,col)) ++ "\n" ++ sep ++ "\n"
        | otherwise = acc ++ (show $ bmap Map.! Ps (row,col)) ++ "|"

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
