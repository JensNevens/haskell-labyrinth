
module Main (main) where

import System.Environment (getArgs)
import System.IO
import System.Process (callCommand)
import System.FilePath.Posix ((-<.>))

import Data.List (zipWith5, sort, (\\))
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map

import Text.Parsec

import Utils
import Data
import Parser

main :: IO ()
main = do
  args <- getArgs
  putStrLn "Welcome to Haskell Labyrinth!"
  case args of
    ["-new"] -> newGame
    ["-load", filename] -> load filename
    otherwise -> print "Usage: '-new' for a new game, '-load filename' to load a game"

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
    cards = [Cd i | i <- [1..24]]
    colors = [Yellow, Red, Blue, Green]
    controls = [c | i <- [0..3], let c = if i < numHumans then Human else AI]
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
    treasures = [Tr i | i <- [1..24]] ++ (replicate 10 $ Tr 0)
    positions = [Ps (r,c) | r <- [1..7], c <- [1..7], even c || even r]

fixedTiles :: Map.Map Position Tile
fixedTiles =
    Map.fromList $ zip positions (zipWith3 (Tile) kinds dirs treasures)
  where
    positions = [Ps (r,c) | r <- [1..7], c <- [1..7], not (even c) && not (even r)]
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
  case (parse gameP "" input) of
    Left _ -> putStrLn "The given file cannot be read"
    Right (players,board) -> loop players board

askFilePath :: IO FilePath
askFilePath = do
  putStrLn "Please enter the name of the file to save your game"
  input <- getLine
  return $ input -<.> ".txt"

save :: [Player] -> Board -> IO ()
save players (Board xtile bmap) = do
    filepath <- askFilePath
    let playerData = foldr (\p acc -> (show p ++ "\n" ++ acc)) "" players
        boardData = printXTile xtile ++ "\n[" ++ (concat $ map printPos (sort keys)) ++ "]"
    writeFile filepath $ playerData ++ boardData
    putStrLn $ "Game saved in " ++ filepath
  where
    keys = Map.keys bmap
    printPos :: Position -> String
    printPos (Ps (r,c))
      | c == 7 && r == 7 = printTile $ bmap Map.! Ps (r,c)
      | otherwise = (printTile $ bmap Map.! Ps (r,c)) ++ ","
    printTile :: Tile -> String
    printTile (Tile L N t) = "LN" ++ show t
    printTile (Tile L E t) = "LE" ++ show t
    printTile (Tile L S t) = "LS" ++ show t
    printTile (Tile L W t) = "LW" ++ show t
    printTile (Tile T N t) = "TN" ++ show t
    printTile (Tile T E t) = "TE" ++ show t
    printTile (Tile T S t) = "TS" ++ show t
    printTile (Tile T W t) = "TW" ++ show t
    printTile (Tile I N t) = "IN" ++ show t
    printTile (Tile I E t) = "IE" ++ show t
    printTile (Tile I S t) = "IS" ++ show t
    printTile (Tile I W t) = "IW" ++ show t
    printXTile :: XTile -> String
    printXTile (XTile L t) = "L" ++ show t
    printXTile (XTile T t) = "T" ++ show t
    printXTile (XTile I t) = "I" ++ show t

-- Each player has access to:
--  the board (tiles, xtile, treasures, other pawns locations)
--  the players own treasure cards
--  the number of cards left for other players

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
  move <- selectMove players board
  let (players', board') = doMove move players board
      (cards, visited) = gatherCards (head players') board'
      firstPlayer = removeCards (head players') cards
  firstPlayer' <- movePlayer firstPlayer visited
  postPrint (firstPlayer':(tail players')) visited cards
  if isWinner firstPlayer'
  then announceWinner firstPlayer'
  else let nextPlayers = tail players' ++ [firstPlayer']
       in callCommand "clear" >> loop nextPlayers board'

-- Turn of the prePrint and postPrint for AI players
-- by using pattern matching on the first player's control
prePrint :: [Player] -> Board -> IO ()
prePrint (me:others) board = do
    putStrLn $ "Player " ++ (show $ color me) ++ " can move!"
    putStrLn $ (show $ color me) ++ " is at " ++ (show $ position me)
    putStrLn $ (show $ color me) ++ " has these cards: " ++ (show $ cards me)
    putStrLn $ concat $ map pprint others
    putStrLn $ show board
  where
    pprint :: Player -> String
    pprint (Player color control position _ cards) =
      "Player " ++ show color ++ " (" ++ show control ++ ") is at "
      ++ show position ++ " and has yet to collect "
      ++ (show $ length cards) ++ " cards\n"

postPrint :: [Player] -> [Position] -> [Card] -> IO ()
postPrint (me:others) visited collectedCards = do
    putStrLn "Your move is over"
    putStrLn $ "You are now at " ++ (show $ position me)
    putStrLn $ concat $ map pprint others
    putStrLn $ "You visited these positions: " ++ show visited
    putStrLn $ "You collected these cards: " ++ show collectedCards
    putStrLn "Press any key to continue"
    input <- getLine
    return ()
  where
    pprint :: Player -> String
    pprint (Player color control position _ _) =
      "Player " ++ show color ++ " (" ++ show control ++ ") is now at " ++ show position ++ "\n"

announceWinner :: Player -> IO ()
announceWinner (Player color _ _ _ _) =
  putStrLn $ "Player " ++ (show color) ++ " has won!"

isWinner :: Player -> Bool
isWinner (Player _ _ position start []) =
  position == start
isWinner (Player _ _ _ _ cards) =
  False

askPosition :: [Position] -> IO Position
-- Ask the player where to move to
askPosition visited = do
  input <- getLine
  let pair = read input :: (Int,Int)
  if Ps pair `elem` visited
  then return $ Ps pair
  else putStrLn "This is not a valid choice. Retry..."
       >> askPosition visited

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
    allPaths player (bmap board) [(position player)] [] []

allPaths :: Player -> Map.Map Position Tile -> [Position] -> [Position] -> [Card] -> ([Card],[Position])
-- Search through all reachable tiles and gather the treasure cards
allPaths player bmap frontier visited cards
    | null sucFrontier = (cards, visited++frontier)
    | otherwise = allPaths player bmap sucFrontier (visited++frontier) collected
  where
    sucFrontier = [ suc | cur <- frontier,
                          suc <- neighbours cur bmap,
                          not $ suc `elem` visited ]
    collected = foldr (collectCards bmap player) cards sucFrontier

collectCards :: Map.Map Position Tile -> Player -> Position -> [Card] -> [Card]
-- Check the treasure on the current tile and check
-- if you have the card associated with it
collectCards bmap player suc cardAcc =
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
  return $ toEnum $ xtiledir - 1

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
    movable = [(r,c) | r <- [1..7], c <- [1..7],
                       (r == 1 || r == 7 || c == 1 || c == 7)
                       && (even c || even r)]

moveAllowed :: Position -> [Player] -> Bool
-- Check if a move does not cause another player
-- to fall of the board
moveAllowed entry players =
    not $ fallsOff `elem` pawnPoss
  where
    fallsOff = exit entry
    pawnPoss = map position players

selectMove :: [Player] -> Board -> IO (Direction, Position)
-- Select a move.
-- For a human player: ask where to insert the XTile
-- For an AI player: find the best place to insert the XTile
selectMove ((Player color Human p s c):others) board = do
  direction <- askDirection $ xtile board
  entry <- askEntry
  if moveAllowed entry ((Player color Human p s c):others)
  then return (direction, entry)
  else putStrLn "This move causes a player to fall off the board! Retry..."
       >> selectMove ((Player color Human p s c):others) board

-- TODO: Finish AI implementation
-- selectMove ((Player color AI _ _ _):others) board =
--   return $ selectBest ((Player color AI p s c):others) board
--
-- selectBest :: [Player] -> [Board] -> (Direction, Position)
-- selectBest players board =
--     fst $ maximumBy (comparing $ (length . snd)) validMoves
--   where
--     validMoves = tryMoves players board
--
-- tryMoves :: [Player] -> Board -> [((Direction, Position), [Card])]
-- tryMoves players board = do
--     entry <- entries
--     direction <- directions
--     guard (moveAllowed entry players)
--     let cards = runReader (simMove (direction,entry)) (players,board)
--     return ((direction,entry), cards)
--   where
--     directions = [N, E, S, W]
--     entries = [(r,c) | r <- [1..7], c <- [1..7],
--                           (r == 1 || r == 7 || c == 1 || c == 7)
--                           && (even c || even r)]
--
-- simMove :: (Direction, Position) -> [Card]
-- simMove (direction, entry) = do
--   (player, board) <- ask

exit :: Position -> Position
-- Give the exit, given the entry
exit (Ps (1,col)) = Ps (7,col)
exit (Ps (7,col)) = Ps (1,col)
exit (Ps (row,1)) = Ps (row,7)
exit (Ps (row,7)) = Ps (row,1)
