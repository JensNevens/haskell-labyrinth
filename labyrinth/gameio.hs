
module GameIO
       (mkPlayers, mkBoard,
       askPlayerCount, askFilePath, askDirection, askEntry, askPosition,
       prePrint, postPrint)
       where

import System.FilePath.Posix ((-<.>))
import System.Process (callCommand)

import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import Data.List (zipWith5)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map

import Utils
import Data

mkPlayers :: Int -> IO [Player]
-- Initialize the players
mkPlayers numHumans =
    shuffle cards >>= (\shuffled ->
      return $ zipWith5 (Player) colors controls starts starts (chunksOf 6 shuffled))
  where
    cards = [Cd i | i <- [1..24]]
    colors = [Yellow, Red, Blue, Green]
    controls = [c | i <- [0..3], let c = if i < numHumans then Human else AI]
    starts = [Ps (1,1), Ps (1,7), Ps (7,1), Ps (7,7)]

mkBoard :: IO Board
-- Initialize the board
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
-- Generate the fixed tiles of the board
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

askPlayerCount :: AskMonad Int
-- Ask how many players will be playing
askPlayerCount = do
  liftIO $ putStrLn "How many players will be playing today? (0-4)"
  -- input <- liftIO getLine
  -- let numHumans = read input :: Int
  numHumans <- fmap read $ liftIO getLine
  if numHumans `elem` [0..4]
  then return numHumans
  else throwE InvalidChoice

askFilePath :: IO FilePath
-- Ask the file path to save the game
askFilePath = do
  putStrLn "Please enter the name of the file to save your game"
  path <- fmap (-<.> ".txt") getLine
  return $ path

askDirection :: XTile -> AskMonad Direction
-- Ask the direction to insert the XTile
askDirection (XTile kind t) = do
  liftIO $ putStrLn "In what direction do you want to insert the XTile?"
  liftIO $ putStrLn $ "Choose " ++ show (Tile kind N (Tr 0)) ++ "(1), "
                                ++ show (Tile kind E (Tr 0)) ++ "(2), "
                                ++ show (Tile kind S (Tr 0)) ++ "(3), "
                                ++ show (Tile kind W (Tr 0)) ++ "(4)"
  -- input <- liftIO getLine
  -- let xtiledir = read input :: Int
  xtiledir <- fmap read $ liftIO getLine
  if xtiledir `elem` [1..4]
  then return $ toEnum $ xtiledir - 1
  else throwE InvalidChoice

askEntry :: AskMonad Position
-- Ask the position where to insert the XTile
askEntry = do
    liftIO $ putStrLn "Where do you want to insert the XTile?"
    liftIO $ putStrLn "You can only insert the XTile in even rows and columns on the edge of the board."
    liftIO $ putStrLn "Give a row and column number in the format (row,column)."
    -- input <- liftIO getLine
    -- let (row,col) = read input :: (Int,Int)
    (row,col) <- fmap read $ liftIO getLine
    if (row,col) `elem` movable
    then return $ Ps (row,col)
    else throwE InvalidInput
  where
    movable = [(r,c) | r <- [1..7], c <- [1..7],
                       (r == 1 || r == 7 || c == 1 || c == 7)
                       && (even c || even r)]

askPosition :: [Position] -> AskMonad Position
-- Ask the player where to move to
askPosition visited = do
  -- input <- liftIO getLine
  -- let pair = read input :: (Int,Int)
  pair <- fmap read $ liftIO getLine
  if Ps pair `elem` visited
  then return $ Ps pair
  else throwE InvalidChoice

-- TODO:
-- Turn of the prePrint and postPrint for AI players
-- by using pattern matching on the first player's control

-- Pre- and post-move printing of information --
prePrint :: [Player] -> Board -> IO ()
-- Show information prior to the move
prePrint ((Player color Human position _ cards):others) board = do
  putStrLn $ "Player " ++ (show color) ++ " can move!"
  putStrLn $ "You are at " ++ (show position)
  putStrLn $ "You have these cards: " ++ (show cards)
  putStrLn $ concat $ map ((++ "\n") . show) others
  putStrLn $ show board

prePrint ((Player _ AI _ _ _):others) board = return ()

postPrint :: [Player] -> Board -> [Card] -> IO ()
-- SHow information after the move
postPrint ((Player color Human position _ cards):others) board collectedCards = do
  callCommand "clear"
  putStrLn "Your move is over"
  putStrLn $ "You are now at " ++ (show position)
  putStrLn $ "You collected these cards: " ++ show collectedCards
  putStrLn $ "You have these cards left: " ++ (show cards)
  putStrLn $ concat $ map ((++ "\n") . show) others
  putStrLn $ show board
  putStrLn "Press any key to continue"
  input <- getLine
  return ()

postPrint ((Player _ AI _ _ _):others) board collectedCards = return ()
