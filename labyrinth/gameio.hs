
module GameIO (mkPlayers, mkBoard, askFilePath, printPos, printTile, printXTile) where

import System.FilePath.Posix ((-<.>))

import Data.List (zipWith5)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map

import Utils
import Data

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

askFilePath :: IO FilePath
askFilePath = do
  putStrLn "Please enter the name of the file to save your game"
  input <- getLine
  return $ input -<.> ".txt"

printPos :: Map.Map Position Tile -> Position -> String
printPos bmap (Ps (r,c))
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
