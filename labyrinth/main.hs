
module Main (main) where

import System.Environment (getArgs)
import System.Process (callCommand)
import System.FilePath.Posix ((-<.>))

import qualified Data.Map.Strict as Map
import Data.List (sort)

import Text.Parsec (parse)

import Utils
import Data
import Parser
import GameIO
import Game

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
  putStrLn "How many players will be playing today? (0-4)"
  input <- getLine
  let numHumans = read input :: Int
  players <- mkPlayers numHumans
  board <- mkBoard
  loop players board

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
  postPrint (firstPlayer':(tail players')) board' visited cards
  if isWinner firstPlayer'
  then announceWinner firstPlayer'
  else let nextPlayers = tail players' ++ [firstPlayer']
       in callCommand "clear" >> loop nextPlayers board'

load :: FilePath -> IO ()
load filename = do
  let posixFilename = filename -<.> ".txt"
  putStrLn $ "I will load: " ++ posixFilename
  input <- readFile posixFilename
  case (parse gameP "" input) of
    Left _ -> putStrLn "The given file cannot be read"
    Right (players,board) -> loop players board

save :: [Player] -> Board -> IO ()
save players (Board xtile bmap) = do
    filepath <- askFilePath
    let playerData = concat $ map ((++ "\n") . show) players
        boardData = printXTile xtile
                    ++ "\n[" ++ (concat $ map (printPos bmap) (sort keys)) ++ "]"
    writeFile filepath $ playerData ++ boardData
    putStrLn $ "Game saved in " ++ filepath
  where
    keys = Map.keys bmap