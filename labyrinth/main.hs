
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
  numHumans <- retryForever askPlayerCount
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

-- Each player has access to:
--  the board (tiles, xtile, treasures, other pawns locations)
--  the players own treasure cards
--  the number of cards left for other players

-- One move consists of:
--  use xtile to shift a row/column
--  get a new xtile as result
--  gather all treasures reachable from position
--  move to a reachable tile
play :: [Player] -> Board -> IO ()
play players board = do
  prePrint players board
  move <- retryForever $ selectMove players board
  let (players', board') = doMove move players board
      (cards, visited) = gatherCards (head players') board'
      firstPlayer = removeCards (head players') cards
  firstPlayer' <- movePlayer firstPlayer visited
  postPrint (firstPlayer':(tail players')) board' cards
  if isWinner firstPlayer'
  then announceWinner firstPlayer'
  else let nextPlayers = tail players' ++ [firstPlayer']
       in callCommand "clear" >>
          case control firstPlayer' of
            Human -> loop nextPlayers board'
            AI -> play nextPlayers board'
  -- else let nextPlayers = tail players' ++ [firstPlayer']
  --      in callCommand "clear" >> loop nextPlayers board'

load :: FilePath -> IO ()
load filename = do
  let posixFilename = filename -<.> ".txt"
  putStrLn $ "I will load: " ++ posixFilename
  input <- readFile posixFilename
  case (parse gameP "" input) of
    Left _ -> putStrLn "The given file cannot be read"
    Right (players,board) -> loop players board

save :: [Player] -> Board -> IO ()
save players board = do
  filepath <- askFilePath
  let playerData = concat $ map ((++ "\n") . serialize) players
      boardData = serialize board
  writeFile filepath $ playerData ++ boardData
  putStrLn $ "Game saved in " ++ filepath
