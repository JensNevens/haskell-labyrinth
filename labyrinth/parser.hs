
module Parser (gameP) where

import Text.Parsec
import Text.Parsec.Char

import qualified Data.Map.Strict as Map

import Control.Monad

import Data

gameP :: Parsec String () ([Player], Board)
-- To parse the game, parse 4 players and the board
gameP = do
  players <- count 4 playerP
  board <- boardP
  return (players,board)

boardP :: Parsec String () Board
-- To parse the board, parse the xtile and all tiles
boardP = do
    xtile <- xtileP
    tiles <- listOf tileP "[" "," "]"
    return $ Board xtile (Map.fromList $ zip positions tiles)
  where
    positions = [Ps (row,col) | row <- [1..7], col <- [1..7] ]

playerP :: Parsec String () Player
-- To parse a player, parse its color, control, position, start and cards
playerP =
  liftM5 Player colorP controlP positionP positionP (listOf cardP "[" "," "]")

colorP :: Parsec String () Color
-- Parse a color
colorP =
    liftM read $ symbol "Yellow" <|> symbol "Red" <|> symbol "Blue" <|> symbol "Green"
  -- where
  --   mkColor :: String -> Color
  --   mkColor "Yellow" = Yellow
  --   mkColor "Red" = Red
  --   mkColor "Blue" = Blue
  --   mkColor "Green" = Green

controlP :: Parsec String () Control
-- Parse the control
controlP =
    liftM read $ symbol "Human" <|> symbol "AI"
  -- where
  --   mkControl :: String -> Control
  --   mkControl "Human" = Human
  --   mkControl "AI" = AI

positionP :: Parsec String () Position
-- Parse a position (a pair)
positionP = liftM Ps $ pair integer integer

cardP :: Parsec String () Card
-- Parse a card
cardP = liftM Cd $ keyword "C" >> integer

directionP :: Parsec String () Direction
-- Parse a drection
directionP =
    liftM read $ symbol "N" <|> symbol "E" <|> symbol "S" <|> symbol "W"
  -- where
  --   mkDirection :: String -> Direction
  --   mkDirection "N" = N
  --   mkDirection "E" = E
  --   mkDirection "S" = S
  --   mkDirection "W" = W

kindP :: Parsec String () Kind
-- Parse a kind
kindP =
    liftM read $ symbol "L" <|> symbol "T" <|> symbol "I"
  -- where
  --   mkKind :: String -> Kind
  --   mkKind "L" = L
  --   mkKind "T" = T
  --   mkKind "I" = I

treasureP :: Parsec String () Treasure
-- Parse a treasure
treasureP = liftM Tr $ keyword "T" >> integer

tileP :: Parsec String () Tile
-- To parse a tile, parse the kind, direction and treasure
tileP = liftM3 Tile kindP directionP treasureP

xtileP :: Parsec String () XTile
-- To parse a xtile, parse the kind and treasure
xtileP = liftM2 XTile kindP treasureP

-- Parsing Utils --
pair :: Parsec String () a -> Parsec String () b -> Parsec String () (a,b)
-- Parse a pair with parsers p and q
pair p q = do
  keyword "("
  val1 <- p
  keyword ","
  val2 <- q
  keyword ")"
  return (val1,val2)

integer :: Parsec String () Int
-- Parse an integer (positive or negative)
integer = do
  spaces
  sign <- (string "-" >> return "-") <|> return ""
  spaces
  digits <- many1 digit
  return $ read (sign ++ digits)

keyword :: String -> Parsec String () ()
-- Parse a string and return nothing
-- If failed, put back all input
keyword s = try (spaces >> string s >> return ())

symbol :: String -> Parsec String () String
-- Parse a string and return it
-- If failed, but back all input
symbol s = try (spaces >> string s)

listOf :: Parsec String () a -> String -> String -> String -> Parsec String () [a]
-- Parse a list of things, enclosed by the symbols 'open' and 'close'
-- and split by the symbol 'sep'
listOf p open sep close =
  between (keyword open) (keyword close) (p `sepBy` keyword sep)
