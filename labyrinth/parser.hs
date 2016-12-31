
module Parser (gameP) where

import Text.Parsec
import Text.Parsec.Char

import qualified Data.Map.Strict as Map

import Control.Monad

import Data

gameP :: Parsec String () ([Player], Board)
gameP = do
  players <- count 4 playerP
  board <- boardP
  return (players,board)

boardP :: Parsec String () Board
boardP = do
    xtile <- xtileP
    tiles <- listOf tileP "[" "," "]"
    return $ Board xtile (Map.fromList $ zip positions tiles)
  where
    positions = [Ps (row,col) | row <- [1..7], col <- [1..7] ]

playerP :: Parsec String () Player
playerP =
  liftM5 Player colorP controlP positionP positionP (listOf cardP "[" "," "]")

colorP :: Parsec String () Color
colorP =
    liftM mkColor $ symbol "Yellow" <|> symbol "Red" <|> symbol "Blue" <|> symbol "Green"
  where
    mkColor :: String -> Color
    mkColor "Yellow" = Yellow
    mkColor "Red" = Red
    mkColor "Blue" = Blue
    mkColor "Green" = Green

controlP :: Parsec String () Control
controlP =
    liftM mkControl $ symbol "Human" <|> symbol "AI"
  where
    mkControl :: String -> Control
    mkControl "Human" = Human
    mkControl "AI" = AI

positionP :: Parsec String () Position
positionP = liftM Ps $ pair integer integer

cardP :: Parsec String () Card
cardP = liftM Cd $ keyword "C" >> integer

directionP :: Parsec String () Direction
directionP =
    liftM mkDirection $ symbol "N" <|> symbol "E" <|> symbol "S" <|> symbol "W"
  where
    mkDirection "N" = N
    mkDirection "E" = E
    mkDirection "S" = S
    mkDirection "W" = W

kindP :: Parsec String () Kind
kindP =
    liftM mkKind $ symbol "L" <|> symbol "T" <|> symbol "I"
  where
    mkKind "L" = L
    mkKind "T" = T
    mkKind "I" = I

treasureP :: Parsec String () Treasure
treasureP =
  ((symbol "T" >> integer) <|> (spaces >> return 0)) >>= return . Tr

tileP :: Parsec String () Tile
tileP = liftM3 Tile kindP directionP treasureP

xtileP :: Parsec String () XTile
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
