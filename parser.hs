
import Text.Parsec
import Text.Parsec.Char
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import Control.Monad

-- Basic positions
newtype Position = Ps (Int,Int)
                   deriving (Eq, Ord, Show)

-- Player data
data Color = Yellow | Red | Blue | Green deriving (Show)
data Control = Human | AI deriving (Show)
newtype Card = Cd Int -- The ID of the treasure to collect
               deriving (Eq, Show)
data Player = Player {
                color :: Color,
                control :: Control,
                position :: Position,
                start :: Position,
                cards :: [Card] } deriving (Show)

-- Board data
data Direction = N | E | S | W deriving (Show, Read, Eq, Enum)
data Kind = L | T | I deriving (Show)
newtype Treasure = Tr Int -- The ID of the treasure
                   deriving (Eq,Show)
data Tile = Tile {
              kind :: Kind,
              direction :: Direction,
              treasure :: Treasure } deriving (Show)
data XTile = XTile {
              xkind :: Kind,
              xtreasure :: Treasure } deriving (Show)
data Board = Board {
              xtile :: XTile,
              bmap :: Map.Map Position Tile } deriving (Show)

gameP :: Parsec String () ([Player], Board)
gameP = do
  players <- count 4 playerP
  board <- boardP
  return (players,board)

-- TODO: Make sure board is executed on 1 line with [...,...,...]
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
    liftM mkColor $ spaces >> (string "Yellow" <|> string "Red" <|> string "Blue" <|> string "Green")
  where
    mkColor :: String -> Color
    mkColor "Yellow" = Yellow
    mkColor "Red" = Red
    mkColor "Blue" = Blue
    mkColor "Green" = Green

controlP :: Parsec String () Control
controlP = liftM mkControl $ spaces >> (string "Human" <|> string "AI")
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
kindP = liftM mkKind $ symbol "L" <|> symbol "T" <|> symbol "I"
  where
    mkKind "L" = L
    mkKind "T" = T
    mkKind "I" = I

treasureP :: Parsec String () Treasure
treasureP = do
  idx <- (string "T" >> integer) <|> (spaces >> return 0)
  return $ Tr idx

tileP :: Parsec String () Tile
tileP = liftM3 Tile kindP directionP (keyword "-" >> treasureP)

xtileP :: Parsec String () XTile
xtileP = liftM2 XTile kindP (keyword "-" >> treasureP)

-- Utils --
pair :: Parsec String () a -> Parsec String () b -> Parsec String () (a,b)
pair p q = do
  keyword "("
  val1 <- p
  keyword ","
  val2 <- q
  keyword ")"
  return (val1,val2)

integer :: Parsec String () Int
integer = do
  spaces
  sign <- (string "-" >> return "-") <|> return ""
  spaces
  digits <- many1 digit
  return $ read (sign ++ digits)

keyword :: String -> Parsec String () ()
keyword s = spaces >> string s >> return ()

symbol :: String -> Parsec String () String
symbol s = spaces >> string s

listOf :: Parsec String () a -> String -> String -> String -> Parsec String () [a]
listOf p open sep close =
  between (keyword open) (keyword close) (p `sepBy` keyword sep)
