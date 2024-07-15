module Constants where 
import Data.Word (Word64) 
import  Data.Map as Map

width :: Int
width = 7
height :: Int
height = 6
boardSize :: Int
boardSize =  width*height
minScore::Int
minScore = -(boardSize `div` 2) + 3
maxScore :: Int
maxScore = ((boardSize+1) `div` 2 )-3 -- best case for score in the following plays, you solve the game whithin 4 moves

type Score = Int
type Action = Int -- the column to be played
data Player = User | AI  deriving Eq
type Bitboard = Word64 
type ActionBoard = Word64 -- the mask of the action to take. everything is 0 excepet the position where you want to place a piece 
-- gamestate contains user board, mask board, action counter
data GameState = GameState Bitboard Bitboard Int deriving (Show)

type Key = Bitboard
type TranspositionTable = Map.Map Key Score
emptyTable:: TranspositionTable
emptyTable = Map.empty

data Piece = Red | Yellow | Empty deriving (Eq)
instance Show Piece where
    show Red = "X"
    show Yellow = "O"
    show Empty = "."

type ShowBoard = [[Piece]]

emptyBitboard :: Bitboard
emptyBitboard = 0
initState :: GameState
initState = GameState emptyBitboard emptyBitboard 0 
----------- general methods -------- 

-- find the maximum of a list of tuples by comparing the 2nd element of the tuple
maximumBySnd ::  Ord b => [(a, b)] -> (a, b)
maximumBySnd ((x, y) : xs) = Prelude.foldl compPair (x, y) xs
  where
    compPair (x1,y1) (x2,y2)
      | y1>=y2 = (x1,y1)
      | otherwise = (x2,y2)

getOpponent:: Player -> Player
getOpponent User = AI
getOpponent AI = User
