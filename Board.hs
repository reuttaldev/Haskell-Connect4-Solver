module Board where 
import Constants
import Data.Bits
-- heuristic: first explore middle columns 
explorationOrder:: [Action]
explorationOrder = [3,2,4,1,5,0,6]

-- 1st boolean indicates whether the bit is set (1) in the user's bitboard
-- 2nd boolean indicates whether the bit is set (1) in the system's bitboard
bitToPiece :: Bool -> Bool -> Piece
bitToPiece True True = Red
bitToPiece False True = Yellow
bitToPiece _ _ = Empty

getShowBoard :: GameState -> ShowBoard
getShowBoard (GameState position mask _) = 
    [[let index = row + col * (height+1) in
        bitToPiece (testBit position index) (testBit mask index)
    | col <- [0..width-1]]| row <- reverse [0..height-1]]

rowToString :: [Piece] -> String
rowToString row = concatMap (\piece -> " "++show piece ++ " ") row ++"\n"

gameStateToString :: GameState -> String
gameStateToString gameState = 
    rowsString ++ 
    " -------------------" ++ "\n"++ 
    " 1  2  3  4  5  6  7 "
    where 
        showBoard = getShowBoard gameState
        rowsString = concatMap rowToString showBoard 

showBitBoard :: GameState -> IO ()
showBitBoard gameState = putStrLn (gameStateToString gameState)

isDraw :: Int -> Bool 
isDraw actionCounter = actionCounter == boardSize

getUserInput :: IO Action
getUserInput = do 
    input <- getLine
    let action = read input :: Int
    return (action-1)

--We use two of these bitstrings: one to encode the poisiton of the player, and another to encode the position of both the player and the computer (mask).
--The positions of only the computer can then be quickly computed using the XOR-operator.
getAIPosition:: GameState -> Bitboard
getAIPosition (GameState position mask _) = position `xor` mask
     
-- return a bitmask with a single 1 at the top cell of col
topMaskCol :: Int -> Bitboard
topMaskCol col = 1 `shiftL` ((height - 1) + col * (height + 1))

-- return a bitboard with a single 1 at to the bottom position of col
bottomMask:: Action -> Bitboard
bottomMask col = 1 `shiftL` (col * (height + 1))

-- return a bitmask with 1s on all the cells of a given column
columnMask :: Action -> Bitboard
columnMask col = ((1 `shiftL` height) - 1) `shiftL` (col * (height + 1))

--  true if the column is playable, false if the column is already full
columnFull:: Bitboard -> Int ->Bool
columnFull mask col = (mask .&. topMaskCol col) /= emptyBitboard       

-- return an action map for the column you want to put a piece in, i.e. indicates the first free row of that column
getActionMask :: Bitboard -> Action -> Maybe ActionBoard
getActionMask mask col 
    | columnFull mask col = Nothing
    | otherwise = Just ((mask + bottomMask col) .&. columnMask col)

-- check if an action is valid and return the position (need to know which row to place piece in) if valid 
isValidActionWithMessage :: Bitboard -> Action -> IO (Maybe ActionBoard)
isValidActionWithMessage mask a 
    | a >= width || a < 0 = do
        putStrLn "Index out of range."
        return Nothing
    | bitMask == Nothing = do
        putStrLn "This column is already full."
        return Nothing
    | otherwise = return bitMask 
    where bitMask = getActionMask mask a 

getValidActions:: GameState -> [ActionBoard]
getValidActions (GameState _ mask _) = [bitMask | a <- explorationOrder, let maybeBitMask = getActionMask mask a, Just bitMask <- [maybeBitMask]]

performAction :: GameState -> Player -> ActionBoard -> GameState
performAction (GameState position mask actionCounter) player a = (GameState newPosition newMask (actionCounter+1))
  where
    -- apply OR operation between the current mask and the mask with a bit in the corresponding action column
    newMask = mask .|. a
    -- the first element is the position of the user. don't update it for the computer 
    newPosition = if player == AI 
                then position 
                else position .|. a
                
-- check for connected 4 in any direction. 
connectedFour :: Bitboard -> Bool
connectedFour pos = checkHorizontal || checkDiagonal1 || checkDiagonal2 || checkVertical
  where
    checkHorizontal = let m = pos .&. (pos `shiftR` (height+1))
                      in m .&. (m `shiftR` (2*(height+1))) /= emptyBitboard
    checkDiagonal1 = let m = pos .&. (pos `shiftR` height)
                     in m .&. (m `shiftR` (2*height)) /= emptyBitboard
    checkDiagonal2 = let m = pos .&. (pos `shiftR` (height+2))
                     in m .&. (m `shiftR` (2*(height+2))) /= emptyBitboard
    checkVertical = let m = pos .&. (pos `shiftR` 1)
                    in m .&. (m `shiftR` 2) /= emptyBitboard

--detect all open ended 3-aligments
-- returns a bitboard where the position is 1 if placing a piece there makes you win immediately 
getWinningActions:: GameState -> Player  -> [ActionBoard]
getWinningActions (GameState userPos mask count) player =
    [ a| a <- allValidActions,connectedFour (a .|. position)]
    where
    allValidActions = getValidActions (GameState userPos mask count)
    position    
        | player == User = userPos
        | otherwise = getAIPosition (GameState userPos mask count)

-- group all bits in the list to a single mask 
actionListToMask::[ActionBoard] -> Bitboard
actionListToMask actions=  foldl (.|.) 0 actions

maskToActionList:: Bitboard -> [ActionBoard] 
maskToActionList mask 
    | mask == emptyBitboard = [] 
    |otherwise = [bit pos | pos <- [0..63], testBit mask pos]

-- all possible next playable positions that do not make the opponent win directly at the next move
-- use it only in positions where you cannot win in the next move
-- otherwise you will block the user's next move instead of winning yourself, which is useless 
getNonLosingActions:: GameState ->Player-> [ActionBoard]
getNonLosingActions gameState player= 
    let
        -- needs to be of the opponent, you are only getting of user here 
        oppWinningActions = getWinningActions gameState (getOpponent player)
        oppWinningMask = actionListToMask oppWinningActions
        -- we want to place a token where if the player would place it there in the next turn, they will win
        -- we also want to avoid playing below an opponent winning spot, i.e. if we put a piece there the player will have a chance to win due to that in the next turn.
        directlyBelowOppWin = oppWinningMask `shiftR` 1
        validActionMask = actionListToMask (getValidActions gameState)
    in 
        -- if opponent has no winning position
        if oppWinningActions /= [] 
            then if (oppWinningMask .&. (oppWinningMask - 1)) /= emptyBitboard
                -- this means the opponent has more than one winning position. i.e. no matter what we do, opponent will win
                then []
            else maskToActionList  (oppWinningMask .&. (complement directlyBelowOppWin))
        else maskToActionList ( validActionMask .&. (complement directlyBelowOppWin))

