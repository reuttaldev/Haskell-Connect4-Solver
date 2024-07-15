module MinMaxSearch where 
import Constants
import Board
import Data.Map as Map
import Data.Maybe

getKey:: GameState ->Key
getKey (GameState position mask actionCounter) = position + mask

-- put a value into the transposition table
putInTable :: GameState -> Score -> TranspositionTable -> TranspositionTable
putInTable gameState value table = Map.insert (getKey gameState) value table

-- get a value from the transposition table
getFromTable :: GameState -> TranspositionTable -> Maybe Score
getFromTable gameState table = Map.lookup (getKey gameState) table

{- to display states with trace later
constructDisplayStates :: [(ActionBoard, Score)] -> [(ActionBoard, Score)]
constructDisplayStates list = Prelude.map constructState list
  where
    constructState (actionBoard, score) = 
      let actionState = GameState emptyBitboard actionBoard 0
      in trace ("Score: " ++ show score ++ "\n" ++ "ActionBoard: \n" ++ gameStateToString actionState) (actionBoard, score)
-}

alphaBetaSearch :: GameState -> TranspositionTable -> (ActionBoard, TranspositionTable)
alphaBetaSearch gameState table = 

    let  
        (GameState position mask actionCounter) =gameState
        -- window bound for alpha and beta (min and max possible scores)
        max =(boardSize -actionCounter) `div` 2  -- popCount counts the number of 1's in the bitboard, i.e number of moves already made by that player 
        min = negate max
        -- calculate the score of each action and store them in a tuple
        nonLosingActions = getNonLosingActions gameState AI
        actions 
            | nonLosingActions == [] = getValidActions gameState
            | otherwise = nonLosingActions
        (actionsWithScores,table') = nullWindowSearchForAllOptions actions gameState min max table 
        bestActionScorePair = maximumBySnd actionsWithScores
        -- extract the best action 
        in
            (fst bestActionScorePair, table')

nullWindowSearchForAllOptions::[ActionBoard] ->GameState ->Int ->Int -> TranspositionTable-> ([(ActionBoard,Score)],TranspositionTable)
nullWindowSearchForAllOptions [] _ _ _ table = ([],table)
nullWindowSearchForAllOptions (a:al) gameState min max table = 
    let 
        newState = performAction gameState AI a
        (score,table') = nullWindowSearch newState min max table
        (resultList,table'') = nullWindowSearchForAllOptions al gameState min max table'
    in ((a,score):resultList, table'')

nullWindowSearch :: GameState -> Int -> Int -> TranspositionTable-> (Score, TranspositionTable)
nullWindowSearch gameState min max table
    | min >= max = (min ,table)
    | otherwise  =
        let mid = min + ((max - min) `div` 2)
            mid'
                | mid <= 0 && min `div` 2 < mid = min `div` 2
                | mid >= 0 && max `div` 2 > mid = max `div` 2
                | otherwise = mid
            (r,table') = negamax gameState AI mid' (mid' + 1) table
        in if r <= mid'
            then nullWindowSearch gameState min r table'
            else nullWindowSearch gameState r max table'

negamax :: GameState -> Player ->Int -> Int -> TranspositionTable-> (Score,TranspositionTable)
negamax (GameState position mask actionCounter) player alpha beta table
    -- check for draw
    | isDraw actionCounter = (0, table)
    -- if we have no non losing position, that means the opponent will win in the next turn. 
    | nonLosingActions == [] = (negate score, table)
    -- player has not won and we are not in a terminal node; recurse. 
    | otherwise= 
        -- if we changed beta to be lower and now alpha is already bigger than it, we are done. return beta 
        if beta' /= beta && alpha >= beta' then (beta',table)
        --  compute the score of all possible next moves by recursively reaching a terminal node, and keep the best one 
        else exploreTree gameState player nonLosingActions alpha beta' table
    where 
        gameState = (GameState position mask actionCounter)
        score = ((boardSize+1) - actionCounter) `div` 2
        value= getFromTable gameState table
        nonLosingActions = getNonLosingActions gameState player
        max 
            | value /= Nothing =(fromJust value) + minScore - 1
            | otherwise = ((boardSize-1) - actionCounter) `div` 2 --upper bound of our score as we cannot win immediately
        beta' 
            | beta > max = max 
            | otherwise =  beta 

exploreTree ::GameState-> Player-> [ActionBoard] -> Int -> Int -> TranspositionTable-> (Score,TranspositionTable)
exploreTree _ _ [] alpha _ table = (alpha,table)
exploreTree gameState player (a:as) alpha beta table= 
    let
        -- do your move 
        newState = performAction gameState player a 
        -- now the opponent plays in the new state. find score in the children
        (s,table')=  (negamax newState (getOpponent player) (-beta) (-alpha) table)
        score = negate s
    in if score >= beta 
        then (score,table') -- prune if we found something better than what we were looking for , return score
        else let
            alpha' 
                | score > alpha = score
                | otherwise = alpha
            in (alpha',table') 
