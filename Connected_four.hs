module Connected_four where

import Data.Maybe
import Constants
import MinMaxSearch
import Board

aiTurn :: GameState -> TranspositionTable -> IO GameState
aiTurn gameState table = do
    let 
        (GameState position mask actionCounter) = gameState
        winningActions= getWinningActions gameState AI 
        -- check if we can win with this move 
        canWin =  winningActions /= []
        (actionMask,table') 
            -- if it can, then do the winning action
            | canWin = (head winningActions, table)
            -- if not, then search for best solution
            | otherwise = alphaBetaSearch gameState table
        newGameState = performAction gameState AI actionMask
        (GameState _ _ newActionCounter) = newGameState

    showBitBoard newGameState
    putStrLn""
    if canWin
        then do
            putStrLn $ "Computer won! It took " ++ show newActionCounter ++ " moves in total."
            return newGameState
        else if isDraw newActionCounter
            then do
                putStrLn "Game over. Result is a draw."
                return newGameState
            else do
                userTurn newGameState table'

userTurn :: GameState -> TranspositionTable -> IO GameState
userTurn gameState table= do
    let (GameState position mask actionCounter) = gameState
    -- get user input 
    putStrLn "Enter your move:"
    a <- getUserInput 
    actionMask <- isValidActionWithMessage mask a
    -- if action is not valid, ask for input again
    if actionMask == Nothing
        then do
            putStrLn "Invalid action. Please try again."
            userTurn gameState table
        -- action is valid. perform it
        else do
            let 
                newGameState = performAction gameState User (fromJust actionMask)
                (GameState newPosition newMask newActionCounter) = newGameState
            showBitBoard newGameState
            putStrLn "" 
            if connectedFour newPosition
                then do
                    putStrLn $ "You won! It took " ++ show newActionCounter ++ "  moves in total."
                    return newGameState
                else if isDraw newActionCounter
                    then do
                        putStrLn "Game over. Result is a draw."
                        return newGameState
                    else aiTurn newGameState table