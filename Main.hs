import Connected_four
import Constants
import Board
import Data.Maybe 
import Control.Applicative (Alternative(empty))
import System.Random

main :: IO ()
main = do
  putStrLn "Hello, welcome to Connect 4."
  let instructions = "To drop a piece, enter the column you want to drop it at (starting at 1)."
  -- randomly decide who starts
  r <- randomRIO (0, 1 :: Int)
  if r == 0 
    then do
      putStrLn "You have the first turn."
      putStrLn instructions
      userTurn initState emptyTable
      return ()
    else do
      let actionMap = getActionMask emptyBitboard (width `div` 2)
          firstState = performAction initState AI (fromJust actionMap)
      putStrLn "AI has the first turn."
      showBitBoard firstState
      putStrLn instructions
      userTurn firstState emptyTable
      return ()