{-# LANGUAGE GADTs #-}
module Main where

import Deck

import Data.List
import System.Random
import Control.Monad.State
import Control.Monad.Operational
import qualified Data.Map.Strict as M

data PlayerType = Dealer | Player
                deriving (Ord, Eq)

data GameActions a where
    GetAction :: PlayerType -> GameActions Action
    ShowState :: GameActions ()
    Win :: GameActions ()
    Lose :: GameActions ()

-- We run all game logic in the GameS monad, which in turn can run powerful functions of the GameActions type
type GameS = ProgramT GameActions (State Game)

main :: IO ()
main = do
  stdGen <- getStdGen
  interpretIO M.empty (mkGame stdGen) gameLoop

interpretIO :: M.Map PlayerType (Game -> IO Action) -> Game -> GameS a -> IO a
interpretIO strats s instr = case runState (viewT instr) s of
                          (a, ns) -> evalinstr ns a 
    where
        evalinstr   _ (Return x) = return x 
        evalinstr stt (a :>>= f) = 
            let runC a' = interpretIO strats stt (f a')
    in  case a of
            GetAction pt ->
                let strategy = M.findWithDefault (const (return Stay)) pt strats
                in  strategy stt >>= runC
            ShowState -> putStrLn "show state" >>= runC
            Win -> putStrLn "You win" >>= runC
            Lose -> putStrLn "You lose" >>= runC


data Game = Game
  { deck  :: Deck                       -- This is our shuffled deck of cards
  , hands :: M.Map PlayerType [Card]    -- this uses the Data.Map.Strict package to map a player to their cards
  }

data Action = Hit | Stay deriving (Eq, Read)

runAction :: PlayerType -> GameS Action
runAction pt = do
    action <- singleton (GetAction pt)
    when (action == Hit) $ do
        curr <- get
        let (card, deck') = runState draw $ deck curr
        put curr { deck = deck'
                 , hands = M.insertWith (++) pt [card] (hands curr)
                 }
    return action

gameLoop :: GameS ()
gameLoop = do
  playerAction <- runAction Player
  dealerAction <- runAction Dealer
  pbust <- isBust Player
  dbust <- isBust Dealer
  let bothStayed = playerAction == Stay && dealerAction == Stay
      gameOver = bothStayed || pbust || dbust
  if gameOver
      then handleGameOver
      else gameLoop

handleGameOver :: GameS ()
handleGameOver = do
    singleton ShowState
    hs <- gets hands
    let playerH = M.findWithDefault [] Player hs
        dealerH = M.findWithDefault [] Dealer hs
    singleton $ if won playerH dealerH then Win else Lose

won :: [Card] -> [Card] -> Bool
won playerH dealerH = playerScore > dealerScore
  where playerScore = score playerH
        dealerScore = score dealerH

score :: [Card] -> Int
score h
  | bust h    = 0
  | otherwise = best h

isBust :: PlayerType -> GameS Bool
isBust pt = gets (check . hands)
    where
        check h = case M.lookup pt h of
                      Just cs -> bust cs
                      Nothing -> True

bust :: [Card] -> Bool
bust = all (21 <) . possiblePoints

twentyOne :: [Card] -> Bool
twentyOne = elem 21 . possiblePoints

best :: [Card] -> Int
best = maximum . filter (21 >=) . possiblePoints

showDealer :: [Card] -> String
showDealer hand = "[" ++ show (head hand) ++ "," ++ intersperse ',' hidden ++ "]"
  where n = length $ tail hand
        hidden = replicate n '?'

mkGame :: StdGen -> Game
mkGame g = Game
  { deck = d'
  , hands = M.fromList [(Player, playerH), (Dealer, dealerH)]
  }
  where d = execState shuffle $ mkDeck g
        ((playerH, dealerH), d') = runState deal d

deal :: DeckS ([Card], [Card])
deal = do
  mine   <- draw
  yours  <- draw
  mine'  <- draw
  yours' <- draw
  let me = [mine, mine']
      you = [yours, yours']
  return (me, you)