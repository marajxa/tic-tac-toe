{-# LANGUAGE GADTs #-}

module TicTacToe where

import Data.Maybe
import System.IO

data Value where
  X     :: Value
  O     :: Value
  deriving (Eq)
instance Show (Value) where
  show X = "X"
  show O = "O"

data Square where
  Square :: { value    :: Maybe Value
            , position :: Int
            , xCord    :: Int
	    , yCord    :: Int } -> Square
instance Show (Square) where
  show (Square v p x y) = "(Square " ++ show v ++ " " ++ show p ++ " " ++ show x ++ " " ++ show y ++ ")"

drawValue :: Maybe Value -> Int -> String
drawValue (Just X) _ = " X "
drawValue (Just O) _ = " O "
drawValue Nothing  p = " " ++ show p ++" "

drawSquare :: Square -> String
drawSquare (Square a p _ 0) = drawValue a p ++ ":"
drawSquare (Square a p _ 1) = drawValue a p ++ ":"
drawSquare (Square a p _ _) = drawValue a p

drawBoard :: [Square] -> String
drawBoard (x:[])                                 = drawSquare x
drawBoard ((Square a p x 0):xs) | x == 1 || x == 2 = "\n---+---+---\n" ++ drawSquare (Square a p x 0) ++ drawBoard xs
drawBoard (x:xs)                                 = drawSquare x ++ drawBoard xs

startingBoard :: [Square]
startingBoard = [ (Square Nothing 1 0 0)
                , (Square Nothing 2 0 1)
		, (Square Nothing 3 0 2)
		, (Square Nothing 4 1 0)
		, (Square Nothing 5 1 1)
		, (Square Nothing 6 1 2)
		, (Square Nothing 7 2 0)
		, (Square Nothing 8 2 1)
		, (Square Nothing 9 2 2)]

welcomeText :: String
welcomeText = "Welcome to Tic-Tac-Toe! You will be playing as O. Please enter the number that corresponds to the square you would like to mark."

takeTurn :: Value -> Int -> [Square] -> [Square]
takeTurn c p b = fillSquare c (chooseMove b) b

chooseMove :: [Square] -> Int
chooseMove _ = undefined

fillSquare :: Value -> Int -> [Square] -> [Square] --need to handle if square is already filled
fillSquare c a ((Square _ p x y):xs) | a == p = ((Square (Just c) p x y):xs)
fillSquare c a (x:xs)                = (x:(fillSquare c a xs))

isWon :: [Square] -> Maybe Value -- inefficient and hangs: need to fix
isWon [(Square a 1 0 0), (Square b 2 0 1), (Square c 3 0 2), _, _, _, _, _, _] | a == b && b == c = a
isWon [_, _, _, (Square a 4 1 0), (Square b 5 1 1), (Square c 6 1 2), _, _, _] | a == b && b == c = a
isWon [_, _, _, _, _, _, (Square a 7 2 0), (Square b 8 2 1), (Square c 9 2 2)] | a == b && b == c = a
isWon [(Square a 1 0 0), _, _, (Square b 4 1 0), _, _, (Square c 7 2 0), _, _] | a == b && b == c = a
isWon [_, (Square a 2 0 1), _, _, (Square b 5 1 1), _, _, (Square c 8 2 1), _] | a == b && b == c = a
isWon [_, _, (Square a 3 0 2), _, _, (Square b 6 1 2), _, _, (Square c 9 2 2)] | a == b && b == c = a
isWon [(Square a 1 0 0), _, _, _, (Square b 5 1 1), _, _, _, (Square c 9 2 2)] | a == b && b == c = a
isWon [_, _, (Square a 3 0 2), _, (Square b 5 1 1), _, (Square c 7 2 0), _, _] | a == b && b == c = a
isWon _                                                                                           = Nothing

keepGoing :: Maybe Value -> Maybe String
keepGoing Nothing   = Nothing
keepGoing (Just X)  = Just "Sorry! You lost!"
keepGoing (Just O)  = Just "Congrats! You won!"

startPlay = do
  putStrLn (drawBoard startingBoard)
  putStrLn welcomeText
  let board = fillSquare X 7 startingBoard
  putStrLn (drawBoard board)
  play board

play board = do
  hFlush stdout
  choice <- getLine
  let newBoard = fillSquare O (read choice) board -- invalid input
  putStrLn (drawBoard newBoard)
  let status = keepGoing (isWon newBoard)
  putStrLn (show newBoard)
  putStrLn (show status)
  if status == Nothing
    then play newBoard
    else putStrLn (fromJust status)