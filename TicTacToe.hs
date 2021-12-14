{-# LANGUAGE GADTs #-}

module Main where

import Data.Maybe
import System.IO

-- some data types to represent the game

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

-- functions for creating the string to visually represent the board

drawValue :: Maybe Value -> Int -> String
drawValue (Just X) _ = " X "
drawValue (Just O) _ = " O "
drawValue Nothing  p = " " ++ show p ++" "

drawSquare :: Square -> String
drawSquare (Square a p _ 0) = drawValue a p ++ ":"
drawSquare (Square a p _ 1) = drawValue a p ++ ":"
drawSquare (Square a p _ _) = drawValue a p

drawBoard :: [Square] -> String
drawBoard (x:[])                                   = drawSquare x
drawBoard ((Square a p x 0):xs) | x == 1 || x == 2 = "\n---+---+---\n" ++ drawSquare (Square a p x 0) ++ drawBoard xs
drawBoard (x:xs)                                   = drawSquare x ++ drawBoard xs

-- a little bit of set-up

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

-- the bulk of the logic

chooseXMove :: [Square] -> Int
chooseXMove [ (Square (Just O) 1 _ _)
            , (Square v2       2 _ _)
	    , (Square v3       3 _ _)
	    , (Square v4       4 _ _)
	    , (Square v5       5 _ _)
	    , _
	    , (Square v7       7 _ _)
	    , _
	    , (Square v9       9 _ _)] | v2 == Just O  && v3 == Nothing = 3
	                               | v2 == Nothing && v3 == Just O  = 2
				       | v4 == Just O  && v7 == Nothing = 7
				       | v4 == Nothing && v7 == Just O  = 4
				       | v5 == Just O  && v9 == Nothing = 9
				       | v5 == Nothing && v9 == Just O  = 5
chooseXMove [ (Square v1       1 _ _)
	    , (Square (Just O) 2 _ _)
	    , (Square v3       3 _ _)
	    , _
	    , (Square v5       5 _ _)
	    , _
	    , _
	    , (Square v8       8 _ _)
	    , _                      ] | v1 == Nothing && v3 == Just O  = 1
	                               | v5 == Just O  && v8 == Nothing = 8
				       | v5 == Nothing && v8 == Just O  = 5
chooseXMove [ _
	    , _
	    , (Square (Just O) 3 _ _)
	    , _
	    , (Square v5       5 _ _)
	    , (Square v6       6 _ _)
	    , (Square v7       7 _ _)
	    , _
	    , (Square v9       9 _ _)] | v5 == Just O  && v7 == Nothing = 7
	                               | v5 == Nothing && v7 == Just O  = 5
				       | v6 == Just O  && v9 == Nothing = 9
				       | v6 == Nothing && v9 == Just O  = 6
chooseXMove [ (Square v1       1 _ _)
            , _
	    , _
	    , (Square (Just O) 4 _ _)
	    , (Square v5       5 _ _)
	    , (Square v6       6 _ _)
	    , (Square v7       7 _ _)
	    , _
	    , _                      ] | v1 == Nothing && v7 == Just O  = 1
	                               | v5 == Just O  && v6 == Nothing = 6
				       | v5 == Nothing && v6 == Just O  = 5
chooseXMove [ (Square v1       1 _ _)
            , (Square v2       2 _ _)
	    , (Square v3       3 _ _)
	    , (Square v4       4 _ _)
	    , (Square (Just O) 5 _ _)
	    , (Square v6       6 _ _)
	    , (Square v7       7 _ _)
	    , (Square v8       8 _ _)
	    , (Square v9       9 _ _)] | v1 == Nothing && v9 == Just O  = 1
	                               | v2 == Nothing && v8 == Just O  = 2
				       | v3 == Nothing && v7 == Just O  = 3
				       | v4 == Nothing && v6 == Just O  = 4
chooseXMove [ _
            , _
	    , (Square Nothing  3 _ _)
	    , _
	    , _
	    , (Square (Just O) 6 _ _)
	    , _
	    , _
	    , (Square (Just O) 9 _ _)]                                  = 3
chooseXMove [ _
            , _
	    , _
	    , _
	    , _
	    , _
	    , (Square (Just O) 7 _ _)
	    , (Square v8       8 _ _)
	    , (Square v9       9 _ _)] | v8 == Just O  && v9 == Nothing = 9
	                               | v8 == Nothing && v9 == Just O  = 8
chooseXMove [ _
            , _
	    , _
	    , _
	    , _
	    , _
	    , (Square Nothing  7 _ _)
	    , (Square (Just O) 8 _ _)
	    , (Square (Just O) 9 _ _)]                                  = 7
chooseXMove xs                                                          = chooseNextXMove xs

chooseNextXMove :: [Square] -> Int
chooseNextXMove ((Square Nothing p _ _):_) = p
chooseNextXMove (_:xs)                     = chooseNextXMove xs

fillSquare :: Value -> Int -> [Square] -> [Square] --need to handle if square is already filled; right now the player could cheat
fillSquare c a ((Square _ p x y):xs) | a == p = ((Square (Just c) p x y):xs)
fillSquare c a (x:xs)                = (x:(fillSquare c a xs))

isWon :: [Square] -> Maybe Value
isWon [(Square a 1 _ _), (Square b 2 _ _), (Square c 3 _ _), _, _, _, _, _, _] | a == b && b == c = a
isWon [_, _, _, (Square a 4 _ _), (Square b 5 _ _), (Square c 6 _ _), _, _, _] | a == b && b == c = a
isWon [_, _, _, _, _, _, (Square a 7 _ _), (Square b 8 _ _), (Square c 9 _ _)] | a == b && b == c = a
isWon [(Square a 1 _ _), _, _, (Square b 4 _ _), _, _, (Square c 7 _ _), _, _] | a == b && b == c = a
isWon [_, (Square a 2 _ _), _, _, (Square b 5 _ _), _, _, (Square c 8 _ _), _] | a == b && b == c = a
isWon [_, _, (Square a 3 _ _), _, _, (Square b 6 _ _), _, _, (Square c 9 _ _)] | a == b && b == c = a
isWon [(Square a 1 _ _), _, _, _, (Square b 5 _ _), _, _, _, (Square c 9 _ _)] | a == b && b == c = a
isWon [_, _, (Square a 3 _ _), _, (Square b 5 _ _), _, (Square c 7 _ _), _, _] | a == b && b == c = a
isWon _                                                                                           = Nothing

isBoardFull :: [Square] -> Bool
isBoardFull (x:[])                      = True
isBoardFull ((Square Nothing _ _ _):xs) = False
isBoardFull (x:xs)                      = isBoardFull xs

keepGoing :: Maybe Value -> Maybe String
keepGoing Nothing   = Nothing
keepGoing (Just X)  = Just "Sorry! You lost!"
keepGoing (Just O)  = Just "Congrats! You won!"

-- the I/O

main = do
  putStrLn (drawBoard startingBoard)
  putStrLn welcomeText
  let board = fillSquare X 7 startingBoard
  putStrLn (drawBoard board)
  playO board

playO board = do
  putStrLn "Time for your turn!"
  hFlush stdout
  choice <- getLine
  let newBoard = fillSquare O (read choice) board -- invalid input
  putStrLn (drawBoard newBoard)
  let winner = isWon newBoard
  let status = if isBoardFull newBoard && winner == Nothing then Just "It's a tie!" else keepGoing (winner) 
  if status == Nothing
    then playX newBoard
    else putStrLn (fromJust status)

playX board = do
  putStrLn "The computer is taking its turn..."
  let newBoard = fillSquare X (chooseXMove board) board
  putStrLn (drawBoard newBoard)
  let winner = isWon newBoard
  let status = if isBoardFull newBoard && winner == Nothing then Just "It's a tie!" else keepGoing (winner) 
  if status == Nothing
    then playO newBoard
    else putStrLn(fromJust status)
