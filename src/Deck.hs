-- Defines a deck of cards
module Deck(
    Suit
  , Value
  , Card
  , Deck
  , freshDeck
  , shuffledDeck
  , cutDeck
  , printDeck
  , getCard
) where

import Data.Array.IO
import System.Random
import Control.Monad
import Shuffle

data Suit = Diamond | Club | Heart | Spade deriving(Eq, Show, Enum)
data Value = Ace | Two | Three | Four | Five | Six |
  Seven | Eight | Nine | Ten | Jack | Queen | King deriving(Eq, Show, Enum)

data Card = Card { suit :: Suit
                 , value :: Value
                 } deriving Eq

instance Show Card where                                  
  show c = show (value c) ++ " of " ++ show (suit c) ++ "s"

type Deck = [Card]

suits = [Diamond .. Spade]
values = [Ace .. King]

freshDeck :: Deck
freshDeck = [Card s v | s <- suits, v <- values]

shuffledDeck :: IO Deck
shuffledDeck = shuffle freshDeck

cutDeck :: Deck -> Int -> Deck
cutDeck d i = drop i d ++ take i d

printDeck :: Deck -> IO ()
printDeck = mapM_ print

-- This is really a State monad
getCard :: Deck -> (Maybe Card, Deck)
getCard []     = (Nothing, [])
getCard (c:cs) = (Just c, cs)
