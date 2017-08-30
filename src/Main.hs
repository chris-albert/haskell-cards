module Main where

import Deck

main :: IO ()
main = do
  deck <- shuffledDeck
  putStrLn $ out $ firstCard deck

firstCard :: Deck -> Maybe Card
firstCard d = fst $ getCard d

out :: Maybe Card -> String
out = show


