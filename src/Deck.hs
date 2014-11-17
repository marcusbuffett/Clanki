module Deck where
import Card
data Deck = Deck {dCards :: [Card], dName :: String} deriving (Show, Eq, Read)

addCard :: Card -> Deck -> Deck
addCard card deck = deck {dCards = (dCards deck) ++ [card]}
