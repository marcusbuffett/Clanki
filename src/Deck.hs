module Deck where
import Card
import Display
import Data.List(delete)
data Deck = Deck {dCards :: [Card], dName :: String} deriving (Show, Eq, Read)

instance Display Deck where
    display deck = dName deck -- ++ "\n" ++ concat [display card ++ "\n" | card <- dCards deck] ++ "--------"

addCardToDeck :: Card -> Deck -> Deck
addCardToDeck card deck = deck {dCards = (dCards deck) ++ [card]}

removeCardFromDeck :: Card -> Deck -> Deck
removeCardFromDeck card deck = deck {dCards = delete card (dCards deck)}
