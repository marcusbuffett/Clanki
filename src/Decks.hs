module Decks where
import Card
import Display
import Data.List(delete)
data Deck = Deck {dCards :: [Card], dName :: String} deriving (Show, Eq, Read)

instance Display Deck where
    display = dName -- ++ "\n" ++ concat [display card ++ "\n" | card <- dCards deck] ++ "--------"

addCardToDeck :: Card -> Deck -> Deck
addCardToDeck card deck = deck {dCards = dCards deck ++ [card]}

removeCardFromDeck :: Card -> Deck -> Deck
removeCardFromDeck card deck = deck {dCards = delete card (dCards deck)}

allCards :: [Deck] -> [Card]
allCards decks = concat [dCards deck | deck <- decks]

replaceDeckNamed :: String -> Deck -> [Deck] -> [Deck]
replaceDeckNamed deckName deck decks = filter (\d -> dName d /= deckName) decks ++ [deck]

