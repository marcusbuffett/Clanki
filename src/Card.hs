module Card where 
import Display
import Text.Printf(printf)
import Data.List(nub)
import StatTracker
data CardTracker = CardTracker {ctEF :: Float, ctN :: Int, ctTimeQuizzed :: Int, ctLastResponseQuality :: Maybe Int} deriving (Show, Read, Eq)

data Card = Card {cardQuestion :: String, cardAnswer :: String, cardDeck :: String, cardTracker :: CardTracker, cardStatTracker :: StatTracker} deriving (Show, Read, Eq)

newCard :: String -> String -> String -> Card
newCard question answer deckName = Card {cardQuestion = question, cardAnswer = answer, cardDeck = deckName, cardTracker = newCardTracker, cardStatTracker = newStatTracker}

newCardTracker :: CardTracker
newCardTracker = CardTracker {ctEF = 2.5, ctN = 0, ctTimeQuizzed = 0, ctLastResponseQuality = Nothing}

instance Display Card where
    display card = cardQuestion card ++ "\n" ++ cardAnswer card

printCard :: Card -> IO ()
printCard card = printf $ cardQuestion card ++ "\n" ++ cardAnswer card

allDeckNames :: [Card] -> [String]
allDeckNames cards = nub $ [cardDeck card | card <- cards]

cardsInDeck :: String -> [Card] -> [Card]
cardsInDeck deckName cards = filter (\card -> cardDeck card == deckName) cards

replaceCardsInDeck :: String -> [Card] -> [Card] -> [Card]
replaceCardsInDeck deckName newCards allCards = do
    let cardsOldDeckRemoved = filter (\card -> cardDeck card /= deckName) allCards
    cardsOldDeckRemoved ++ newCards
