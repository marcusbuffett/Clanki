module Card where 
import Display
import Text.Printf(printf)
import Data.List(nub, intercalate)
import StatTracker
data CardTracker = CardTracker {ctEF :: Float, ctN :: Int, ctTimeQuizzed :: Int, ctLastResponseQuality :: Maybe Int} deriving (Show, Read, Eq)

data Card = Card {cardQuestion :: String, cardAnswer :: String, cardDeck :: String, cardTracker :: CardTracker, cardStatTracker :: StatTracker} deriving (Show, Read, Eq)

newCard :: String -> String -> String -> Card
newCard question answer deckName = Card {cardQuestion = question, cardAnswer = answer, cardDeck = deckName, cardTracker = newCardTracker, cardStatTracker = newStatTracker}

newCardTracker :: CardTracker
newCardTracker = CardTracker {ctEF = 2.5, ctN = 0, ctTimeQuizzed = 0, ctLastResponseQuality = Nothing}

instance Display Card where
    display card = "Q : " ++ cardQuestion card ++ "\n" ++ "A : " ++ cardAnswer card

printCard :: Card -> IO ()
printCard card = printf $ "Q : " ++ cardQuestion card ++ "\n" ++ "A : " ++ cardAnswer card

allDeckNames :: [Card] -> [String]
allDeckNames cards = nub $ [cardDeck card | card <- cards]

cardsInDeck :: String -> [Card] -> [Card]
cardsInDeck deckName cards = filter (\card -> cardDeck card == deckName) cards

replaceCardsInDeck :: String -> [Card] -> [Card] -> [Card]
replaceCardsInDeck deckName newCards allCards = do
    let cardsOldDeckRemoved = filter (\card -> cardDeck card /= deckName) allCards
    cardsOldDeckRemoved ++ newCards

displayCardsInDeck :: String -> [Card] -> String
displayCardsInDeck deckName cards = 
    prependDeckName . appendNewLine . displayListCards $ deckCards
    where
        prependDeckName = (deckName ++) . ("\n" ++)
        appendNewLine = (++ "\n")
        deckCards = filter (\card -> cardDeck card == deckName) cards

displayListCards :: [Card] -> String
displayListCards cards = intercalate "\n" . map display $ cards

displayAllDecks :: [Card] -> IO ()
displayAllDecks cards = do
    let deckNames = allDeckNames cards
    let cardsDisplayed = intercalate "\n" $ map (\dName -> displayCardsInDeck dName cards) deckNames
    printf $ cardsDisplayed ++ "\n"

hasDeckNamed :: String -> [Card] -> Bool
hasDeckNamed deckName cards = not . null $ filter (\card -> cardDeck card == deckName) cards
