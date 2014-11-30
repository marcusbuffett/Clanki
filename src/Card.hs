module Card where 
import Display
import Text.Printf(printf)
data CardTracker = CardTracker {ctEF :: Float, ctN :: Int, ctTimeQuizzed :: Integer, ctLastResponseQuality :: Maybe Integer} deriving (Show, Read, Eq)

data Card = Card {cardQuestion :: String, cardAnswer :: String, cardTracker :: CardTracker} deriving (Show, Read, Eq)

newCard :: String -> String -> Card
newCard question answer = Card {cardQuestion = question, cardAnswer = answer, cardTracker = newCardTracker} 

newCardTracker :: CardTracker
newCardTracker = CardTracker {ctEF = 2.5, ctN = 0, ctTimeQuizzed = 0, ctLastResponseQuality = Nothing}

instance Display Card where
    display card = cardQuestion card ++ "\n" ++ cardAnswer card

printCard :: Card -> IO ()
printCard card = printf $ cardQuestion card ++ "\n" ++ cardAnswer card

