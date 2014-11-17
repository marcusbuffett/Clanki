module Add where
import Card
import Deck
import Tracker
import Data.List(lookup)
import Data.Char(isSpace)
import ClaskData
import Text.Printf(printf)
import qualified Input
data AddAction = NewDeck | ToDeck deriving (Show, Eq)

addLoop :: ClaskData -> IO ClaskData
addLoop claskData = do
    addAction <- getAddAction claskData
    runAddAction addAction claskData

runAddAction :: Maybe AddAction -> ClaskData -> IO ClaskData
runAddAction addAction claskData
    | addAction == Just NewDeck = newDeck claskData
    | addAction == Just ToDeck  = toDeck claskData
    | addAction == Nothing      = return claskData

newDeck :: ClaskData -> IO ClaskData
newDeck claskData = do
    print "Please input the name of the new deck"
    deckName <- getLine
    case deckName of 
        "" -> do
                return claskData
        _  -> if containsDeckNamed deckName claskData 
                then do
                  print "Invalid input, already a deck with that name"
                  newDeck claskData
                else 
                  return $ addDeckWithName deckName claskData

addDeckWithName :: String -> ClaskData -> ClaskData
addDeckWithName name claskData = addDeckToData deck claskData
    where deck = Deck {dCards = [], dName = name}

toDeck :: ClaskData -> IO ClaskData
toDeck claskData = do
    chosenDeck <- Input.getUserChoice $ decks claskData
    case chosenDeck of
        Just deck -> do
                        deckWithNewItems <- toDeckLoop deck
                        let newDecks = replaceDeck deckWithNewItems (decks claskData)
                        let newTracker = updateTrackerWithCards (dCards deckWithNewItems) (tracker claskData)
                        return $ claskData {decks = newDecks, tracker = newTracker} 
        Nothing -> return claskData

toDeckLoop :: Deck -> IO Deck
toDeckLoop deck = do
    print "Please input the question, enter to stop adding"
    question <- getLine
    case question of 
        "" -> do
                print "You wish to stop adding"
                return deck
        _  -> do
                print "Please input the answer"
                answer <- getLine
                case answer of
                    "" -> do
                        print "You wish to stop adding"
                        return deck
                    _  -> do
                        let card = Card question answer
                        toDeckLoop (addCard card deck)

getAddAction :: ClaskData -> IO (Maybe AddAction)
getAddAction claskData
    | null $ decks claskData = return $ Just NewDeck
    | otherwise = Input.getUserChoice allAddActions

allAddActions :: [AddAction]
allAddActions = [NewDeck, ToDeck]
