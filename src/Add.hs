module Add where
import Card
import Decks
import Tracker
import Data.List(lookup)
import Data.Char(isSpace)
import Display
import Text.Printf(printf)
import qualified Input
data AddAction = NewDeck | ToDeck deriving (Show, Eq)

instance Display AddAction where
    display addAct = show addAct

addLoop :: [Deck] -> IO [Deck]
addLoop decks = do
    addAction <- getAddAction decks
    runAddAction addAction decks

runAddAction :: Maybe AddAction -> [Deck] -> IO [Deck]
runAddAction addAction decks
    | addAction == Just NewDeck = newDeck decks
    | addAction == Just ToDeck  = toDeck decks
    | addAction == Nothing      = return decks

newDeck :: [Deck] -> IO [Deck]
newDeck decks = do
    print "Please input the name of the new deck"
    deckName <- getLine
    case deckName of 
        "" -> do
                return decks
        _  -> if any (\deck -> dName deck == deckName) decks
                then do
                  print "Invalid input, already a deck with that name"
                  newDeck decks
                else 
                  return $ addDeckWithName deckName decks

addDeckWithName :: String -> [Deck] -> [Deck]
addDeckWithName name decks = decks ++ [Deck {dCards = [], dName = name}]

toDeck :: [Deck] -> IO [Deck]
toDeck decks = do
    chosenDeck <- Input.getUserChoice $ decks
    case chosenDeck of
        Just deck -> do
                        deckWithNewItems <- toDeckLoop deck
                        let newDecks = replaceDeckNamed (dName deck) deckWithNewItems decks
                        return $ newDecks
        Nothing -> return decks


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
                        let card = newCard question answer
                        toDeckLoop (addCardToDeck card deck)

getAddAction :: [Deck] -> IO (Maybe AddAction)
getAddAction decks
    | null decks = return $ Just NewDeck
    | otherwise = Input.getUserChoice allAddActions

allAddActions :: [AddAction]
allAddActions = [NewDeck, ToDeck]
