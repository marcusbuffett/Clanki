module Remove where
import Card
import Decks
import Tracker
import Data.List(lookup)
import Data.Char(isSpace)
import Display
import Text.Printf(printf)
import qualified Input
data RemoveAction = RemoveDeck | RemoveFromDeck deriving (Show, Eq)

instance Display RemoveAction where
    display removeAct
        | removeAct == RemoveDeck = "Remove a deck"
        | removeAct == RemoveFromDeck = "Remove cards from a deck"

removeLoop :: [Deck] -> IO [Deck]
removeLoop decks = do
    removeAction <- getRemoveAction decks
    updatedDecks <- runRemoveAction removeAction decks
    return $ updatedDecks

runRemoveAction :: Maybe RemoveAction -> [Deck] -> IO [Deck]
runRemoveAction removeAction decks
    | removeAction == Just RemoveDeck     = removeDeck decks
    | removeAction == Just RemoveFromDeck = removeFromDeck decks
    | removeAction == Nothing             = return decks

removeDeck :: [Deck] -> IO [Deck]
removeDeck decks = do
    print "Choose what deck to remove"
    input <- Input.getUserChoice decks
    case input of 
        Just deck -> return $ filter (/= deck) decks
        Nothing   -> return decks


removeFromDeck :: [Deck] -> IO [Deck]
removeFromDeck decks = do
    chosenDeck <- Input.getUserChoice $ decks
    case chosenDeck of
        Just deck -> do
                        updatedDeck <- removeFromDeckLoop deck
                        let newDecks = replaceDeckNamed (dName updatedDeck) updatedDeck decks 
                        return newDecks 
        Nothing -> return decks

removeFromDeckLoop :: Deck -> IO Deck
removeFromDeckLoop deck = do
    input <- Input.getUserChoice $ dCards deck
    case input of 
        Just card  -> removeFromDeckLoop (removeCardFromDeck card deck)
        Nothing    -> return deck

getRemoveAction :: [Deck] -> IO (Maybe RemoveAction)
getRemoveAction decks
    | null decks = return Nothing
    | otherwise = Input.getUserChoice allRemoveActions

allRemoveActions :: [RemoveAction]
allRemoveActions = [RemoveDeck, RemoveFromDeck]
