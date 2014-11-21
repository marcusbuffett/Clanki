module Remove where
import Card
import Deck
import Tracker
import Data.List(lookup)
import Data.Char(isSpace)
import ClaskData
import Display
import Text.Printf(printf)
import qualified Input
data RemoveAction = RemoveDeck | RemoveFromDeck deriving (Show, Eq)

instance Display RemoveAction where
    display removeAct
        | removeAct == RemoveDeck = "Remove a deck"
        | removeAct == RemoveFromDeck = "Remove cards from a deck"

removeLoop :: ClaskData -> IO ClaskData
removeLoop claskData = do
    removeAction <- getRemoveAction claskData
    updatedClaskData <- runRemoveAction removeAction claskData
    newTracker <- trimUnusedCards (allCards updatedClaskData) (tracker claskData) 
    return $ updatedClaskData {tracker = newTracker}

runRemoveAction :: Maybe RemoveAction -> ClaskData -> IO ClaskData
runRemoveAction removeAction claskData
    | removeAction == Just RemoveDeck     = removeDeck claskData
    | removeAction == Just RemoveFromDeck = removeFromDeck claskData
    | removeAction == Nothing             = return claskData

removeDeck :: ClaskData -> IO ClaskData
removeDeck claskData = do
    print "Choose what deck to remove"
    input <- Input.getUserChoice $ decks claskData
    case input of 
        Just deck -> return $ claskData {decks = removeDeckNamed (dName deck) (decks claskData)}
        Nothing   -> return claskData


removeFromDeck :: ClaskData -> IO ClaskData
removeFromDeck claskData = do
    chosenDeck <- Input.getUserChoice $ decks claskData
    case chosenDeck of
        Just deck -> do
                        deckWithItemsRemoved <- removeFromDeckLoop deck
                        let newDecks = replaceDeck deckWithItemsRemoved (decks claskData)
                        let newTracker = updateTrackerWithCards (dCards deckWithItemsRemoved) (tracker claskData)
                        return $ claskData {decks = newDecks, tracker = newTracker} 
        Nothing -> return claskData

removeFromDeckLoop :: Deck -> IO Deck
removeFromDeckLoop deck = do
    input <- Input.getUserChoice $ dCards deck
    case input of 
        Just card  -> removeFromDeckLoop (removeCardFromDeck card deck)
        Nothing    -> return deck

getRemoveAction :: ClaskData -> IO (Maybe RemoveAction)
getRemoveAction claskData
    | null $ decks claskData = return Nothing
    | otherwise = Input.getUserChoice allRemoveActions

allRemoveActions :: [RemoveAction]
allRemoveActions = [RemoveDeck, RemoveFromDeck]
