module Remove where
import Display
import Card
import Text.Printf(printf)
import Data.List(delete)
import qualified Input
data RemoveAction = RemoveDeck | RemoveFromDeck deriving (Show, Eq)

instance Display RemoveAction where
    display RemoveDeck = "Remove a deck"
    display RemoveFromDeck = "Remove cards from a deck"

removeLoop :: [Card] -> IO [Card]
removeLoop cards = do
    removeAction <- getRemoveAction cards
    runRemoveAction removeAction cards

runRemoveAction :: Maybe RemoveAction -> [Card] -> IO [Card]
runRemoveAction (Just RemoveDeck) cards     = removeDeck cards
runRemoveAction (Just RemoveFromDeck) cards = removeFromDeck cards
runRemoveAction Nothing cards               = return cards

removeDeck :: [Card] -> IO [Card]
removeDeck cards = do
    printf $ "Choose what deck to remove" ++ "\n"
    
    input <- Input.getUserChoiceStr $ allDeckNames cards
    case input of 
        Just deckName -> return $ filter (\card -> cardDeck card /= deckName) cards
        Nothing   -> return cards


removeFromDeck :: [Card] -> IO [Card]
removeFromDeck cards = do
    chosenDeckName <- Input.getUserChoiceStr $ allDeckNames cards
    case chosenDeckName of
        Just deckName -> do
                        updatedCards <- removeFromDeckLoop deckName (cardsInDeck deckName cards)
                        let newCards = replaceCardsInDeck deckName updatedCards cards
                        return newCards 
        Nothing -> return cards

removeFromDeckLoop :: String -> [Card] -> IO [Card]
removeFromDeckLoop     _        [] = return []
removeFromDeckLoop deckName cards = do
    input <- Input.getUserChoice cards
    case input of 
        Just card  -> removeFromDeckLoop deckName $ delete card cards
        Nothing    -> return cards

getRemoveAction :: [Card] -> IO (Maybe RemoveAction)
getRemoveAction [] = return Nothing
getRemoveAction decks = do
        printf $ "What would you like to do?" ++ "\n"
        Input.getUserChoice allRemoveActions

allRemoveActions :: [RemoveAction]
allRemoveActions = [RemoveDeck, RemoveFromDeck]
