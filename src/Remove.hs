module Remove where
import Decks
import Display
import Text.Printf(printf)
import qualified Input
data RemoveAction = RemoveDeck | RemoveFromDeck deriving (Show, Eq)

instance Display RemoveAction where
    display RemoveDeck = "Remove a deck"
    display RemoveFromDeck = "Remove cards from a deck"

removeLoop :: [Deck] -> IO [Deck]
removeLoop decks = do
    removeAction <- getRemoveAction decks
    runRemoveAction removeAction decks

runRemoveAction :: Maybe RemoveAction -> [Deck] -> IO [Deck]
runRemoveAction (Just RemoveDeck) decks     = removeDeck decks
runRemoveAction (Just RemoveFromDeck) decks = removeFromDeck decks
runRemoveAction Nothing decks               = return decks

removeDeck :: [Deck] -> IO [Deck]
removeDeck decks = do
    printf $ "Choose what deck to remove" ++ "\n"
    input <- Input.getUserChoice decks
    case input of 
        Just deck -> return $ filter (/= deck) decks
        Nothing   -> return decks


removeFromDeck :: [Deck] -> IO [Deck]
removeFromDeck decks = do
    chosenDeck <- Input.getUserChoice $ filter (not . null . dCards) decks
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
        Just card  -> if not $ null (dCards deck)
                        then removeFromDeckLoop $ removeCardFromDeck card deck
                        else return deck
        Nothing    -> return deck

getRemoveAction :: [Deck] -> IO (Maybe RemoveAction)
getRemoveAction [] = return Nothing
getRemoveAction decks
    | all (null . dCards) decks = return $ Just RemoveDeck
    | otherwise  = Input.getUserChoice allRemoveActions

allRemoveActions :: [RemoveAction]
allRemoveActions = [RemoveDeck, RemoveFromDeck]
