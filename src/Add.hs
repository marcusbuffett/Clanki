module Add where
import Card
import Display
import Text.Printf(printf)
import qualified Input
data AddAction = NewDeck | ToDeck deriving (Show, Eq)

instance Display AddAction where
    display NewDeck = "New deck"
    display ToDeck  = "Add to deck"

addLoop :: [Card] -> IO [Card]
addLoop decks = do
    addAction <- getAddAction decks
    runAddAction addAction decks

runAddAction :: Maybe AddAction -> [Card] -> IO [Card]
runAddAction = maybe return newOrTo
  where
      newOrTo NewDeck = newDeck
      newOrTo ToDeck  = toDeck

newDeck :: [Card] -> IO [Card]
newDeck cards = do
    deckName <- Input.sameLinePrompt "New deck name : "
    case deckName of 
        "" -> return cards
        _  -> if any (\card -> cardDeck card == deckName) cards
                then do
                  printf $ "Invalid input, already a deck with that name" ++ "\n"
                  newDeck cards
                else 
                  toDeckLoop deckName cards

toDeck :: [Card] -> IO [Card]
toDeck cards = do
    chosenDeckName <- Input.getUserChoiceStr $ allDeckNames cards
    case chosenDeckName of
        Just deckName -> do
                        newCards <- toDeckLoop deckName cards
                        return newCards
        Nothing -> return cards


toDeckLoop :: String -> [Card] -> IO [Card]
toDeckLoop deckName cards = do
    question <- Input.sameLinePrompt "Add question (<Enter> to stop adding) : "

    case question of 
        "" -> do
                {-printf $ "You wish to stop adding" ++ "\n"-}
                printf "\n"
                return cards
        _  -> do
                answer <- Input.sameLinePrompt "Add answer : "
                case answer of
                    "" -> do
                        return cards
                    _  -> do
                        let card = newCard question answer deckName
                        toDeckLoop deckName (card:cards)

getAddAction :: [Card] -> IO (Maybe AddAction)
getAddAction [] = return $ Just NewDeck
getAddAction _  = Input.getUserChoice allAddActions

allAddActions :: [AddAction]
allAddActions = [NewDeck, ToDeck]
