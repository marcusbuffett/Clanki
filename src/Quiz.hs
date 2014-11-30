module Quiz where
import Card
import Decks
import Tracker
import Data.List(lookup)
import Data.Char(isSpace)
import Display
import Text.Printf(printf)
import Safe(readMay)
import Data.Time.Clock.POSIX
import qualified Input
data QuizAction = AllCards | FromDeck deriving (Show, Eq)

allQuizActions :: [QuizAction]
allQuizActions = [AllCards, FromDeck]

instance Display QuizAction where
    display quizAct 
        | quizAct == AllCards = "Quiz from all cards"
        | quizAct == FromDeck = "Quiz from a deck"

quizLoop :: [Deck] -> IO [Deck]
quizLoop decks = do
    quizAction <- getQuizAction decks
    runQuizAction quizAction decks

anyCardsToQuiz :: [Deck] -> IO Bool
anyCardsToQuiz decks = do
    let cards = allCards decks
    sequence [shouldQuizCard card | card <- cards] >>= return . (True `elem`)

runQuizAction :: Maybe QuizAction -> [Deck] -> IO [Deck]
runQuizAction quizAct decks
    | quizAct == Just AllCards = quizDecks decks decks
    | quizAct == Just FromDeck = do
        input <- Input.getUserChoice decks
        case input of
            Just deck -> return decks
            Nothing   -> return decks
    | otherwise = return decks

quizDecks :: [Deck] -> [Deck] -> IO [Deck]
quizDecks decksToQuiz allDecks = sequence $ map (\deck -> if deck `elem` decksToQuiz then quizDeck deck else return deck) allDecks

quizDeck :: Deck -> IO Deck
quizDeck deck = do
    newCards <- sequence [maybeQuiz card | card <- dCards deck]
    return deck {dCards = newCards}


maybeQuiz :: Card -> IO Card
maybeQuiz card = shouldQuizCard card >>= (\shouldQuiz -> if shouldQuiz then quizCard card else return card)
    

{-quizCards :: [Card] -> [Deck] -> IO [Deck]-}
{-quizCards cards decks = return decks-}
    {-newTrackers <- sequence [quizCard card tracker | card <- cards, let tracker = trackerOfCard card (cardTrackers decks)]-}
    {--- TODO: replace trackers, but not all -}
    {-return $ decks {cardTrackers = newTrackers}-}

quizCard :: Card -> IO Card
quizCard card = do
    printf $ "Question : " ++ cardQuestion card ++ "\n"
    printf "Input your answer, then press enter to continue\n"
    _ <- getLine
    --printf $ "Answer : " ++ cardAnswer card ++ "\n"
    printf $ "Rate your answer, 0-5" ++ "\n"
    confidence <- getAnswerConfidence
    let newEF = adjustEF (ctEF $ cardTracker card) confidence
    let oldTracker = cardTracker card
    let n = if confidence < 3 then 1 else ctN oldTracker + 1
    currentTime <- fmap round getPOSIXTime
    printf "\n"
    return $ card {cardTracker = oldTracker {ctEF = newEF, ctN = n, ctLastResponseQuality = Just confidence, ctTimeQuizzed = currentTime}}

getAnswerConfidence :: IO Integer
getAnswerConfidence = do
    input <- getLine 
    let readInput = readMay input :: Maybe Integer
    case readInput of
        Just confidence -> if confidence `elem` [1 .. 5] then return confidence else getAnswerConfidence
        Nothing         -> getAnswerConfidence




{-quizFromDeck :: Deck -> [Deck] -> IO [Deck]-}
{-quizFromDeck deck decks = return decks-}

getQuizAction :: [Deck] -> IO (Maybe QuizAction)
getQuizAction decks = Input.getUserChoice allQuizActions
    

