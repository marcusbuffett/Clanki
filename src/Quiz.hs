module Quiz where
import Card
import Decks
import Tracker
import Display
import Text.Printf(printf)
import Safe(readMay)
import Data.Time.Clock.POSIX
import Data.Maybe(isJust, fromJust)
import qualified Input
import Control.Monad(filterM)
import Data.List((\\), isInfixOf)
data QuizAction = AllCards | FromDeck deriving (Show, Eq)

allQuizActions :: [QuizAction]
allQuizActions = [AllCards, FromDeck]

instance Display QuizAction where
    display AllCards = "Quiz from all cards"
    display FromDeck = "Quiz from a deck"

quizLoop :: [Deck] -> IO [Deck]
quizLoop decks = do
    quizAction <- getQuizAction decks
    runQuizAction quizAction decks

anyCardsToQuiz :: [Deck] -> IO Bool
anyCardsToQuiz decks = shouldQuizList >>= return . (True `elem`)
    where shouldQuizList = sequence [shouldQuizCard card | card <- allCards decks] 

runQuizAction :: Maybe QuizAction -> [Deck] -> IO [Deck]
runQuizAction (Just AllCards) decks = quizDecks decks decks
runQuizAction (Just FromDeck) decks = do
        input <- Input.getUserChoice decks
        case input of
            Just deck -> quizFromDeck deck decks
            Nothing   -> return decks
runQuizAction Nothing decks = return decks

quizDecks :: [Deck] -> [Deck] -> IO [Deck]
quizDecks decksToQuiz allDecks = sequence $ map (\deck -> if deck `elem` decksToQuiz then quizDeck deck else return deck) allDecks

quizDeck :: Deck -> IO Deck
quizDeck deck = do
    newCards <- sequence [maybeQuiz card | card <- dCards deck]
    return deck {dCards = newCards}


maybeQuiz :: Card -> IO Card
maybeQuiz card = shouldQuizCard card >>= (\shouldQuiz -> if shouldQuiz then quizCard card else return card)

cardsToQuiz :: [Deck] -> IO [Card]
cardsToQuiz decks = filterM shouldQuizCard cards
    where cards = allCards decks
    

{-quizCards :: [Card] -> [Deck] -> IO [Deck]-}
{-quizCards cards decks = return decks-}
    {-newTrackers <- sequence [quizCard card tracker | card <- cards, let tracker = trackerOfCard card (cardTrackers decks)]-}
    {--- TODO: replace trackers, but not all -}
    {-return $ decks {cardTrackers = newTrackers}-}

quizCard :: Card -> IO Card
quizCard card = do
    printf $ "Question : " ++ cardQuestion card ++ "\n"
    printf "Input your answer, then press enter to continue\n"
    answer <- getLine
    confidence <- getConfidence answer (cardAnswer card)
    let newEF = adjustEF (ctEF $ cardTracker card) confidence
    let oldTracker = cardTracker card
    let n = if confidence < 3 then 1 else ctN oldTracker + 1
    currentTime <- fmap round getPOSIXTime
    printf "\n"
    return $ card {cardTracker = oldTracker {ctEF = newEF, ctN = n, ctLastResponseQuality = Just confidence, ctTimeQuizzed = currentTime}}


getConfidence :: String -> String -> IO Int
getConfidence answer correctAnswer
    | isJust inferredConfidence = return $ fromJust inferredConfidence
    | otherwise = do
            printf $ "Correct answer : " ++ correctAnswer ++ "\n"
            printf $ "Rate your answer on a scale from 0-5" ++ "\n"
            promptConfidence
    where
    inferredConfidence = inferConfidence answer correctAnswer

inferConfidence :: String -> String -> Maybe Int
inferConfidence answer correctAnswer
    | rightAnswer =
        case answer \\ correctAnswer of
            "!" -> Just 5
            "." -> Just 4
            "?" -> Just 3
            _   -> Nothing
    | otherwise   = Nothing
        where rightAnswer = correctAnswer `isInfixOf` answer

promptConfidence :: IO Int
promptConfidence = do
    input <- getLine 
    let readInput = readMay input :: Maybe Int
    case readInput of
        Just confidence -> if confidence `elem` [1 .. 5] then return confidence else promptConfidence
        Nothing         -> promptConfidence

quizFromDeck :: Deck -> [Deck] -> IO [Deck]
quizFromDeck deck decks = quizDeck deck >>= (\newDeck -> return $ replaceDeckNamed (dName deck) newDeck decks)

getQuizAction :: [Deck] -> IO (Maybe QuizAction)
getQuizAction [] = do
            printf $ "No decks, returning to menu" ++ "\n"
            return Nothing
getQuizAction decks = do
    cardsToBeQuizzed <- cardsToQuiz decks
    case cardsToBeQuizzed of
        [] -> do
            printf $ "No cards need to be quizzed at this time" ++ "\n" 
            return Nothing
        _  -> do
            printf $ "What would you like to be quizzed on?" ++ "\n"
            Input.getUserChoice allQuizActions
