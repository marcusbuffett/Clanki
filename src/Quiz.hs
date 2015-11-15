module Quiz(quizLoop, quizCards, quizFromDeck, quizSomeCards) where
import Card
import Tracker
import StatTracker
import Display
import Text.Printf(printf)
import Safe(readMay)
import Data.Time.Clock.POSIX
import Data.Maybe(isJust)
import qualified Input
import Control.Monad(filterM, liftM)
import System.IO(hSetBuffering, BufferMode(NoBuffering, LineBuffering), stdin, stdout, hFlush)
import Data.List((\\), isInfixOf, delete)
data QuizAction = AllCards | FromDeck deriving (Show, Eq)

allQuizActions :: [QuizAction]
allQuizActions = [AllCards, FromDeck]

instance Display QuizAction where
    display AllCards = "Quiz from all cards"
    display FromDeck = "Quiz from a deck"

quizLoop :: [Card] -> IO [Card]
quizLoop cards = do
    quizAction <- getQuizAction cards
    runQuizAction quizAction cards

anyCardsToQuiz :: [Card] -> IO Bool
anyCardsToQuiz cards = liftM (True `elem`) shouldQuizList
    where shouldQuizList = sequence $ fmap shouldQuizCard cards

runQuizAction :: Maybe QuizAction -> [Card] -> IO [Card]
runQuizAction (Just AllCards) cards = cardsToQuiz cards >>= (\someCards -> quizSomeCards someCards cards)
runQuizAction (Just FromDeck) cards = do
        input <- Input.getUserChoiceStr $ allDeckNames cards
        case input of
            Just deckName -> quizFromDeck deckName cards
            Nothing   -> return cards
runQuizAction Nothing decks = return decks

cardsToQuiz :: [Card] -> IO [Card]
cardsToQuiz = filterM shouldQuizCard

quizCards :: [Card] -> IO [Card]
quizCards []    = return []
quizCards cards = quizSomeCards cards cards

quizSomeCards :: [Card] -> [Card] -> IO [Card]
quizSomeCards subset set = do
    printf "Press <Enter> at any time to stop quizzing\n"
    quizSomeCards' subset set 
    where
    quizSomeCards' cards allCards
        | null cards = return allCards
        | otherwise = do
        quizResult <- quizCard headCard
        case quizResult of
            Nothing -> return allCards
            Just updatedCard -> do
                needsRequizzing <- shouldQuizCard updatedCard
                if needsRequizzing
                    then quizSomeCards' (restOfCards ++ [updatedCard]) allCards
                    else do
                        let newAllCards = (updatedCard:) . delete headCard $ allCards 
                        quizSomeCards' restOfCards newAllCards
        where
        headCard = head cards
        restOfCards = tail cards

quizCard :: Card -> IO (Maybe Card)
quizCard card = do
    printf $ "Question : " ++ cardQuestion card ++ "\n"
    answer <- Input.sameLinePrompt "Answer   : "
    confidence <- getConfidence answer (cardAnswer card)
    case confidence of
        Just conf -> do
            let newEF = adjustEF (ctEF $ cardTracker card) conf
            let oldTracker = cardTracker card
            let n = if conf < 3 then 1 else ctN oldTracker + 1
            currentTime <- fmap round getPOSIXTime
            printf "\n\n"
            let st = cardStatTracker card
            let newST = updateStatTracker st conf
            let newTracker = oldTracker {ctEF = newEF, ctN = n, ctLastResponseQuality = Just conf, ctTimeQuizzed = currentTime}
            return . Just $ card {cardTracker = newTracker, cardStatTracker = newST}
        Nothing   -> return Nothing 

getConfidence :: String -> String -> IO (Maybe Int)
getConfidence answer correctAnswer
    | isJust inferredConfidence = return inferredConfidence
    | otherwise = do
            printf $ "Correct answer : " ++ correctAnswer ++ "\n"
            printf "Rate your answer on a scale from 0-5 : "
            hFlush stdout
            promptConfidence
    where
    inferredConfidence = inferConfidence answer correctAnswer

{- Default values to use when the user is right, not yet implemented -}
inferConfidence :: String -> String -> Maybe Int
inferConfidence answer correctAnswer
    | rightAnswer = Nothing
    | otherwise   = Nothing
        where rightAnswer = correctAnswer == answer

promptConfidence :: IO (Maybe Int)
promptConfidence = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    input <- fmap (:[]) getChar 
    let readInput = readMay input :: Maybe Int
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    case readInput of
        Just confidence -> if confidence `elem` [0 .. 5] then return $ Just confidence else promptConfidence
        Nothing         -> printf "\n" >> return Nothing

quizFromDeck :: String -> [Card] -> IO [Card]
quizFromDeck deckName cards = do
    cardsToBeQuizzed <- cardsToQuiz deckCards
    quizSomeCards cardsToBeQuizzed cards
    where
    deckCards = cardsInDeck deckName cards

getQuizAction :: [Card] -> IO (Maybe QuizAction)
getQuizAction [] = do
            printf $ "No decks, returning to menu" ++ "\n"
            return Nothing
getQuizAction cards = do
    cardsToBeQuizzed <- cardsToQuiz cards
    case cardsToBeQuizzed of
        [] -> do
            printf $ "No cards need to be quizzed at this time" ++ "\n" 
            return Nothing
        _  -> case allDeckNames cards of
                [_] -> return $ Just AllCards
                _   -> do
                    printf $ "What would you like to be quizzed on?" ++ "\n"
                    Input.getUserChoice allQuizActions
