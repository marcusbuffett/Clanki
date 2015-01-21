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
import Control.Monad(filterM)
import System.IO(hSetBuffering, BufferMode(NoBuffering, LineBuffering), stdin, stdout, hFlush)
import Data.List((\\), isInfixOf, delete)
{-import StatTracker-}
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
anyCardsToQuiz cards = shouldQuizList >>= return . (True `elem`)
    where shouldQuizList = sequence [shouldQuizCard card | card <- cards] 

runQuizAction :: Maybe QuizAction -> [Card] -> IO [Card]
runQuizAction (Just AllCards) cards = cardsToQuiz cards >>= (\someCards -> quizSomeCards someCards cards)
runQuizAction (Just FromDeck) cards = do
        input <- Input.getUserChoiceStr $ allDeckNames cards
        case input of
            Just deckName -> quizFromDeck deckName cards
            Nothing   -> return cards
runQuizAction Nothing decks = return decks

{-quizCards :: [Card] -> IO [Card]-}
{-quizCards cards = do-}
    {-newCards <- sequence [maybeQuiz card | card <- cards]-}
    {-return cards-}


{-maybeQuiz :: Card -> IO Card-}
{-maybeQuiz card = shouldQuizCard card >>= (\shouldQuiz -> if shouldQuiz then quizCard card else return card)-}

cardsToQuiz :: [Card] -> IO [Card]
cardsToQuiz cards = filterM shouldQuizCard cards

quizCards :: [Card] -> IO [Card]
quizCards []    = return []
quizCards cards = quizSomeCards cards cards

quizSomeCards :: [Card] -> [Card] -> IO [Card]
quizSomeCards cards allCards
    | null cards = return allCards
    | otherwise = do
    quizResult <- quizCard headCard
    case quizResult of
        Nothing -> return allCards
        Just updatedCard -> do
            needsRequizzing <- shouldQuizCard updatedCard
            if needsRequizzing
                then do
                    quizSomeCards (restOfCards ++ [updatedCard]) allCards
                else do
                    let newAllCards = (updatedCard:) . delete headCard $ allCards 
                    quizSomeCards (restOfCards) newAllCards
    where
    headCard = head cards
    restOfCards = tail cards

quizCard :: Card -> IO (Maybe Card)
quizCard card = do
    printf $ "Question : " ++ cardQuestion card ++ "\n"
    {-printf $ "Answer   : "-}
    {-hFlush stdout-}
    {-answer <- getLine-}
    answer <- Input.sameLinePrompt "Answer : "
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
    | isJust inferredConfidence = return $ inferredConfidence
    | otherwise = do
            printf $ "Correct answer : " ++ correctAnswer ++ "\n"
            printf $ "Rate your answer on a scale from 0-5, <Enter> to stop quizzing : "
            hFlush stdout
            promptConfidence
    where
    inferredConfidence = inferConfidence answer correctAnswer

inferConfidence :: String -> String -> Maybe Int
inferConfidence answer correctAnswer
    | rightAnswer =
        case answer \\ correctAnswer of
            {-"!" -> Just 5-}
            {-"." -> Just 4-}
            {-"?" -> Just 3-}
            _   -> Nothing
    | otherwise   = Nothing
        where rightAnswer = correctAnswer `isInfixOf` answer

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
        Nothing         -> return Nothing

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
getQuizAction decks = do
    cardsToBeQuizzed <- cardsToQuiz decks
    case cardsToBeQuizzed of
        [] -> do
            printf $ "No cards need to be quizzed at this time" ++ "\n" 
            return Nothing
        _  -> do
            printf $ "What would you like to be quizzed on?" ++ "\n"
            Input.getUserChoice allQuizActions
