module Main where
import Tracker
import Card
import Deck
import Data.List(lookup)
data Action = Quiz | Add | Quit deriving (Show, Eq, Ord, Enum)
data AddAction = NewDeck | ToDeck Deck deriving (Show, Eq)
data ClaskData = ClaskData {decks :: [Deck], tracker :: Tracker} deriving (Show)

getAction :: IO Action
getAction = do
    input <- getLine
    let actionsWithShortcuts = zip allKeyShortcuts allActions 
    print actionsWithShortcuts
    let action = lookup input actionsWithShortcuts
    return Quiz
    
allActions :: [Action]
allActions = [Quiz ..]

allKeyShortcuts = map (\x -> [x]) ['a' .. 'z'] 

runAction :: Action -> IO ()
runAction action
    | action == Quiz = print "quiz"
    | action == Add  = print "add"
    | action == Quit = print "quit"
    | otherwise      = print "error"


getAddAction :: ClaskData -> IO AddAction
getAddAction claskData
    | null $ decks claskData = return NewDeck
    | otherwise = do
        chosenIndex <- readLn :: IO Int
        let chosenDeck = decks claskData !! chosenIndex
        return $ ToDeck chosenDeck


addLoop :: ClaskData -> IO ClaskData
addLoop claskData = do
    addAction <- getAddAction claskData
    case addAction of 
        NewDeck     -> return $ claskData
        ToDeck deck -> return $ claskData


quizLoop :: ClaskData -> IO ClaskData
quizLoop claskData = return claskData


mainLoop :: ClaskData -> IO String
mainLoop claskData = do
    action <- getAction
    case action of 
        Quit -> return $ show claskData
        Add  -> addLoop claskData   >>= mainLoop
        Quiz -> quizLoop claskData  >>= mainLoop
    {-return $ show claskData-}
    {-mainLoop claskData-}


getData :: IO ClaskData
getData = return $ ClaskData [] []


main :: IO ()
main = do
    claskData <- getData
    mainLoop claskData >>= print
    print claskData
