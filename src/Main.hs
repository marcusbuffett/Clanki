module Main where
{-import Decks-}
import Add
import Input
import Remove
import Quiz
import Card
import Text.Printf(printf)
import Display
import Data.List(intercalate)
import System.Environment(getArgs)
import System.Directory(doesFileExist)

data Action = Quiz | Add | Remove | Show | Quit deriving (Show, Eq, Ord, Enum)

instance Display Action where
    display act = show act

allActions :: [Action]
allActions = [Quiz ..]

getAction :: IO (Maybe Action)
getAction = do
    Input.getUserChoice allActions

runAction :: Maybe Action -> [Card] -> IO [Card]
runAction (Just Quit) cards   = return cards
runAction (Just Add) cards    = addLoop cards   >>= mainLoop
runAction (Just Quiz) cards   = quizLoop cards  >>= mainLoop
runAction (Just Remove) cards = removeLoop cards >>= mainLoop
runAction (Just Show) cards   = do
    printf $ show cards ++ "\n"
    mainLoop cards
runAction Nothing cards       = do
    printf $ "Invalid input" ++ "\n"
    mainLoop cards

{-showDecks decks = printf $ unlines $ map display decks-}

fileName :: String
fileName = "data.clanki"


mainLoop :: [Card] -> IO [Card]
mainLoop decks = do
    action <- getAction
    runAction action decks


loadData :: IO [Card]
loadData = do
    fileExists <- doesFileExist fileName
    if fileExists
        then do
        x <- readFile fileName
        return (read x :: [Card])
        else return []


saveData :: [Card] -> IO ()
saveData decks = writeFile fileName (show decks)

startWithArgs :: [String] -> [Card] -> IO [Card]
startWithArgs args decks
    | null args = mainLoop decks
    | "--help" `elem` args || "-h" `elem` args = 
        do
            displayHelp
            return decks
    | "--list" `elem` args || "-l" `elem` args =
        do
            displayList decks
            return decks
    | otherwise = mainLoop decks

displayHelp :: IO ()
displayHelp = do
    printf $ "OPTIONS:" ++ "\n"
    printf $ "--list  -l              List available decks in default directory" ++ "\n"
    printf $ "--stats -s              Show some stats about the decks" ++ "\n"
    printf $ "--help -h               Show this help" ++ "\n"
            


main :: IO ()
main = do
    decks <- loadData
    args <- getArgs
    newDecks <- startWithArgs args decks
    saveData newDecks

