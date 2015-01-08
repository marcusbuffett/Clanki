module Main where
import Decks
import Add
import Input
import Remove
import Quiz
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

runAction :: Maybe Action -> [Deck] -> IO [Deck]
runAction (Just Quit) decks   = return decks
runAction (Just Add) decks    = addLoop decks   >>= mainLoop
runAction (Just Quiz) decks   = quizLoop decks  >>= mainLoop
runAction (Just Remove) decks = removeLoop decks >>= mainLoop
runAction (Just Show) decks   = do
    printf $ show decks ++ "\n"
    mainLoop decks
runAction Nothing decks       = do
    printf $ "Invalid input" ++ "\n"
    mainLoop decks

{-showDecks decks = printf $ unlines $ map display decks-}

fileName :: String
fileName = "data.clanki"


mainLoop :: [Deck] -> IO [Deck]
mainLoop decks = do
    action <- getAction
    runAction action decks


loadData :: IO [Deck]
loadData = do
    fileExists <- doesFileExist fileName
    if fileExists
        then do
        x <- readFile fileName
        return (read x :: [Deck])
        else return []


saveData :: [Deck] -> IO ()
saveData decks = writeFile fileName (show decks)

startWithArgs :: [String] -> [Deck] -> IO [Deck]
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

