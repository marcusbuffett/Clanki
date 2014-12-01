module Main where
import Decks
import Add
import Input
import Remove
import Quiz
import Text.Printf(printf)
import System.Directory(doesFileExist)

import Display
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


main :: IO ()
main = do
    decks <- loadData
    newDecks <- mainLoop decks
    saveData newDecks
    return ()

