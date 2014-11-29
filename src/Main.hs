module Main where
import Tracker
import Card
import Decks
import Add
import Input
import Remove
import Quiz
import Data.List(lookup)
import Text.Printf(printf)
import System.Directory(getCurrentDirectory, doesFileExist)

import Display
data Action = Quiz | Add | Remove | Quit deriving (Show, Eq, Ord, Enum)

instance Display Action where
    display act = show act

allActions :: [Action]
allActions = [Quiz ..]

getAction :: IO (Maybe Action)
getAction = do
    Input.getUserChoice allActions

userInputLetter :: IO String
userInputLetter = do
    input <- fmap (:[]) getChar
    putStrLn ""
    return input

runAction :: Maybe Action -> [Deck] -> IO [Deck]
runAction action decks= case action of 
    Just Quit   -> return decks
    Just Add    -> addLoop decks   >>= mainLoop
    Just Quiz   -> quizLoop decks  >>= mainLoop
    Just Remove -> removeLoop decks >>= mainLoop
    Nothing   -> do
        print "Invalid input"
        mainLoop decks


mainLoop :: [Deck] -> IO [Deck]
mainLoop decks = do
    action <- getAction
    runAction action decks


loadData :: IO [Deck]
loadData = do
    fileExists <- doesFileExist "thing.data"
    if fileExists
        then do
        x <- readFile "thing.data" 
        return (read x :: [Deck])
        else return $ []


saveData :: [Deck] -> IO ()
saveData decks = writeFile "thing.data" (show decks)


main :: IO ()
main = do
    decks <- loadData
    newDecks <- mainLoop decks
    saveData newDecks
    return ()

