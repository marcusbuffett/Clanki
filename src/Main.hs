module Main where
import Tracker
import Card
import Deck
import Add
import ClaskData
import Input
import Remove
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

runAction :: Maybe Action -> ClaskData -> IO ClaskData
runAction action claskData= case action of 
    Just Quit   -> return claskData
    Just Add    -> addLoop claskData   >>= mainLoop
    Just Quiz   -> quizLoop claskData  >>= mainLoop
    Just Remove -> removeLoop claskData >>= mainLoop
    Nothing   -> do
        print "Invalid input"
        mainLoop claskData

quizLoop :: ClaskData -> IO ClaskData
quizLoop claskData = return claskData


mainLoop :: ClaskData -> IO ClaskData
mainLoop claskData = do
    action <- getAction
    print claskData
    runAction action claskData


loadData :: IO ClaskData
loadData = do
    fileExists <- doesFileExist "thing.data"
    if fileExists
        then do
        x <- readFile "thing.data" 
        print x
        printf $ show (read x :: ClaskData)
        return (read x :: ClaskData)
        else return $ ClaskData [] []


saveData :: ClaskData -> IO ()
saveData claskData = writeFile "thing.data" (show claskData)


main :: IO ()
main = do
    {-getCurrentDirectory >= print-}
    claskData <- loadData
    mainLoop claskData >>= saveData
