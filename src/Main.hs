module Main where
import Decks
import Add
import Input
import Remove
import Quiz
import Text.Printf(printf)
import System.Directory(doesFileExist)

import Display
data Action = Quiz | Add | Remove | Quit {--| Show --}  deriving (Show, Eq, Ord, Enum)

instance Display Action where
    display act = show act

allActions :: [Action]
allActions = [Quiz ..]

getAction :: IO (Maybe Action)
getAction = do
    Input.getUserChoice allActions

runAction :: Maybe Action -> [Deck] -> IO [Deck]
runAction action decks= case action of 
    Just Quit   -> return decks
    Just Add    -> addLoop decks   >>= mainLoop
    Just Quiz   -> quizLoop decks  >>= mainLoop
    Just Remove -> removeLoop decks >>= mainLoop
    {-Just Show   -> do-}
        {-showDecks decks -}
        {-mainLoop decks-}
    Nothing     -> do
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
        else return $ []


saveData :: [Deck] -> IO ()
saveData decks = writeFile fileName (show decks)


main :: IO ()
main = do
    decks <- loadData
    newDecks <- mainLoop decks
    saveData newDecks
    return ()

