module Main where
import Tracker
import Card
import Deck
import Add
import ClaskData
import Input
import Data.List(lookup)
import Text.Printf(printf)
data Action = Quiz | Add | Quit deriving (Show, Eq, Ord, Enum)

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

runAction :: Maybe Action -> ClaskData -> IO String
runAction action claskData= case action of 
    Just Quit -> return $ "QUIT"
    Just Add  -> addLoop claskData   >>= mainLoop
    Just Quiz -> quizLoop claskData  >>= mainLoop
    Nothing   -> do
        print "Invalid input"
        mainLoop claskData


quizLoop :: ClaskData -> IO ClaskData
quizLoop claskData = return claskData


mainLoop :: ClaskData -> IO String
mainLoop claskData = do
    action <- getAction
    print claskData
    runAction action claskData


getData :: IO ClaskData
getData = return $ ClaskData [] []


main :: IO ()
main = do
    claskData <- getData
    mainLoop claskData >>= print
