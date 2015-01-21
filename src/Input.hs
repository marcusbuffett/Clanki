module Input(getUserChoice, getUserChoiceStr) where
import Text.Printf(printf)
import Data.Maybe(isJust)
import System.IO(hSetBuffering, BufferMode(NoBuffering, LineBuffering), stdin, stdout)
import Display
import Control.Monad(when)
import Data.Char(toUpper)

keys :: [String]
keys = map (:[]) ['A' .. 'Z']

getUserChoice :: (Display a) => [a] -> IO (Maybe a)
getUserChoice = getChoiceWithKeys . zip keys

getUserChoiceStr :: [String] -> IO (Maybe String)
getUserChoiceStr = getChoiceWithKeysStr . zip keys

getChoiceWithKeysStr :: [(String, String)] -> IO (Maybe String)
getChoiceWithKeysStr choices = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    printf $ repKeysAndValues choices
    input <- userInputLetter
    let action = lookup input choices
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    when (isJust action) $ printf "\n"
    return action

choicesToStringPairs :: (Display a) => [(String, a)] -> [(String, String)]
choicesToStringPairs choicePairs = [(fst pair, display $ snd pair) | pair <- choicePairs]

repKeysAndValues :: [(String, String)] -> String
repKeysAndValues choices = unlines [key ++ ")" ++ " " ++ a | (key, a) <- choices]


getChoiceWithKeys :: (Display a) => [(String, a)] -> IO (Maybe a)
getChoiceWithKeys choices = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    printf $ repKeysAndValues $ choicesToStringPairs choices
    input <- userInputLetter
    let action = lookup input choices
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    when (isJust action) $ printf "\n"

    return action

userInputLetter :: IO String
userInputLetter = do
    input <- fmap ((:[]) . toUpper) getChar
    putStrLn ""
    return input

