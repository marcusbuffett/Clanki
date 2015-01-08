module Input(getUserChoice) where
import Text.Printf(printf)
import Data.Maybe(isJust)
import System.IO(hSetBuffering, BufferMode(NoBuffering, LineBuffering), stdin, stdout)
import Display
import Control.Monad(when)

keys :: [String]
keys = map (:[]) ['a' .. 'z']

repKeysAndValues :: (Display a) => [(String, a)] -> String
repKeysAndValues choices = unlines [key ++ ")" ++ " " ++ display a | (key, a) <- choices]

getUserChoice :: (Display a) => [a] -> IO (Maybe a)
getUserChoice = getChoiceWithKeys . zip keys

getChoiceWithKeys :: (Display a) => [(String, a)] -> IO (Maybe a)
getChoiceWithKeys choices = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    printf $ repKeysAndValues choices
    input <- userInputLetter
    let action = lookup input choices
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    when (isJust action) $ printf "\n"

    return action

userInputLetter :: IO String
userInputLetter = do
    input <- fmap (:[]) getChar
    putStrLn ""
    return input

