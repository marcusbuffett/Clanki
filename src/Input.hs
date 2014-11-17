module Input(getUserChoice) where
import Text.Printf(printf)
import System.IO(hSetBuffering, BufferMode(NoBuffering, LineBuffering), stdin, stdout)

keys :: [String]
keys = map (:[]) ['a' .. 'z']

repKeysAndValues :: (Show a) => [(String, a)] -> String
repKeysAndValues choices = unlines [key ++ ")" ++ " " ++ (show a) | (key, a) <- choices]

getUserChoice :: (Show a) => [a] -> IO (Maybe a)
getUserChoice choices = do
    getChoiceWithKeys $ zip keys choices

getChoiceWithKeys :: (Show a) => [(String, a)] -> IO (Maybe a)
getChoiceWithKeys choices = do
    print "Please select an option :"
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    printf $ repKeysAndValues choices
    input <- userInputLetter
    let action = lookup input (choices)
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    return action

userInputLetter :: IO String
userInputLetter = do
    input <- fmap (:[]) getChar
    putStrLn ""
    return input


