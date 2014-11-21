module Card where 
import Display

data Card = Card {cardQuestion :: String, cardAnswer :: String} deriving (Show, Read, Eq)

instance Display Card where
    display card = cardQuestion card ++ "\n" ++ cardAnswer card

printCard :: Card -> IO ()
printCard card = print $ cardQuestion card ++ "\n" ++ cardAnswer card

