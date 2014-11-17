module Card where 

data Card = Card {cardQuestion :: String, cardAnswer :: String} deriving (Show, Read, Eq)

printCard :: Card -> IO ()
printCard card = print $ cardQuestion card ++ "\n" ++ cardAnswer card

