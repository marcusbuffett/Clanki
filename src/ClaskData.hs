module ClaskData where
import Deck
import Tracker
import Card
data ClaskData = ClaskData {decks :: [Deck], tracker :: Tracker} deriving (Show)

containsDeckNamed :: String -> ClaskData -> Bool
containsDeckNamed name claskData
    | any (\d -> dName d == name) (decks claskData) = True
    | otherwise = False

addDeckToData :: Deck -> ClaskData -> ClaskData
addDeckToData deck claskData = claskData {decks = (decks claskData) ++ [deck]}

removeDeckNamed :: String -> [Deck] -> [Deck]
removeDeckNamed name decks = filter (\d -> dName d /= name) decks

replaceDeck :: Deck -> [Deck] -> [Deck]
replaceDeck deck decks = do
    [deck] ++ removeDeckNamed (dName deck) decks

updateClaskDataTracker :: ClaskData -> ClaskData
updateClaskDataTracker claskData = do
    let allCards = concat [dCards deck | deck <- decks claskData]
    claskData {tracker = updateTrackerWithCards allCards  (tracker claskData)}

updateClaskDataTrackerWithCards :: [Card] -> ClaskData -> ClaskData
updateClaskDataTrackerWithCards cards claskData = claskData {tracker = updatedTracker}
    where updatedTracker = updateTrackerWithCards cards  (tracker claskData)


