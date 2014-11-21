module Tracker where
import Card
import Data.Time.Clock.POSIX
type Tracker = [CardTracker]
data CardTracker = CardTracker {ctCard :: Card, ctEF :: Float, ctN :: Int, ctTimeQuizzed :: Integer} deriving (Show, Read)

sm2 :: Int -> Float -> Int
sm2 n ef
    | n == 1 = 1
    | n == 2 = 6
    | otherwise = sm2 (n-1) ef

trackCard :: Card -> [CardTracker] -> [CardTracker]
trackCard card tracker = tracker ++ [CardTracker {ctCard = card, ctEF = 2.5, ctN = 1, ctTimeQuizzed = 0}]

untrackCard :: Card -> [CardTracker] -> [CardTracker]
untrackCard card tracker = filter (\cTracker -> ctCard cTracker /= card) tracker

newCardTracker :: Card -> CardTracker
newCardTracker card = CardTracker {ctCard = card, ctEF = 2.5, ctN = 1, ctTimeQuizzed = 0}

cardTracked :: Card -> Tracker -> Bool
cardTracked card tracker = card `elem` [ctCard cTracker | cTracker <- tracker]

updateTrackerWithCards :: [Card] -> Tracker -> Tracker
updateTrackerWithCards cards tracker = do
    
    let cardsUntracked = filter (\card -> not $ trackerContainsCard card tracker) cards
    let untrackedCardTrackers = [CardTracker card 2.5 1 0| card <- cardsUntracked]
    tracker ++ untrackedCardTrackers

trackerContainsCard :: Card -> Tracker -> Bool
trackerContainsCard card tracker = any (\cTracker -> ctCard cTracker == card) tracker


trimUnusedCards :: [Card] -> Tracker -> IO Tracker
trimUnusedCards allCards tracker = return [cTracker | cTracker <- tracker, ctCard cTracker `elem` allCards]

