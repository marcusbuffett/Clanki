module Tracker where
import Card
import Data.Time.Clock.POSIX
import Data.Maybe(fromJust, fromMaybe, isNothing)

sm2 :: Int -> Float -> Float
sm2 n ef
    | n == 0 = 1
    | n == 1 = 1
    | n == 2 = 6
    | otherwise = sm2 (n-1) ef

shouldQuizCard :: Card -> IO Bool
shouldQuizCard card = do
    currentTime <- return 1000 --fmap round getPOSIXTime
    let timeDifference = currentTime - (ctTimeQuizzed $ cardTracker card)
        daysSinceQuiz = floor $ (fromIntegral $ timeDifference :: Float) / 3600 
        sm2Time = (ceiling $ sm2 (ctN tracker) (ctEF tracker)) :: Integer
        tracker = cardTracker card
        lastResponse = ctLastResponseQuality tracker
        noLastResponse = isNothing lastResponse
        justLastResponse = fromJust lastResponse
    return (daysSinceQuiz >= sm2Time ||  noLastResponse || justLastResponse < 4)

adjustEF :: Float -> Integer -> Float
adjustEF ef confidence = ef-0.8+0.28*q-0.02*q*q
    where q = fromIntegral confidence :: Float
