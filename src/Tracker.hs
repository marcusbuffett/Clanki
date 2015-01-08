module Tracker where
import Card
import Data.Time.Clock.POSIX
import Data.Maybe(fromJust, isNothing)

sm2 :: Int -> Float -> Float
sm2 0 _  = 1
sm2 1 _  = 1
sm2 2 _  = 6
sm2 n ef = sm2 (n-1) ef * ef

shouldQuizCard :: Card -> IO Bool
shouldQuizCard card = do
    currentTime <- fmap round getPOSIXTime :: IO Integer
    let timeDifference = currentTime - fromIntegral (ctTimeQuizzed (cardTracker card))
        daysSinceQuiz = floor $ (fromIntegral timeDifference :: Float) / 86400
        sm2Time = (ceiling $ sm2 (ctN tracker) (ctEF tracker)) :: Integer
        tracker = cardTracker card
        lastResponse = ctLastResponseQuality tracker
        noLastResponse = isNothing lastResponse
        justLastResponse = fromJust lastResponse
    return (daysSinceQuiz >= sm2Time ||  noLastResponse || justLastResponse < 4)

adjustEF :: Float -> Int -> Float
adjustEF ef confidence = ef-0.8+0.28*q-0.02*q*q
    where q = fromIntegral confidence :: Float
