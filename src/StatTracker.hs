module StatTracker where

data StatTracker = StatTracker {stTimesQuizzed :: Int, stResponseCounters :: [Int]} deriving (Show, Eq, Read)

newStatTracker :: StatTracker
newStatTracker = StatTracker {stTimesQuizzed = 0, stResponseCounters = replicate 0 6}

updateStatTracker :: StatTracker -> Int -> StatTracker
updateStatTracker st confidence = st {stTimesQuizzed = oldTimesQuizzed + 1, stResponseCounters = updateResponseCounters oldCounters confidence}
    where
    oldTimesQuizzed = stTimesQuizzed st + 1
    oldCounters     = stResponseCounters st

updateResponseCounters :: [Int] -> Int -> [Int]
--TODO: Make this actually do something
updateResponseCounters counters confidence = counters
