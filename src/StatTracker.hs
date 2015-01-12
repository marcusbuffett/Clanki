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
updateResponseCounters counters confidence = [newCount | i <- [0..5], let newCount = counters !! i + (if confidence == i then 1 else 0)]
