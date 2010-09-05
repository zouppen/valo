module Timing where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent (threadDelay)
import Control.Monad (when)

frameTiming :: Integer -> IO [Integer]
frameTiming fps = do
  now <- getMicroTime
  --let now = 0
  return $ map (stamp now) [1..]
    where interval = 1000000 `div` fps
          stamp now i = now + i * interval

-- |Gets current time in microseconds.
getMicroTime :: IO Integer
getMicroTime = do
  t <- getPOSIXTime
  return $ floor (t*1e6)

-- |Maps a given list of actions at given microsecond moment.
sequenceTime :: [(Integer, IO a)] -> IO ()
sequenceTime [] = return ()
sequenceTime ((targetTime,action):xs) = do
  curTime <- getMicroTime
  let sleepTime = fromInteger (targetTime - curTime)
  
  when (sleepTime > 0) (threadDelay sleepTime)
  action

  sequenceTime xs

-- |Runs the list of actions at given fps.
-- TODO implement frameskip if needed.
sequenceFps fps actions = do
  timings <- frameTiming fps
  sequenceTime $ zip timings actions

-- Random helpers...
apu i = putStrLn ("on " ++ show i)
apuja = map apu [1..]
