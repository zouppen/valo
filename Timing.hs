module Timing where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent (threadDelay)
import Control.Monad (when)

-- |Computes suitable frame times for actions. Uses wall clock and
-- |given frames-per-second value to compute timings for all following
-- |"frames".
frameTiming :: (RealFrac a) => a -> IO [Integer]
frameTiming fps = do
  now <- getMicroTime
  return $ map (stamp now) [1..]
    where interval = round $ 1e6 / fps
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
