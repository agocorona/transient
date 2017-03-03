-----------------------------------------------------------------------------
--
-- Module      :  Transient.Indeterminism
-- Copyright   :
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | see <https://www.fpcomplete.com/user/agocorona/beautiful-parallel-non-determinism-transient-effects-iii>
--
-----------------------------------------------------------------------------
{-# LANGUAGE  ScopedTypeVariables #-}
module Transient.Indeterminism (
choose, choose', collect, collect', group, groupByTime
) where

import Transient.Internals hiding (retry)

import Data.IORef
import Control.Applicative
import Data.Monoid
import Control.Concurrent
import Data.Typeable
import Control.Monad.State
import GHC.Conc
import Data.Time.Clock
import Control.Exception


-- | slurp a list of values and process them in parallel . To limit the number of processing
-- threads, use `threads`
choose  :: Show a =>  [a] -> TransIO a
choose []= empty
choose   xs = do
    evs <- liftIO $ newIORef xs
    r <- parallel $ do
           es <- atomicModifyIORef' evs $ \es -> let tes= tail es in (tes,es)
           case es  of
            [x]  -> x `seq` return $ SLast x
            x:_  -> x `seq` return $ SMore x
    checkFinalize r

-- | alternative definition with more parallelism, as the composition of n `async` sentences
choose' :: [a] -> TransIO a
choose' xs = foldl (<|>) empty $ map (async . return) xs


-- | group the output of a possible multithreaded process in groups of n elements.
group :: Int -> TransIO a -> TransIO [a]
group num proc =  do
    v <- liftIO $ newIORef (0,[])
    x <- proc

    mn <- liftIO $ atomicModifyIORef' v $ \(n,xs) ->
            let n'=n +1
            in  if n'== num

              then ((0,[]), Just $ x:xs)
              else ((n', x:xs),Nothing)
    case mn of
      Nothing -> stop
      Just xs -> return xs

-- | group result for a time interval, measured with `diffUTCTime`
groupByTime :: Integer -> TransIO a -> TransIO [a]

groupByTime time proc =  do
    v  <- liftIO $ newIORef (0,[])
    t  <- liftIO getCurrentTime
    x  <- proc
    t' <- liftIO getCurrentTime
    mn <- liftIO $ atomicModifyIORef' v $ \(n,xs) -> let n'=n +1
            in
            if diffUTCTime t' t < fromIntegral time
             then ((n', x:xs),Nothing)
             else   ((0,[]), Just $ x:xs)
    case mn of
      Nothing -> stop
      Just xs -> return xs



-- collect the results of a search done in parallel, usually initiated by
-- `choose` .
--
-- execute a process and get at least the first n solutions (they could be more).
-- if he find the number of solutions requested, it kill the non-free threads of the process and return
collect ::  Int -> TransIO a -> TransIO [a]
collect n = collect' n 0

-- | search with a timeout
-- After the the timeout, it stop unconditionally and return the current results.
-- It also stops as soon as there are enough results specified in the first parameter.
collect' :: Int -> Int -> TransIO a -> TransIO [a]
collect' n t2 search= do

  rv <- liftIO $ newEmptyMVar     -- !> "NEWMVAR"


  results <- liftIO $ newIORef (0,[])

  let worker = do
        r <- search
        liftIO $  putMVar rv r
        stop

      timer= if t2 > 0 then async $ threadDelay t2 >> readIORef results >>= return . snd
                       else empty

      monitor=  async loop

          where
          loop = do
                     r <- takeMVar rv
                     (n',rs) <- readIORef results
                     let n''= n' +1
                     writeIORef results  (n'',r:rs)

                     t' <-  getCurrentTime
                     if (n > 0 && n'' >= n)
                       then readIORef results >>= return . snd else loop
                 `catch` \(e :: BlockedIndefinitelyOnMVar) ->
                                   readIORef results >>= return . snd


  r <- oneThread $ monitor <|> worker <|> timer

  return r


