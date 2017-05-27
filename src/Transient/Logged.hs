-----------------------------------------------------------------------------
--
-- Module      :  Transient.Logged
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | The 'logged' primitive is used to save the results of the subcomputations
-- of a transient computation (including all its threads) in a log buffer. At
-- any point, a 'suspend' or 'checkpoint' can be used to save the accumulated
-- log on a persistent storage. A 'restore' reads the saved logs and resumes
-- the computation from the saved checkpoint. On resumption, the saved results
-- are used for the computations which have already been performed. The log
-- contains purely application level state, and is therefore independent of the
-- underlying machine architecture. The saved logs can be sent across the wire
-- to another machine and the computation can then be resumed on that machine.
-- We can also save the log to gather diagnostic information, especially in
-- 'finish' blocks.
--
-- The following example illustrates the APIs. In its first run 'suspend' saves
-- the state in a directory named @logs@ and exits, in the second run it
-- resumes from that point and then stops at the 'checkpoint', in the third run
-- it resumes from the checkpoint and then finishes.
--
-- @
-- main= keep $ restore  $ do
--      r <- logged $ choose [1..10 :: Int]
--      logged $ liftIO $ print (\"A",r)
--      suspend ()
--      logged $ liftIO $ print (\"B",r)
--      checkpoint
--      liftIO $ print (\"C",r)
-- @
-----------------------------------------------------------------------------
{-# LANGUAGE  CPP,ExistentialQuantification, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}

module Transient.Logged(
Loggable, logged, received, param

#ifndef ghcjs_HOST_OS
, suspend, checkpoint, restore
#endif
) where

import Data.Typeable
import Unsafe.Coerce
import Transient.Base
import Transient.Internals(Loggable, pattern Transient)
import Transient.Indeterminism(choose)
import Transient.Internals(AsyncT(..), onNothing,reads1,IDynamic(..),Log(..),LogElem(..),RemoteStatus(..))
import Control.Applicative
import Control.Monad.IO.Class
import System.Directory
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Control (MonadBaseControl)



#ifndef ghcjs_HOST_OS
import System.Random
#endif



#ifndef ghcjs_HOST_OS
logs= "logs/"

-- | Reads the saved logs from the @logs@ subdirectory of the current
-- directory, restores the state of the computation from the logs, and runs the
-- computation.  The log files are removed after the state has been restored.
--
restore :: (MonadIO m, MonadBaseControl IO m) => AsyncT m a -> AsyncT m a
restore   proc= do
     liftIO $ createDirectory logs  `catch` (\(e :: SomeException) -> return ())
     list <- liftIO $ getDirectoryContents logs
                 `catch` (\(e::SomeException) -> return [])
     if length list== 2 then proc else do

         let list'= filter ((/=) '.' . head) list
         file <- choose  list'       -- !> list'

         logstr <- liftIO $ readFile (logs++file)
         let log= length logstr `seq` read' logstr

         log `seq` setData (Log True (reverse log) log)
         liftIO $ remove $ logs ++ file
         proc
     where
     read'= fst . head . reads1

     remove f=  removeFile f `catch` (\(e::SomeException) -> remove f)



-- | Saves the logged state of the current computation that has been
-- accumulated using 'logged', and then 'exit's using the passed parameter as
-- the exit code. Note that all the computations before a 'suspend' must be
-- 'logged' to have a consistent log state. The logs are saved in the @logs@
-- subdirectory of the current directory. Each thread's log is saved in a
-- separate file.
--
suspend :: (MonadIO m, MonadBaseControl IO m, Typeable a) => a -> AsyncT m a
suspend  x= do
   Log recovery _ log <- getData `onNothing` return (Log False [] [])
   if recovery then return x else do
        logAll  log
        exit x

-- | Saves the accumulated logs of the current computation, like 'suspend', but
-- does not exit.
checkpoint :: (MonadIO m, MonadBaseControl IO m) => AsyncT m ()
checkpoint = do
   Log recovery _ log <- getData `onNothing` return (Log False [] [])
   if recovery then return () else logAll log


logAll :: (MonadIO m, MonadBaseControl IO m) => [LogElem] -> AsyncT m ()
logAll log= do
        newlogfile <- liftIO $  (logs ++) <$> replicateM 7 (randomRIO ('a','z'))
        liftIO $ writeFile newlogfile $ show log

#endif


fromIDyn :: Loggable a => IDynamic -> a
fromIDyn (IDynamic x)=r where r= unsafeCoerce x     -- !> "coerce" ++ " to type "++ show (typeOf r)

fromIDyn (IDyns s)=r `seq`r where r= read s         -- !> "read " ++ s ++ " to type "++ show (typeOf r)



toIDyn x= IDynamic x



-- | Run the computation, write its result in a log in the parent computation
-- and return the result. If the log already contains the result of this
-- computation ('restore'd from previous saved state) then that result is used
-- instead of running the computation again.
--
-- 'logged' can be used for computations inside a 'logged' computation. Once
-- the parent computation is finished its internal (subcomputation) logs are
-- discarded.
--
logged :: (MonadIO m, Loggable a) => AsyncT m a -> AsyncT m a
logged mx =  Transient $ do
   Log recover rs full <- getData `onNothing` return ( Log False  [][])
   runTrans $
    case (recover ,rs)   of                               --    !> ("logged enter",recover,rs) of
      (True, Var x: rs') -> do
            setData $ Log True rs' full
            return $ fromIDyn x
--                                                  !> ("Var:", x)

      (True, Exec:rs') -> do
            setData $ Log True  rs' full
            mx
--                                                  !> "Exec"

      (True, Wait:rs') -> do
            setData (Log True  rs' full)          -- !> "Wait"
            empty

      _ -> do
--            let add= Exec: full
            setData $ Log False (Exec : rs) (Exec: full)     -- !> ("setLog False", Exec:rs)

            r <-  mx <** ( do  -- when   p1 <|> p2, to avoid the re-execution of p1 at the
                                -- recovery when p1 is asynchronous
                            r <- getSData <|> return NoRemote
                            case r of
                                      WasParallel ->
--                                         let add= Wait: full
                                           setData $ Log False (Wait: rs) (Wait: full)
                                      _ -> return ())

            Log recoverAfter lognew _ <- getData `onNothing` return ( Log False  [][])
            let add= Var (toIDyn r):  full
            if recoverAfter && (not $ null lognew)      -- !> ("recoverAfter", recoverAfter)
              then  (setData $ Log True lognew (reverse lognew ++ add) )
                                                        -- !> ("recover",reverse lognew ,add)
              else if recoverAfter && (null lognew) then
                   setData $ Log False [] add
              else
                  (setData $ Log False (Var (toIDyn r):rs) add)  -- !> ("restore", (Var (toIDyn r):rs))
            return  r




-------- parsing the log for API's

received :: Loggable a => a -> TransIO ()
received n=Transient $ do
   Log recover rs full <- getData `onNothing` return ( Log False  [][])
   case rs of
     [] -> return Nothing
     Var (IDyns s):t -> if s == show1 n
          then  do
            setData $ Log recover t full
            return $ Just ()
          else return Nothing
     _  -> return Nothing
   where
   show1 x= if typeOf x == typeOf "" then unsafeCoerce x else show x

param :: Loggable a => TransIO a
param= res where
 res= Transient $ do
   Log recover rs full <- getData `onNothing` return ( Log False  [][])
   case rs of
     [] -> return Nothing
     Var (IDynamic v):t ->do
           setData $ Log recover t full
           return $ cast v
     Var (IDyns s):t -> do
       let mr = reads1  s `asTypeOf` type1 res

       case mr of
          [] -> return Nothing
          (v,r):_ -> do
              setData $ Log recover t full
              return $ Just v
     _ -> return Nothing

   where
   type1 :: TransIO a -> [(a,String)]
   type1= error "type1: typelevel"
