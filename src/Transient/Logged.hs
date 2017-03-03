-----------------------------------------------------------------------------
--
-- Module      :  Transient.Logged
-- Copyright   :
-- License     :  GPL-3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE  CPP,ExistentialQuantification, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}
module Transient.Logged(

#ifndef ghcjs_HOST_OS
restore,checkpoint,suspend,
#endif

logged, received,param, Loggable) where

import Data.Typeable
import Unsafe.Coerce
import Transient.Base
import Transient.Internals(Loggable)
import Transient.Indeterminism(choose)
import Transient.Internals(onNothing,reads1,IDynamic(..),Log(..),LogElem(..),RemoteStatus(..),StateIO)
import Control.Applicative
import Control.Monad.IO.Class
import System.Directory
import Control.Exception
import Control.Monad



#ifndef ghcjs_HOST_OS
import System.Random
#endif



#ifndef ghcjs_HOST_OS
logs= "logs/"

-- re-excutes all the threads whose state has been logged in the "./logs" folder
-- .Each log is removed when it is executed.
--
-- example: this program, if executed three times will first print hello <number> some times
-- but `suspend` will kill the threads and exit it.

-- The second time, it will print "world" <number> and "world22222" <number> and will stay.
--
-- The third time that it is executed, it only present "world22222" <number> messages
--
-- > main= keep $ restore  $ do
-- >    r <- logged $ choose [1..10 :: Int]
-- >    logged $ liftIO $ print ("hello",r)
-- >    suspend ()
-- >    logged $ liftIO $ print ("world",r)
-- >    checkpoint
-- >    logged $ liftIO $ print ("world22222",r)

restore :: TransIO a -> TransIO a
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



-- | save the state of  the thread that execute it and exit the transient block initiated with `keep` or similar
-- . `keep` will return the value passed by `suspend`.
-- If the process is executed again with `restore` it will reexecute the thread from this point on.
--
-- it is useful to insert it in `finish` blocks to gather error information,
suspend :: Typeable a => a -> TransIO a
suspend  x= do
   Log recovery _ log <- getData `onNothing` return (Log False [] [])
   if recovery then return x else do
        logAll  log
        exit x

-- | Save the state of every thread at this point. If the process is re-executed with `restore` it will reexecute the thread from this point on..
checkpoint ::  TransIO ()
checkpoint = do
   Log recovery _ log <- getData `onNothing` return (Log False [] [])
   if recovery then return () else logAll log


logAll log= do
        newlogfile <- liftIO $  (logs ++) <$> replicateM 7 (randomRIO ('a','z'))
        liftIO $ writeFile newlogfile $ show log
      :: TransIO ()

#endif


fromIDyn :: Loggable a => IDynamic -> a
fromIDyn (IDynamic x)=r where r= unsafeCoerce x     -- !> "coerce" ++ " to type "++ show (typeOf r)

fromIDyn (IDyns s)=r `seq`r where r= read s         -- !> "read " ++ s ++ " to type "++ show (typeOf r)



toIDyn x= IDynamic x



-- | write the result of the computation in  the log and return it.
-- but if there is data in the internal log, it read the data from the log and
-- do not execute the computation.
--
-- It accept nested step's. The effect is that if the outer step is executed completely
-- the log of the inner steps are erased. If it is not the case, the inner steps are logged
-- this reduce the log of large computations to the minimum. That is a feature not present
-- in the package Workflow.
--
-- >  r <- logged $ do
-- >          logged this :: TransIO ()
-- >          logged that :: TransIO ()
-- >          logged thatOther
-- >  liftIO $ print r
--
--  when `print` is executed, the log is just the value of r.
--
--  but at the `thatOther` execution the log is: [Exec,(), ()]
--
logged :: Loggable a => TransientIO a -> TransientIO a
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
