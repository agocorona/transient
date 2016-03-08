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
{-# LANGUAGE  ExistentialQuantification, FlexibleInstances, UndecidableInstances #-}
module Transient.Logged  where

import Data.Typeable
import Unsafe.Coerce
import Transient.Base
import Control.Applicative
import Control.Monad.IO.Class



class (Show a, Read a,Typeable a) => Loggable a
instance (Show a, Read a,Typeable a) => Loggable a

fromIDyn :: (Read a, Show a, Typeable a) => IDynamic -> a
fromIDyn (IDynamic x)= unsafeCoerce x

fromIDyn (IDyns s)=r where r= read s  -- !!> "read " ++ s ++ "to type "++ show (typeOf r)

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
logged :: (Show a, Read a, Typeable a) => TransientIO a -> TransientIO a
logged mx =  Transient $ do
   Log recover rs full <- getData `onNothing` return ( Log False  [][])
   runTrans $
    case (recover,rs) of
      (True, Step x: rs') -> do
            setSData $ Log True rs' full
            (return $ fromIDyn x)              -- !!>  "read in step:" ++ show x

      (True, Exec:rs') -> do
            setSData $ Log True  rs' full
            mx                                 -- !!> "step True Exec"

      (True, Wait:rs') -> do
            setSData (Log True  rs' full)      -- !!> "Wait2"
            empty

--      (True, Wormhole:rs') -> do
--            setSData (Log True  rs' full)      -- !!> "Wait2"
--            mx


      _ -> do
            let add= Exec: full
            setSData $ Log False add add

            r <-  mx <*** ( do  -- para evitar que   p1 <|> p2   ejecute p1 cuando p1 espera input  ejecutando p2
                            r <- getSData <|> return NoRemote
                            case r of WasParallel ->
                                         let add= Wait: full
                                         in setSData $ Log False add add
                                      _ -> return ())


            let add= Step (toIDyn r): full
            (setSData $ Log False add add)     -- !!> "AFTER STEP"
            return  r







--step :: (Show a, Read a, Typeable a) => TransientIO a -> TransientIO a
--step mx = step' mx $ \full mx -> do
--            let add= Exec: full
--            setSData $ Log False add add
--
--            r <-  mx
--
--            let add= Step (toIDyn r): full
--            (setSData $ Log False add add)     -- !!> "AFTER STEP"
--            return  r
--
--


