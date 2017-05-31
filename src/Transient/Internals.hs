-----------------------------------------------------------------------------
--
-- Module      :  Base
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | See http://github.com/agocorona/transient
-- Everything in this module is exported in order to allow extensibility.
-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE PatternSynonyms           #-}

module Transient.Internals where

import           Control.Applicative
import           Control.Monad.State
import           Data.Dynamic
import qualified Data.Map               as M
import           System.IO.Unsafe
import           Unsafe.Coerce
import           Control.Exception hiding (try,onException)
import qualified Control.Exception.Lifted as E
import           Control.Monad.Base (MonadBase(..), liftBaseDefault)
import           Control.Monad.Trans.Class (MonadTrans(lift))
import           Control.Monad.Trans.Control ( ComposeSt, MonadBaseControl(..)
                                             , MonadTransControl (..), Run
                                             , control
                                             , defaultLiftBaseWith
                                             , defaultRestoreM
                                             , liftBaseOp_)
import           Control.Concurrent
import           GHC.Conc(unsafeIOToSTM)
import           Control.Concurrent.STM hiding (retry)
import qualified Control.Concurrent.STM  as STM (retry)
import           System.Mem.StableName
import           Data.Maybe

import           Data.List
import           Data.IORef
import           System.Environment
import           System.IO

import qualified Data.ByteString.Char8 as BS

#ifdef DEBUG

import           Data.Monoid
import           Debug.Trace
import           System.Exit

{-# INLINE (!>) #-}
(!>) :: Show a => b -> a -> b
(!>) x y = trace (show y) x
infixr 0 !>

#else

{-# INLINE (!>) #-}
(!>) :: a -> b -> a
(!>) = const

#endif

type StateM m = StateT EventF m

newtype AsyncT m a = AsyncT { runTrans :: StateM m (Maybe a) }

type SData = ()

type EventId = Int

pattern Transient x = AsyncT x
type TransIO = AsyncT IO
type TransientIO = TransIO

data LifeCycle = Alive | Parent | Listener | Dead
  deriving (Eq, Show)

-- | EventF describes the context of a TransientIO computation:
data EventF = forall m a b. EventF
  { meffects     :: ()

  , event       :: Maybe SData
    -- ^ Not yet consumed result (event) from the last asynchronous run of the
    -- computation

  , xcomp       :: AsyncT m a
  , fcomp       :: [b -> AsyncT m b]
    -- ^ List of continuations

  , mfData      :: M.Map TypeRep SData
    -- ^ State data accessed with get or put operations

  , mfSequence  :: Int
  , threadId    :: ThreadId
  , freeTh      :: Bool
    -- ^ When 'True', threads are not killed using kill primitives

  , parent      :: Maybe EventF
    -- ^ The parent of this thread

  , children    :: MVar [EventF]
    -- ^ Forked child threads, used only when 'freeTh' is 'False'

  , maxThread   :: Maybe (IORef Int)
    -- ^ Maximum number of threads that are allowed to be created

  , labelth     :: IORef (LifeCycle, BS.ByteString)
    -- ^ Label the thread with its lifecycle state and a label string
  } deriving Typeable

-- type Effects =
--   forall a b c. TransIO a -> TransIO a -> (a -> TransIO b)
--     -> StateIO (StateIO (Maybe c) -> StateIO (Maybe c), Maybe a)

instance (Monad m, Monad (AsyncT m)) => MonadState EventF (AsyncT m) where
  get     = Transient $ get   >>= return . Just
  put x   = Transient $ put x >>  return (Just ())
  state f =  Transient $ do
    s <- get
    let ~(a, s') = f s
    put s'
    return $ Just a

-- | Run a "non transient" computation within the underlying state monad, so it is
-- guaranteed that the computation neither can stop neither can trigger additional
-- events/threads.
noTrans :: Monad m => StateM m x -> AsyncT m x
noTrans x = Transient $ x >>= return . Just

emptyEventF :: AsyncT m a -> ThreadId -> IORef (LifeCycle, BS.ByteString) -> MVar [EventF] -> EventF
emptyEventF x th label childs =
  EventF { meffects   = mempty
         , event      = mempty
         , xcomp      = x
         , fcomp      = []
         , mfData     = mempty
         , mfSequence = 0
         , threadId   = th
         , freeTh     = False
         , parent     = Nothing
         , children   = childs
         , maxThread  = Nothing
         , labelth    = label }

-- | Run a transient computation with a default initial state
runTransient :: forall m a. MonadIO m => AsyncT m a -> m (Maybe a, EventF)
runTransient t = do
  th     <- liftIO $ myThreadId
  label  <- liftIO $ newIORef $ (Alive, BS.pack "top")
  childs <- liftIO $ newMVar []
  runStateT (runTrans t) $ emptyEventF (empty :: AsyncT m a) th label childs

-- | Run a transient computation with a given initial state
runTransState :: EventF -> AsyncT m x -> m (Maybe x, EventF)
runTransState st x = runStateT (runTrans x) st

-- | Get the continuation context: closure, continuation, state, child threads etc
getCont :: Monad m => AsyncT m EventF
getCont = Transient $ Just <$> get

-- | Run the closure and the continuation using the state data of the calling thread
runCont :: (MonadIO m, Monad (AsyncT m)) => EventF -> StateM m (Maybe a)
runCont EventF { xcomp = x, fcomp = fs } = runTrans $ do
  r <- unsafeCoerce x
  compose (unsafeCoerce fs) r

-- | Run the closure and the continuation using its own state data.
runCont' :: (MonadIO m, Monad (AsyncT m)) => EventF -> m (Maybe a, EventF)
runCont' cont = runStateT (runCont cont) cont

-- | Warning: Radically untyped stuff. handle with care
getContinuations :: Monad m => StateM m [a -> AsyncT m b]
getContinuations = do
  EventF { fcomp = fs } <- get
  return $ unsafeCoerce fs

{-
runCont cont = do
  mr <- runClosure cont
  case mr of
    Nothing -> return Nothing
    Just r -> runContinuation cont r
-}

-- | Compose a list of continuations.
compose :: MonadIO m => [a -> AsyncT m a] -> (a -> AsyncT m b)
compose []     = const empty
compose (f:fs) = \x -> f x >>= compose fs

-- | Run the closure  (the 'x' in 'x >>= f') of the current bind operation.
runClosure :: EventF -> StateM m (Maybe a)
runClosure EventF { xcomp = x } = unsafeCoerce (runTrans x)

-- | Run the continuation (the 'f' in 'x >>= f') of the current bind operation with the current state.
runContinuation :: MonadIO m => EventF -> a -> StateM m (Maybe b)
runContinuation EventF { fcomp = fs } =
  runTrans . (compose $  (unsafeCoerce fs))

-- | Save a closure and a continuation ('x' and 'f' in 'x >>= f').
setContinuation :: Monad m
    => AsyncT m a -> (a -> AsyncT m b) -> [c -> AsyncT m c] -> StateM m ()
setContinuation  b c fs = do
  modify $ \EventF{..} -> EventF { xcomp = b
                                  , fcomp = unsafeCoerce c : fs
                                  , .. }

-- | Save a closure and continuation, run the closure, restore the old continuation.
-- | NOTE: The old closure is discarded.
withContinuation :: MonadIO m => b -> AsyncT m a -> AsyncT m a
withContinuation c mx = do
  EventF { fcomp = fs, .. } <- get
  put $ EventF { xcomp = unsafeCoerce mx
               , fcomp = unsafeCoerce c : fs
               , .. }
  r <- mx
  restoreStack fs
  return r

-- | Restore the continuations to the provided ones.
-- | NOTE: Events are also cleared out.
-- restoreStack :: MonadState EventF m => [a -> AsyncT m a] -> m ()
restoreStack fs = modify $ \EventF {..} ->
    EventF { event = Nothing, fcomp = (unsafeCoerce fs), .. }

-- | Run a chain of continuations.
-- WARNING: It is up to the programmer to assure that each continuation typechecks
-- with the next, and that the parameter type match the input of the first
-- continuation.
-- NOTE: Normally this makes sense to stop the current flow with `stop` after the
-- invocation.
runContinuations :: MonadIO m => [a -> AsyncT m b] -> c -> AsyncT m d
runContinuations fs x = compose (unsafeCoerce fs)  x

-- Instances for Transient Monad

instance Monad (AsyncT m) => Functor (AsyncT m) where
  fmap f mx = do
    x <- mx
    return $ f x

instance MonadIO m => Applicative (AsyncT m) where
  pure a  = Transient . return $ Just a

  f <*> g = Transient $ do
         rf <- liftIO $ newIORef (Nothing,[])
         rg <- liftIO $ newIORef (Nothing,[])
#ifdef DEBUG
                 !> "NEWIOREF"
#endif

         fs <- getContinuations

         let hasWait (_:Wait:_) = True
             hasWait _          = False

             appf k = Transient $  do
                   Log rec _ full <- getData `onNothing` return (Log False [] [])
                   (liftIO $ writeIORef rf  (Just k,full))
#ifdef DEBUG
                               !> ( show $ unsafePerformIO myThreadId) ++"APPF"
#endif
                   (x, full2)<- liftIO $ readIORef rg
                   when (hasWait  full ) $
#ifdef DEBUG
                       (!> (hasWait full,"full",full, "\nfull2",full2)) $
#endif
                        let full'= head full: full2
                        in (setData $ Log rec full' full')
#ifdef DEBUG
                           !> ("result1",full')
#endif
                   return $ Just (unsafeCoerce k) <*> x

             appg x = Transient $  do
                   Log rec _ full <- getData `onNothing` return (Log False [] [])
                   liftIO $ writeIORef rg (Just x, full)
#ifdef DEBUG
                     !> ( show $ unsafePerformIO myThreadId) ++ "APPG"
#endif
                   (k,full1) <- liftIO $ readIORef rf
                   when (hasWait  full) $
#ifdef DEBUG
                       (!> ("full", full, "\nfull1",full1)) $
#endif
                        let full'= head full: full1
                        in (setData $ Log rec full' full')
#ifdef DEBUG
                           !> ("result2",full')
#endif
                   return $ unsafeCoerce k <*> Just x

         setContinuation f appf fs

         k <- runTrans f
#ifdef DEBUG
                  !> ( show $ unsafePerformIO myThreadId)++ "RUN f"
#endif
         was <- getData `onNothing` return NoRemote
         when (was == WasParallel) $  setData NoRemote

         Log recovery _ full <- getData `onNothing` return (Log False [] [])



         if was== WasRemote  || (not recovery && was == NoRemote  && isNothing k )
#ifdef DEBUG
              !>  ("was,recovery,isNothing=",was,recovery, isNothing k)
#endif
         -- if the first operand was a remote request
         -- (so this node is not master and hasn't to execute the whole expression)
         -- or it was not an asyncronous term (a normal term without async or parallel
         -- like primitives) and is nothing
           then  do
             restoreStack fs
             return Nothing
           else do
             when (isJust k) $ liftIO $ writeIORef rf  (k,full)
                -- when necessary since it maybe WasParallel and Nothing

             setContinuation g appg fs

             x <- runTrans g
#ifdef DEBUG
                     !> ( show $ unsafePerformIO myThreadId) ++ "RUN g"
#endif
             Log recovery _ full' <- getData `onNothing` return (Log False [] [])
             liftIO $ writeIORef rg  (x,full')
             restoreStack fs
             k'' <- if was== WasParallel
                      then do
                        (k',_) <- liftIO $ readIORef rf -- since k may have been updated by a parallel f
                        return k'
                      else return k
             return $ k'' <*> x

instance MonadIO m => Monad (AsyncT m) where
  return   = pure
  x >>= f  = Transient $ do
    c  <- setEventCont x f
    mk <- runTrans x
    resetEventCont mk c
    case mk of
      Just k  -> runTrans (f k)
      Nothing -> return Nothing

instance MonadIO m => MonadIO (AsyncT m) where
  liftIO mx = do
    ex <- liftIO' $ (mx >>= return . Right) `catch`
                    (\(e :: SomeException) -> return $ Left e)
    case ex of
      Left  e -> back e  -- finish $ Just e
      Right x -> return x
    where liftIO' x = Transient $ liftIO x >>= return . Just
              --     let x= liftIO io in x `seq` lift x

instance (Monoid a, Monad (AsyncT m)) => Monoid (AsyncT m a) where
  mappend x y = mappend <$> x <*> y
  mempty      = return mempty

instance (MonadIO m, MonadPlus (AsyncT m)) => Alternative (AsyncT m) where
    empty = Transient $ return  Nothing
    (<|>) = mplus

instance MonadIO m => MonadPlus (AsyncT m) where
  mzero     = empty
  mplus x y = Transient $ do
    mx <- runTrans x
#ifdef DEBUG
            !> "RUNTRANS11111"
#endif
    was <- getData `onNothing` return NoRemote
    if was == WasRemote
#ifdef DEBUG
        !> was
#endif
      then return Nothing
      else case mx of
            Nothing -> runTrans y
#ifdef DEBUG
                          !> "RUNTRANS22222"
#endif
            justx -> return justx

-------------------------------------------------------------------------------
-- AsyncT transformer
-------------------------------------------------------------------------------

instance MonadTrans AsyncT where
  lift mx = AsyncT $ lift mx >>= return . Just

instance MonadTransControl AsyncT where
    type StT AsyncT a = (Maybe a, EventF)
    liftWith f = AsyncT $ StateT $ \s ->
                   liftM (\x -> (Just x, s))
                         (f $ \t -> runStateT (runTrans t) s)
    restoreT = AsyncT . StateT . const
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance (MonadBase b m, MonadIO m) => MonadBase b (AsyncT m) where
    liftBase = liftBaseDefault

instance (MonadBaseControl b m, MonadIO m) => MonadBaseControl b (AsyncT m) where
    type StM (AsyncT m) a = ComposeSt AsyncT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}

readWithErr :: (Typeable a, Read a) => String -> IO [(a, String)]
readWithErr line =
  (v `seq` return [(v, left)])
     `catch` (\(e :: SomeException) ->
                error $ "read error trying to read type: \"" ++ show (typeOf v)
                     ++ "\" in:  " ++ " <" ++ show line ++ "> ")
  where [(v, left)] = readsPrec 0 line

readsPrec' _ = unsafePerformIO . readWithErr

-- | Constraint type synonym for a value that can be logged.
type Loggable a = (Show a, Read a, Typeable a)

-- | Dynamic serializable data for logging.
data IDynamic =
    IDyns String
  | forall a. Loggable a => IDynamic a

instance Show IDynamic where
  show (IDynamic x) = show (show x)
  show (IDyns    s) = show s

instance Read IDynamic where
  readsPrec n str = map (\(x,s) -> (IDyns x,s)) $ readsPrec' n str

type Recover        = Bool
type CurrentPointer = [LogElem]
type LogEntries     = [LogElem]

data LogElem        =  Wait | Exec | Var IDynamic
  deriving (Read, Show)

data Log            = Log Recover CurrentPointer LogEntries
  deriving (Typeable, Show)

data RemoteStatus   = WasRemote | WasParallel | NoRemote
  deriving (Typeable, Eq, Show)

-- | A synonym of 'empty' that can be used in a monadic expression. It stops
-- the computation, which allows the next computation in an 'Alternative'
-- ('<|>') composition to run.
stop :: Alternative m => m stopped
stop = empty

--instance (Num a,Eq a,Fractional a) =>Fractional (TransIO a)where
--     mf / mg = (/) <$> mf <*> mg
--     fromRational (x:%y) =  fromInteger x % fromInteger y


instance (Num a, Eq a, Monad (AsyncT m)) => Num (AsyncT m a) where
  fromInteger = return . fromInteger
  mf + mg     = (+) <$> mf <*> mg
  mf * mg     = (*) <$> mf <*> mg
  negate f    = f >>= return . negate
  abs f       = f >>= return . abs
  signum f    = f >>= return . signum

class AdditionalOperators m where

  -- | Run @m a@ discarding its result before running @m b@.
  (**>)  :: m a -> m b -> m b

  -- | Run @m b@ discarding its result, after the whole task set @m a@ is
  -- done.
  (<**)  :: m a -> m b -> m a

  atEnd' :: m a -> m b -> m a
  atEnd' = (<**)

  -- | Run @m b@ discarding its result, once after each task in @m a@, and
  -- once again after the whole task set is done.
  (<***) :: m a -> m b -> m a

  atEnd  :: m a -> m b -> m a
  atEnd  = (<***)

instance (Monad m, Monad (AsyncT m)) => AdditionalOperators (AsyncT m) where

  (**>) :: AsyncT m a -> AsyncT m b -> AsyncT m b
  (**>) x y =
    Transient $ do
      runTrans x
      runTrans y

  (<***) :: AsyncT m a -> AsyncT m b -> AsyncT m a
  (<***) ma mb =
    Transient $ do
      fs  <- getContinuations
      setContinuation ma (\x -> mb >> return x)  fs
      a <- runTrans ma
      runTrans mb
      restoreStack fs
      return  a

  (<**) :: AsyncT m a -> AsyncT m b -> AsyncT m a
  (<**) ma mb =
    Transient $ do
      a <- runTrans ma
#ifdef DEBUG
             !> "ma"
#endif

      runTrans  mb
#ifdef DEBUG
        !> "mb"
#endif
      return a

infixr 1 <***, <**, **>

-- | Run @b@ once, discarding its result when the first task in task set @a@
-- has finished. Useful to start a singleton task after the first task has been
-- setup.
(<|) :: MonadIO m => AsyncT m a -> AsyncT m b -> AsyncT m a
(<|) ma mb = Transient $ do
  fs  <- getContinuations
  ref <- liftIO $ newIORef False
  setContinuation ma (cont ref)  fs
  r   <- runTrans ma
  restoreStack fs
  return  r
  where cont ref x = Transient $ do
          n <- liftIO $ readIORef ref
          if n == True
            then return $ Just x
            else do liftIO $ writeIORef ref True
                    runTrans mb
                    return $ Just x

-- | Set the current closure and continuation for the current statement
setEventCont :: Monad m => AsyncT m a -> (a -> AsyncT m b) -> StateM m EventF
setEventCont x f  = do
  EventF { fcomp = fs, .. } <- get
#ifdef DEBUG
                   !> "SET"
#endif
  let cont = EventF { xcomp = x
                    , fcomp = unsafeCoerce f : unsafeCoerce fs
                    , .. }
  put cont
  return cont

-- | Reset the closure and continuation. Remove inner binds than the previous
-- computations may have stacked in the list of continuations.
resetEventCont :: forall m a b. MonadIO m
    => Maybe a -> EventF -> StateM m (AsyncT m b -> AsyncT m b)
resetEventCont mx _ = do
  EventF { fcomp = fs, .. } <- get
#ifdef DEBUG
                   !> "reset"
#endif
  let f mx = case mx of
        Nothing -> (empty :: AsyncT m a)
        Just x  -> unsafeCoerce (head fs) x
  put $ EventF { xcomp = f mx
               , fcomp = tailsafe (unsafeCoerce fs)
               , .. }
  return  id

-- | Total variant of `tail` that returns an empty list when given an empty list.
tailsafe :: [a] -> [a]
tailsafe []     = []
tailsafe (x:xs) = xs

--refEventCont= unsafePerformIO $ newIORef baseEffects

-- effects not used
-- {-# INLINE baseEffects #-}
--baseEffects :: Effects
--
--baseEffects x  x' f' = do
--            c <- setEventCont x'  f'
--            mk <- runTrans x
--            t <- resetEventCont mk c
--            return (t,mk)


-- * Threads

waitQSemB   sem = atomicModifyIORef sem $ \n ->
                    if n > 0 then(n - 1, True) else (n, False)
signalQSemB sem = atomicModifyIORef sem $ \n -> (n + 1, ())

-- | Sets the maximum number of threads that can be created for the given task
-- set.  When set to 0, new tasks start synchronously in the current thread.
-- New threads are created by 'parallel', and APIs that use parallel.
threads :: MonadIO m => Int -> AsyncT m a -> AsyncT m a
threads n process = do
   msem <- gets maxThread
   sem <- liftIO $ newIORef n
   modify $ \s -> s { maxThread = Just sem }
   r <- process <** (modify $ \s -> s { maxThread = msem }) -- restore it
   return r

-- | Terminate all the child threads in the given task set and continue
-- execution in the current thread. Useful to reap the children when a task is
-- done.
--
oneThread :: MonadIO m => AsyncT m a -> AsyncT m a
oneThread comp = do
  st    <-  get
  chs   <- liftIO $ newMVar []
  label <- liftIO $ newIORef (Alive, BS.pack "oneThread")
  let st' = st { parent   = Just st
              , children = chs
              , labelth  = label }
  liftIO $ hangThread st st'
  put st'
  x   <- comp
  th  <- liftIO myThreadId
#ifdef DEBUG
          !> ("FATHER:", threadId st)
#endif
  chs <- liftIO $ readMVar chs -- children st'
  liftIO $ mapM_ (killChildren1 th) chs
  return x
  where killChildren1 :: ThreadId  ->  EventF -> IO ()
        killChildren1 th state = do
          ths' <- modifyMVar (children state) $ \ths -> do
                    let (inn, ths')=  partition (\st -> threadId st == th) ths
                    return (inn, ths')
          mapM_ (killChildren1  th) ths'
          mapM_ (killThread . threadId) ths'
#ifdef DEBUG
            !> ("KILLEVENT1 ", map threadId ths' )
#endif

-- | Add a label to the current passing threads so it can be printed by debugging calls like `showThreads`
labelState :: String -> TransIO ()
labelState l =  do
  st <- get
  liftIO $ atomicModifyIORef (labelth st) $ \(status,_) -> ((status, BS.pack l), ())

printBlock :: MVar ()
printBlock = unsafePerformIO $ newMVar ()

-- | Show the tree of threads hanging from the state.
showThreads :: MonadIO m => EventF -> m ()
showThreads st = liftIO $ withMVar printBlock $ const $ do
  mythread <- myThreadId
#ifdef DEBUG
                !> "showThreads"
#endif
  putStrLn "---------Threads-----------"
  let showTree n ch = do
        liftIO $ do
          putStr $ take n $ repeat ' '
          (state, label) <- readIORef $ labelth ch
          if BS.null label
            then putStr . show $ threadId ch
            else do BS.putStr label; putStr . drop 8 . show $ threadId ch
                    when (state == Dead) $ putStr " dead"
          putStrLn $ if mythread == threadId ch then " <--" else ""
        chs <- readMVar $ children ch
        mapM_ (showTree $ n + 2) $ reverse chs
  showTree 0 st

-- | Return the state of the thread that initiated the transient computation
topState :: MonadState EventF (AsyncT m) => AsyncT m EventF
topState = do
  st <- get
  return $ toplevel st
  where toplevel st = case parent st of
                        Nothing -> st
                        Just p  -> toplevel p

-- | Return the state variable of the type desired with which a thread, identified by his number in the treee was initiated
showState :: (Typeable a, MonadIO m, Alternative m) => String -> EventF -> m  (Maybe a)
showState th top = resp
  where resp = do
          let thstring = drop 9 . show $ threadId top
          if thstring == th
          then getstate top
          else do
            sts <- liftIO $ readMVar $ children top
            foldl (<|>) empty $ map (showState th) sts
        getstate st =
            case M.lookup (typeOf $ typeResp resp) $ mfData st of
              Just x  -> return . Just $ unsafeCoerce x
              Nothing -> return Nothing
        typeResp :: m (Maybe x) -> x
        typeResp = undefined

-- | Add n threads to the limit of threads. If there is no limit, the limit is set.
addThreads' :: MonadIO m => Int -> AsyncT m ()
addThreads' n= noTrans $ do
  msem <- gets maxThread
  case msem of
    Just sem -> liftIO $ modifyIORef sem $ \n' -> n + n'
    Nothing  -> do
      sem <- liftIO (newIORef n)
      modify $ \ s -> s { maxThread = Just sem }

-- | Ensure that at least n threads are available for the current task set.
addThreads :: Int -> TransIO ()
addThreads n = noTrans $ do
  msem <- gets maxThread
  case msem of
    Nothing  -> return ()
    Just sem -> liftIO $ modifyIORef sem $ \n' -> if n' > n then n' else  n

--getNonUsedThreads :: TransIO (Maybe Int)
--getNonUsedThreads= Transient $ do
--   msem <- gets maxThread
--   case msem of
--    Just sem -> liftIO $ Just <$> readIORef sem
--    Nothing -> return Nothing

-- | Disable tracking and therefore the ability to terminate the child threads.
-- By default, child threads are terminated automatically when the parent
-- thread dies, or they can be terminated using the kill primitives. Disabling
-- it may improve performance a bit, however, all threads must be well-behaved
-- to exit on their own to avoid a leak.
freeThreads :: Monad m => AsyncT m a -> AsyncT m a
freeThreads process = Transient $ do
  st <- get
  put st { freeTh = True }
  r  <- runTrans process
  modify $ \s -> s { freeTh = freeTh st }
  return r

-- | Enable tracking and therefore the ability to terminate the child threads.
-- This is the default but can be used to re-enable tracking if it was
-- previously disabled with 'freeThreads'.
hookedThreads :: Monad m => AsyncT m a -> AsyncT m a
hookedThreads process = Transient $ do
  st <- get
  put st {freeTh = False}
  r  <- runTrans process
  modify $ \st -> st { freeTh = freeTh st }
  return r

-- | Kill all the child threads of the current thread.
killChilds :: MonadIO m => AsyncT m ()
killChilds = noTrans $ do
  cont <- get
  liftIO $ do
    killChildren $ children cont
    writeIORef (labelth cont) (Alive, mempty)
#ifdef DEBUG
      !> (threadId cont,"relabeled")
#endif
  return ()

-- | Kill the current thread and the childs.
killBranch :: MonadIO m => AsyncT m ()
killBranch = noTrans $ do
  st <- get
  liftIO $ killBranch' st

-- | Kill the childs and the thread of an state
killBranch' :: EventF -> IO ()
killBranch' cont = do
  killChildren $ children cont
  let thisth  = threadId  cont
      mparent = parent    cont
  when (isJust mparent) $
    modifyMVar_ (children $ fromJust mparent) $ \sts ->
      return $ filter (\st -> threadId st /= thisth) sts
  killThread $ thisth

-- * Extensible State: Session Data Management

-- | Same as 'getSData' but with a more general type. If the data is found, a
-- 'Just' value is returned. Otherwise, a 'Nothing' value is returned.
getData :: (MonadState EventF m, Typeable a) => m (Maybe a)
getData = resp
  where resp = do
          list <- gets mfData
          case M.lookup (typeOf $ typeResp resp) list of
            Just x  -> return . Just $ unsafeCoerce x
            Nothing -> return Nothing
        typeResp :: m (Maybe x) -> x
        typeResp = undefined

-- | Retrieve a previously stored data item of the given data type from the
-- monad state. The data type to retrieve is implicitly determined from the
-- requested type context.
-- If the data item is not found, an 'empty' value (a void event) is returned.
-- Remember that an empty value stops the monad computation. If you want to
-- print an error message or a default value in that case, you can use an
-- 'Alternative' composition. For example:
--
-- > getSData <|> error "no data"
-- > getInt = getSData <|> return (0 :: Int)
getSData ::  (Monad m, Typeable a) => AsyncT m a
getSData = Transient getData

-- | Same as `getSData`
getState ::  (Monad m, Typeable a) => AsyncT m a
getState = getSData

-- | 'setData' stores a data item in the monad state which can be retrieved
-- later using 'getData' or 'getSData'. Stored data items are keyed by their
-- data type, and therefore only one item of a given type can be stored. A
-- newtype wrapper can be used to distinguish two data items of the same type.
--
-- @
-- import Control.Monad.IO.Class (liftIO)
-- import Transient.Base
-- import Data.Typeable
--
-- data Person = Person
--    { name :: String
--    , age :: Int
--    } deriving Typeable
--
-- main = keep $ do
--      setData $ Person "Alberto"  55
--      Person name age <- getSData
--      liftIO $ print (name, age)
-- @
setData :: (MonadState EventF m, Typeable a) => a -> m ()
setData x = modify $ \st -> st { mfData = M.insert t (unsafeCoerce x) (mfData st) }
  where t = typeOf x

setSData ::  (Monad m, Typeable a) => a -> AsyncT m ()
setSData x = AsyncT $ do
    setData x
    return (Just ())

-- | Accepts a function that takes the current value of the stored data type
-- and returns the modified value. If the function returns 'Nothing' the value
-- is deleted otherwise updated.
modifyData :: (MonadState EventF m, Typeable a) => (Maybe a -> Maybe a) -> m ()
modifyData f = modify $ \st -> st { mfData = M.alter alterf t (mfData st) }
  where typeResp :: (Maybe a -> b) -> a
        typeResp   = undefined
        t          = typeOf (typeResp f)
        alterf mx  = unsafeCoerce $ f x'
          where x' = case mx of
                       Just x  -> Just $ unsafeCoerce x
                       Nothing -> Nothing

-- | Same as modifyData
modifyState :: (MonadState EventF m, Typeable a) => (Maybe a -> Maybe a) -> m ()
modifyState = modifyData

-- | Same as 'setData'
setState :: (MonadState EventF m, Typeable a) => a -> m ()
setState = setData

-- | Delete the data item of the given type from the monad state.
delData :: (MonadState EventF m, Typeable a) => a -> m ()
delData x = modify $ \st -> st { mfData = M.delete (typeOf x) (mfData st) }

-- | Same as 'delData'
delState :: (MonadState EventF m, Typeable a) => a -> m ()
delState = delData

-- | Run an action, if the result is a void action undo any state changes
-- that it might have caused.
try :: MonadIO m => AsyncT m a -> AsyncT m a
try mx = do
  sd <- gets mfData
  mx <|> (modify (\s -> s { mfData = sd }) >> empty)

-- | Executes the computation and reset the state either if it fails or not
sandbox :: MonadIO m => AsyncT m a -> AsyncT m a
sandbox mx = do
  sd <- gets mfData
  mx <*** modify (\s ->s { mfData = sd})

-- | Generator of identifiers that are unique within the current monadic
-- sequence They are not unique in the whole program.
genId :: MonadState EventF m => m Int
genId = do
  st <- get
  let n = mfSequence st
  put st { mfSequence = n + 1 }
  return n

getPrevId :: MonadState EventF m => m Int
getPrevId = gets mfSequence

instance Read SomeException where
  readsPrec n str = [(SomeException $ ErrorCall s, r)]
    where [(s , r)] = read str

-- | 'StreamData' represents a task in a task stream being generated.
data StreamData a =
      SMore a               -- ^ More tasks to come
    | SLast a               -- ^ This is the last task
    | SDone                 -- ^ No more tasks, we are done
    | SError SomeException  -- ^ An error occurred
    deriving (Typeable, Show,Read)

-- | An task stream generator that produces an infinite stream of tasks by
-- running an IO computation in a loop. A task is triggered carrying the output
-- of the computation. See 'parallel' for notes on the return value.
waitEvents :: (MonadIO m, MonadBaseControl IO m) => IO a -> AsyncT m a
waitEvents io = do
  mr <- parallel (SMore <$> io)
  case mr of
    SMore  x -> return x
    SError e -> back   e

-- | Run an IO computation asynchronously and generate a single task carrying
-- the result of the computation when it completes. See 'parallel' for notes on
-- the return value.
async  :: (MonadIO m, MonadBaseControl IO m) => IO a -> AsyncT m a
async io = do
  mr <- parallel (SLast <$> io)
  case mr of
    SLast  x -> return x
    SError e -> back   e

-- | Force an async computation to run synchronously. It can be useful in an
-- 'Alternative' composition to run the alternative only after finishing a
-- computation.  Note that in Applicatives it might result in an undesired
-- serialization.
sync :: MonadIO m => AsyncT m a -> AsyncT m a
sync x = do
  setSData WasRemote
  r <- x
  delData WasRemote
  return r

-- | @spawn = freeThreads . waitEvents@
spawn :: (MonadIO m, MonadBaseControl IO m) => IO a -> AsyncT m a
spawn = freeThreads . waitEvents

-- | An task stream generator that produces an infinite stream of tasks by
-- running an IO computation periodically at the specified time interval. The
-- task carries the result of the computation.  A new task is generated only if
-- the output of the computation is different from the previous one.  See
-- 'parallel' for notes on the return value.
sample :: (Eq a, MonadIO m, MonadBaseControl IO m) => IO a -> Int -> AsyncT m a
sample action interval = do
  v    <- liftIO action
  prev <- liftIO $ newIORef v
  waitEvents (loop action prev) <|> async (return v)
  where loop action prev = loop'
          where loop' = do
                  threadDelay interval
                  v  <- action
                  v' <- readIORef prev
                  if v /= v' then writeIORef prev v >> return v else loop'

--serial  ::    IO (StreamData b) -> TransIO (StreamData b)
--serial  ioaction= Transient $   do
--    cont <- get                    -- !> "PARALLEL"
--    case event cont of
--         j@(Just _) -> do
--                    put cont{event=Nothing}
--                    return $ unsafeCoerce j
--         Nothing -> do
--                    liftIO $ loop cont ioaction
--                    return Nothing
--
--   where loop cont ioaction= do
--            let iocont dat= do
--                    runStateT (runCont cont) cont{event= Just $ unsafeCoerce dat}
--                    return ()
--            mdat <- ioaction `catch` \(e :: SomeException) -> return $ SError e
--            case mdat of
--                 se@(SError _) ->  iocont se
--                 SDone ->          iocont SDone
--                 last@(SLast _) -> iocont last
--
--                 more@(SMore _) -> do
--                      iocont more
--                      loop cont ioaction

-- | Run an IO action one or more times to generate a stream of tasks. The IO
-- action returns a 'StreamData'. When it returns an 'SMore' or 'SLast' a new
-- task is triggered with the result value. If the return value is 'SMore', the
-- action is run again to generate the next task, otherwise task creation
-- stops.
--
-- Unless the maximum number of threads (set with 'threads') has been reached,
-- the task is generated in a new thread and the current thread returns a void
-- task.
parallel  :: (Monad m, MonadIO m, MonadBaseControl IO m)
    => IO (StreamData a) -> AsyncT m (StreamData a)
parallel ioaction = Transient $ do
  cont <- get
#ifdef DEBUG
            !> "PARALLEL"
#endif
  case event cont of
    j@(Just _) -> do
      put cont { event = Nothing }
      return $ unsafeCoerce j
    Nothing    -> do
      liftIO $ atomicModifyIORef (labelth cont) $ \(_, lab) -> ((Parent, lab), ())
      lift $ loop cont ioaction
      was <- getData `onNothing` return NoRemote
      when (was /= WasRemote) $ setData WasParallel
--            th <- liftIO myThreadId
--            return () !> ("finish",th)
      return Nothing

-- | Execute the IO action and the continuation
loop ::  (MonadIO m, MonadBaseControl IO m) => EventF -> IO (StreamData t) -> m ()
loop parentc rec = forkMaybe parentc $ \cont -> do
  -- Execute the IO computation and then the closure-continuation
  liftIO $ atomicModifyIORef (labelth cont) $ const ((Listener,BS.pack "wait"),())
  let loop'=   do
         mdat <- liftIO $ rec `catch` \(e :: SomeException) -> return $ SError e
         case mdat of
             se@(SError _)  -> setworker cont >> iocont  se    cont
             SDone          -> setworker cont >> iocont  SDone cont
             last@(SLast _) -> setworker cont >> iocont  last  cont

             more@(SMore _) -> do
                  forkMaybe cont $ iocont  more
                  loop'

         where
         setworker cont= liftIO $ atomicModifyIORef (labelth cont) $ const ((Alive,BS.pack "work"),())

         iocont  dat cont = do

              let cont'= cont{event= Just $ unsafeCoerce dat}
              runStateT (runCont cont')  cont'
              return ()


  loop'
  return ()
  where

  forkMaybe parent  proc = do
     case maxThread parent  of
       Nothing -> forkIt parent  proc
       Just sem  -> do
             dofork <- liftIO $ waitQSemB sem
             if dofork then  forkIt parent proc else proc parent


  forkIt parent  proc= do
     chs <- liftIO $ newMVar []

     label <- liftIO $ newIORef (Alive, BS.pack "work")
     let cont = parent{parent=Just parent,children=   chs, labelth= label}

     forkFinally1 (do
         th <- liftIO $ myThreadId
         let cont'= cont{threadId=th}
         when (not $ freeTh parent ) $ liftIO $ hangThread parent cont'
                                    -- !>  ("thread created: ",th,"in",threadId parent )

         proc cont')
         $ \me -> do

--             case me of -- !> "THREAD END" of
--              Left  e -> do
----                 when (fromException e /= Just ThreadKilled)$
--                 liftIO $ print e
--                 killChildren $ children cont
----                                   !> "KILL RECEIVED" ++ (show $ unsafePerformIO myThreadId)
--
--              Right _ ->



             case maxThread cont of
               Just sem -> signalQSemB sem      -- !> "freed thread"
               Nothing -> when(not $ freeTh parent  )  $ do -- if was not a free thread

                 th <- myThreadId
                 (can,label) <- atomicModifyIORef (labelth cont) $ \(l@(status,label)) ->
                    ((if status== Alive then Dead else status, label),l)


                 when (can/= Parent ) $ free th parent
     return ()

  forkFinally1 :: (MonadIO m, MonadBaseControl IO m) =>
        m a -> (Either SomeException a -> IO ()) -> m ThreadId
  forkFinally1 action and_then =
        E.mask $ \restore ->
            liftBaseWith $ \runInIO -> forkIO $ do
                runInIO $ E.try (restore action) >>= liftIO . and_then
                return ()

free th env= do
--       return ()                                       !> ("freeing",th,"in",threadId env)
       let sibling=  children env

       (sbs',found) <- modifyMVar sibling $ \sbs -> do
                   let (sbs', found) = drop [] th  sbs
                   return (sbs',(sbs',found))



       if found
         then do

--                                             !> ("new list for",threadId env,map threadId sbs')
           (typ,_) <- readIORef $ labelth env
           if (null sbs' && typ /= Listener && isJust (parent env))
            -- free the parent
            then free (threadId env) ( fromJust $ parent env)
            else return ()

--               return env
         else return () -- putMVar sibling sbs
                                                     -- !>  (th,"orphan")

       where
       drop processed th []= (processed,False)
       drop processed th (ev:evts)| th ==  threadId ev= (processed ++ evts, True)
                                  | otherwise= drop (ev:processed) th evts



hangThread parentProc child =  do

       let headpths= children parentProc

       modifyMVar_ headpths $ \ths -> return (child:ths)
--       ths <- takeMVar headpths
--       putMVar headpths (child:ths)


           --  !> ("hang", threadId child, threadId parentProc,map threadId ths,unsafePerformIO $ readIORef $ labelth parentProc)

-- | kill  all the child threads associated with the continuation context
killChildren childs  = do


           ths <- modifyMVar childs $ \ths -> return ([],ths)
--           ths <- takeMVar childs
--           putMVar childs []

           mapM_ (killChildren . children) ths


           mapM_ (killThread . threadId) ths  -- !> ("KILL", map threadId ths )





-- | Make a transient task generator from an asynchronous callback handler.
--
-- The first parameter is a callback. The second parameter is a value to be
-- returned to the callback; if the callback expects no return value it
-- can just be a @return ()@. The callback expects a setter function taking the
-- @eventdata@ as an argument and returning a value to the callback; this
-- function is supplied by 'react'.
--
-- Callbacks from foreign code can be wrapped into such a handler and hooked
-- into the transient monad using 'react'. Every time the callback is called it
-- generates a new task for the transient monad.
--
react
  :: (Monad m, MonadIO m, Typeable eventdata)
  => ((eventdata ->  m response) -> m ())
  -> IO  response
  -> AsyncT m eventdata
react setHandler iob= Transient $ do
        cont    <- get
        case event cont of
          Nothing -> do
            lift $ setHandler $ \dat ->do
              runStateT (runCont cont) cont{event= Just $ unsafeCoerce dat}
              liftIO iob
            was <- getData `onNothing` return NoRemote
            when (was /= WasRemote) $ setData WasParallel
            return Nothing

          j@(Just _) -> do
            put cont{event=Nothing}
            return $ unsafeCoerce j

-- | Runs a computation asynchronously without generating any events. Returns
-- 'empty' in an 'Alternative' composition.

abduce :: (MonadIO m, MonadBaseControl IO m) => AsyncT m ()
abduce = Transient $ do
   st <-  get
   case  event st of
          Just _ -> do
               put st{event=Nothing}
               return $ Just ()
          Nothing -> do
               chs <- liftIO $ newMVar []

               label <-  liftIO $ newIORef (Alive, BS.pack "abduce")
               liftBaseWith $ \runInIO -> forkIO $ do
                   th <- myThreadId
                   let st' = st{event= Just (),parent=Just st,children=   chs, threadId=th,labelth= label}
                   liftIO $ hangThread st st'

                   runInIO $ runCont' st'
                   return()
               return Nothing


-- * non-blocking keyboard input

getLineRef= unsafePerformIO $ newTVarIO Nothing


roption= unsafePerformIO $ newMVar []

-- | Waits on stdin in a loop and triggers a new task every time the input data
-- matches the first parameter.  The value contained by the task is the matched
-- value i.e. the first argument  itself. The second parameter is a label for
-- the option. The label is displayed on the console when the option is
-- activated.
--
-- Note that if two independent invocations of 'option' are expecting the same
-- input, only one of them gets it and triggers a task. It cannot be
-- predicted which one gets it.
--
option :: (MonadIO m, MonadBaseControl IO m, Typeable b, Show b, Read b, Eq b)
    => b -> String -> AsyncT m b
option ret message= do
    let sret= show ret

    liftIO $ putStrLn $ "Enter  "++sret++"\tto: " ++ message
    liftIO $ modifyMVar_ roption $ \msgs-> return $ sret:msgs
    waitEvents  $ getLine' (==ret)
    liftIO $ putStr "\noption: " >> putStrLn (show ret)
    return ret


-- | Waits on stdin and triggers a task when a console input matches the
-- predicate specified in the first argument.  The second parameter is a string
-- to be displayed on the console before waiting.
--
input :: (MonadIO m, Typeable a, Read a, Show a)
    => (a -> Bool) -> String -> AsyncT m a
input cond prompt= Transient . liftIO $do
   putStr prompt >> hFlush stdout
   atomically $ do
       mr <- readTVar getLineRef
       case mr of
         Nothing -> STM.retry
         Just r ->
            case reads1 r  of
            (s,_):_ -> if cond s  --  !> show (cond s)
                     then do
                       unsafeIOToSTM $ print s
                       writeTVar  getLineRef Nothing -- !>"match"
                       return $ Just s

                     else return Nothing
            _ -> return Nothing

-- | Non blocking `getLine` with a validator
getLine' cond=    do
     atomically $ do
       mr <- readTVar getLineRef
       case mr of
         Nothing -> STM.retry
         Just r ->
            case reads1 r of --  !> ("received " ++  show r ++ show (unsafePerformIO myThreadId)) of
            (s,_):_ -> if cond s -- !> show (cond s)
                     then do
                       writeTVar  getLineRef Nothing -- !>"match"
                       return s

                     else STM.retry
            _ -> STM.retry

reads1 s=x where
      x= if typeOf(typeOfr x) == typeOf "" then unsafeCoerce[(s,"")] else readsPrec' 0 s
      typeOfr :: [(a,String)] ->  a
      typeOfr  = undefined


inputLoop= do
           r<- getLine
           -- XXX hoping that the previous value has been consumed by now.
           -- otherwise its just lost by overwriting.
           atomically $ writeTVar getLineRef Nothing
           processLine r
           inputLoop

processLine r= do

   let rs = breakSlash [] r

   -- XXX this blocks forever if an input is not consumed by any consumer.
   -- e.g. try this "xxx/xxx" on the stdin
   liftIO $ mapM_ (\ r ->
                 atomically $ do
--                    threadDelay 1000000
                    t <- readTVar getLineRef
                    when (isJust  t) STM.retry
                    writeTVar  getLineRef $ Just r ) rs


    where
    breakSlash :: [String] -> String -> [String]
    breakSlash [] ""= [""]
    breakSlash s ""= s
    breakSlash res ('\"':s)=
      let (r,rest) = span(/= '\"') s
      in breakSlash (res++[r]) $ tail1 rest

    breakSlash res s=
      let (r,rest) = span(/= '/') s
      in breakSlash (res++[r]) $ tail1 rest

    tail1 []=[]
    tail1 x= tail x




-- | Wait for the execution of `exit` and return the result or the exhaustion of thread activity

stay rexit=  takeMVar rexit
 `catch` \(e :: BlockedIndefinitelyOnMVar) -> return Nothing

newtype Exit a= Exit a deriving Typeable

-- | Runs the transient computation in a child thread and keeps the main thread
-- running until all the user threads exit or some thread invokes 'exit'.
--
-- The main thread provides facilities to accept keyboard input in a
-- non-blocking but line-oriented manner. The program reads the standard input
-- and feeds it to all the async input consumers (e.g. 'option' and 'input').
-- All async input consumers contend for each line entered on the standard
-- input and try to read it atomically. When a consumer consumes the input
-- others do not get to see it, otherwise it is left in the buffer for others
-- to consume. If nobody consumes the input, it is discarded.
--
-- A @/@ in the input line is treated as a newline.
--
-- When using asynchronous input, regular synchronous IO APIs like getLine
-- cannot be used as they will contend for the standard input along with the
-- asynchronous input thread. Instead you can use the asynchronous input APIs
-- provided by transient.
--
-- A built-in interactive command handler also reads the stdin asynchronously.
-- All available commands handled by the command handler are displayed when the
-- program is run.  The following commands are available:
--
-- 1. @ps@: show threads
-- 2. @log@: inspect the log of a thread
-- 3. @end@, @exit@: terminate the program
--
-- An input not handled by the command handler can be handled by the program.
--
-- The program's command line is scanned for @-p@ or @--path@ command line
-- options.  The arguments to these options are injected into the async input
-- channel as keyboard input to the program. Each line of input is separated by
-- a @/@. For example:
--
-- >  foo  -p  ps/end
--
keep :: Typeable a => AsyncT IO a -> IO (Maybe a)
keep mx = do

   liftIO $ hSetBuffering stdout LineBuffering
   rexit <- newEmptyMVar
   forkIO $ do
--       liftIO $ putMVar rexit  $ Right Nothing
       runTransient $ do
           st <- get

           setSData $ Exit rexit
           (async (return ()) >> labelState "input" >> liftIO inputLoop)

            <|> do
                   option "ps" "show threads"
                   liftIO $ showThreads st
                   empty
            <|> do
                   option "log" "inspect the log of a thread"
                   th <- input (const True)  "thread number>"
                   ml <- liftIO $ showState th st
                   liftIO $ print $ fmap (\(Log _ _ log) -> reverse log) ml
                   empty
            <|> do
                   option "end" "exit"
                   killChilds
                   liftIO $ putMVar rexit Nothing
                   empty
            <|> mx
       return ()
   threadDelay 10000
   execCommandLine
   stay rexit

   where
   type1 :: TransIO a -> Either String (Maybe a)
   type1= undefined

-- | Same as `keep` but does not read from the standard input, and therefore
-- the async input APIs ('option' and 'input') cannot be used in the monad.
-- However, keyboard input can still be passed via command line arguments as
-- described in 'keep'.  Useful for debugging or for creating background tasks,
-- as well as to embed the Transient monad inside another computation. It
-- returns either the value returned by `exit`.  or Nothing, when there are no
-- more threads running
--
keep' :: Typeable a => AsyncT IO a -> IO (Maybe a)
keep' mx  = do
   liftIO $ hSetBuffering stdout LineBuffering
   rexit <- newEmptyMVar
   forkIO $ do
           runTransient $ do
              setSData $ Exit rexit
              mx

           return ()
   threadDelay 10000
   forkIO $ execCommandLine
   stay rexit


execCommandLine= do
   args <- getArgs
   let mindex =  findIndex (\o ->  o == "-p" || o == "--path" ) args
   when (isJust mindex) $ do
        let i= fromJust mindex +1
        when (length  args >= i) $ do
          let path= args !! i
          putStr "Executing: " >> print  path
          processLine  path

-- | Exit the main thread, and thus all the Transient threads (and the
-- application if there is no more code)
exit :: (MonadIO m, Typeable a) => a -> AsyncT m a
exit x= do
  Exit rexit <- getSData <|> error "exit: not the type expected"  `asTypeOf` type1 x
  liftIO $  putMVar  rexit  $ Just x
  stop
  where
  type1 :: a -> AsyncT m (Exit (MVar (Maybe a)))
  type1= undefined



-- | If the first parameter is 'Nothing' return the second parameter otherwise
-- return the first parameter..
onNothing :: Monad m => m (Maybe b) -> m b -> m b
onNothing iox iox'= do
       mx <- iox
       case mx of
           Just x -> return x
           Nothing -> iox'





----------------------------------backtracking ------------------------


data Backtrack b= Show b =>Backtrack{backtracking :: Maybe b
                                    ,backStack :: [EventF] }
                                    deriving Typeable



-- | Delete all the undo actions registered till now for the given track id.
backCut :: (Monad m, Typeable b, Show b) => b -> AsyncT m ()
backCut reason= Transient $ do
     delData $ Backtrack (Just reason)  []
     return $ Just ()

-- | 'backCut' for the default track; equivalent to @backCut ()@.
undoCut ::  Monad m => AsyncT m ()
undoCut = backCut ()

-- | Run the action in the first parameter and register the second parameter as
-- the undo action. On undo ('back') the second parameter is called with the
-- undo track id as argument.
--
{-# NOINLINE onBack #-}
onBack :: (Monad m, MonadIO m, Typeable b, Show b)
    => AsyncT m a -> ( b -> AsyncT m a) -> AsyncT m a
onBack ac bac = registerBack (typeof bac) $ Transient $ do
     Backtrack mreason _  <- getData `onNothing` backStateOf (typeof bac)
     runTrans $ case mreason of
                  Nothing     -> ac
                  Just reason -> bac reason
     where
     typeof :: Monad m => (b -> AsyncT m a) -> b
     typeof = undefined

-- | 'onBack' for the default track; equivalent to @onBack ()@.
onUndo :: MonadIO m => AsyncT m a -> AsyncT m a -> AsyncT m a
onUndo x y= onBack x (\() -> y)


-- | Register an undo action to be executed when backtracking. The first
-- parameter is a "witness" whose data type is used to uniquely identify this
-- backtracking action. The value of the witness parameter is not used.
--
{-# NOINLINE registerUndo #-}
registerBack :: (Monad m, MonadIO m, Typeable b, Show b)
    => b -> AsyncT m a -> AsyncT m a
registerBack witness f  = Transient $ do
   cont@(EventF _ _ x _ _ _ _ _ _ _ _ _)  <- get   -- !!> "backregister"

   md <- getData `asTypeOf` (Just <$> backStateOf witness)

   case md of
        Just (bss@(Backtrack b (bs@((EventF _ _ x'  _ _ _ _ _ _ _ _ _):_)))) ->
           when (isNothing b) $ do
               addrx  <- addr x
               addrx' <- addr x'         -- to avoid duplicate backtracking points
               setData $ if addrx == addrx' then bss else  Backtrack mwit (cont:bs)
        Nothing ->  setData $ Backtrack mwit [cont]

   runTrans f
   where
   mwit= Nothing `asTypeOf` (Just witness)
   addr x = liftIO $ return . hashStableName =<< (makeStableName $! x)


registerUndo :: MonadIO m => AsyncT m a -> AsyncT m a
registerUndo f= registerBack ()  f

-- XXX Should we enforce retry of the same track which is being undone? If the
-- user specifies a different track would it make sense?
--
-- | For a given undo track id, stop executing more backtracking actions and
-- resume normal execution in the forward direction. Used inside an undo
-- action.
--
forward :: (Monad m, Typeable b, Show b) => b -> AsyncT m ()
forward reason= Transient $ do
    Backtrack _ stack <- getData `onNothing`  (backStateOf reason)
    setData $ Backtrack(Nothing `asTypeOf` Just reason)  stack
    return $ Just ()

-- | 'forward' for the default undo track; equivalent to @forward ()@.
retry :: Monad m  => AsyncT m ()
retry= forward ()

-- | Abort finish. Stop executing more finish actions and resume normal
-- execution.  Used inside 'onFinish' actions.
--
noFinish :: Monad m  => AsyncT m ()
noFinish= forward (FinishReason Nothing)

-- | Start the undo process for the given undo track id. Performs all the undo
-- actions registered till now in reverse order. An undo action can use
-- 'forward' to stop the undo process and resume forward execution. If there
-- are no more undo actions registered execution stops and a 'stop' action is
-- returned.
--
back :: (MonadIO m, Typeable b, Show b) => b -> AsyncT m a
back reason = Transient $ do
  bs <- getData  `onNothing`  backStateOf  reason           -- !!>"GOBACK"
  goBackt  bs

  where

  goBackt (Backtrack _ [] )= return Nothing                      -- !!> "END"
  goBackt (Backtrack b (stack@(first : bs)) )= do
        (setData $ Backtrack (Just reason) stack)

        mr <-  runClosure first                                  -- !> "RUNCLOSURE"

        Backtrack back _ <- getData `onNothing`  backStateOf  reason
                                                                 -- !> "END RUNCLOSURE"
--        case back of
--           Nothing -> case mr of
--                   Nothing ->  return empty                      -- !> "FORWARD END"
--                   Just x  ->  runContinuation first x           -- !> "FORWARD EXEC"
--           justreason -> goBackt $ Backtrack justreason bs       -- !> ("BACK AGAIN",back)

        case mr of
           Nothing -> return empty                                      -- !> "END EXECUTION"
           Just x -> case back of
                 Nothing -> runContinuation first x                     -- !> "FORWARD EXEC"
                 justreason -> goBackt $ Backtrack justreason bs        -- !> ("BACK AGAIN",back)

backStateOf :: (Monad m, Show a, Typeable a) => a -> m (Backtrack a)
backStateOf reason= return $ Backtrack (Nothing `asTypeOf` (Just reason)) []

-- | 'back' for the default undo track; equivalent to @back ()@.
--
undo :: MonadIO m => AsyncT m a
undo= back ()


------ finalization

newtype FinishReason= FinishReason (Maybe SomeException) deriving (Typeable, Show)

-- | Clear all finish actions registered till now.
initFinish :: Monad m => AsyncT m ()
initFinish= backCut (FinishReason Nothing)

-- | Register an action that to be run when 'finish' is called. 'onFinish' can
-- be used multiple times to register multiple actions. Actions are run in
-- reverse order. Used in infix style.
--
onFinish :: MonadIO m => ((Maybe SomeException) -> AsyncT m ()) -> AsyncT m ()
onFinish f= onFinish' (return ()) f


-- | Run the action specified in the first parameter and register the second
-- parameter as a finish action to be run when 'finish' is called. Used in
-- infix style.
--
onFinish' :: MonadIO m
    => AsyncT m a ->((Maybe SomeException) -> AsyncT m a) -> AsyncT m a
onFinish' proc f= proc `onBack`   \(FinishReason reason) ->
    f reason


-- | Execute all the finalization actions registered up to the last
-- 'initFinish', in reverse order.  Either an exception or 'Nothing' can be
-- passed to 'finish'.  The argument passed is made available in the 'onFinish'
-- actions invoked.
--
finish :: MonadIO m => Maybe SomeException -> AsyncT m a
finish reason= back (FinishReason reason)



-- | trigger finish when the stream of data ends
checkFinalize v=
   case v of
      SDone ->  stop
      SLast x ->  return x
      SError e -> back  e
      SMore x -> return x

------ exceptions ---
--
-- | Install an exception handler.  On exception, currently installed handlers
-- are executed in reverse (i.e. last in first out) order. Note that multiple
-- handlers can be installed for the same exception type.
--
onException :: (Exception e, MonadIO m) => (e -> AsyncT m ()) -> AsyncT m ()
onException exc= return () `onException'` exc


onException' :: (Exception e, MonadIO m)
    => AsyncT m a -> (e -> AsyncT m a) -> AsyncT m a
onException' mx f= onAnyException mx $ \e ->
    case fromException e of
       Nothing -> empty
       Just e'  -> f e'
  where
  onAnyException :: MonadIO m
    => AsyncT m a -> (SomeException -> AsyncT m a) -> AsyncT m a
  onAnyException mx f=  mx `onBack` f

-- | Delete all the exception handlers registered till now.
cutExceptions :: (Monad m) => AsyncT m ()
cutExceptions= backCut (undefined :: SomeException)

-- | Used inside an exception handler. Stop executing any further exception
-- handlers and resume normal execution from this point on.
--
continue :: Monad m  => AsyncT m ()
continue = forward (undefined :: SomeException)


catcht mx exc=  sandbox $ do
         cutExceptions
         onException' mx exc
   where
   sandbox mx= do
     exState <- getState <|> backStateOf (undefined :: SomeException)
     mx
       <*** setState exState




