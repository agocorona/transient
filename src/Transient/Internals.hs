{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE PartialTypeSignatures     #-}
-----------------------------------------------------------------------------
--
-- Module      :  Internals
-- Copyright   :  (c) 2014-2016 Alberto G. Corona
-- License     :  MIT-style (see the file LICENSE)
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | See http://github.com/transient-haskell/transient
-- Everything in this module is exported in order to allow extensibility.
-----------------------------------------------------------------------------
-- show
module Transient.Internals where
-- /show

import           Control.Applicative
import           Control.Monad.State
import           Control.Arrow (first)
import           Data.Dynamic
import qualified Data.Map               as M
import           Data.Monoid
import           Debug.Trace
import           System.IO.Unsafe
import           Unsafe.Coerce
import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.STM
import           System.Mem.StableName
import           Data.Maybe
import           GHC.Conc
import           Data.List
import           Data.IORef
import           System.Environment
import           System.IO (hFlush,stdout)
import           System.Exit
import           Transient.Utils

type StateIO = StateT EventF IO
type TransientM a = StateIO (Maybe a)

newtype TransIO x = Transient {runTrans :: TransientM x}

type SData = ()

type EventId = Int

data EventF = forall a b.
  EventF { meffects   :: Effects
         , event      :: Maybe SData
         , xcomp      :: TransIO a
         , fcomp      :: [b -> TransIO b]
         , mfData     :: M.Map TypeRep SData
         , mfSequence :: Int
         , threadId   :: ThreadId
         , freeTh     :: Bool
         , parent     :: Maybe EventF
         , children   :: TVar [EventF]
         , maxThread  :: Maybe (IORef Int) }
         deriving Typeable

type Effects = forall a b c.
  TransIO a -> TransIO a -> (a -> TransIO b)
  -> StateIO (TransientM c -> TransientM c, Maybe a)

instance MonadState EventF TransIO where
  get  = Transient $ get >>= return . Just
  put x= Transient $ put x >> return (Just ())
  state f =  Transient $ do
      s <- get
      let ~(a, s') = f s
      put s'
      return $ Just a

-- | Run the transient computation with blank state.
runTransient :: TransIO x -> IO (Maybe x, EventF)
runTransient t = do
  th <- myThreadId
  let eventf0 =
        EventF { meffects   = baseEffects
               , event      = Nothing
               , xcomp      = empty
               , fcomp      = []
               , mfData     = M.empty
               , mfSequence = 0
               , threadId   = th
               , freeTh     = False
               , parent     = Nothing
               , children   = unsafePerformIO (newTVarIO [])
               , maxThread  = Nothing }

  runStateT (runTrans t) eventf0

-- | Run the transient computation with a given state.
runTransState st x = runStateT (runTrans x) st

-- | Get the continuation context: closure, continuation, state, child threads, etc.
getCont :: TransIO EventF
getCont = Transient $ Just <$> get

-- | Run the closure and the continuation using the state data of the calling thread.
runCont :: EventF -> TransientM a
runCont EventF { xcomp = x, fcomp = fs } = runTrans $ do
  r <- unsafeCoerce x
  compose fs r

{-
runCont cont= do
     mr <- runClosure cont
     case mr of
         Nothing -> return Nothing
         Just r -> runContinuation cont r
-}

-- | Run the closure and the continuation using given state data.
runCont' :: EventF -> IO (Maybe a, EventF)
runCont' cont = runStateT (runCont cont) cont

-- | Warning: radioactive untyped stuff. handle with care
getContinuations :: StateIO [a -> TransIO b]
getContinuations= do
  EventF { fcomp = fs } <- get
  return $ unsafeCoerce fs

-- | Compose a list of continuations.
-- compose :: [a -> TransIO a] -> (a -> TransIO a)
compose []     = const empty
compose (f:fs) = \x -> f x >>= compose fs

-- | Run the closure (the 'x'  in 'x >>= f') of the current bind operation.
runClosure :: EventF -> StateIO (Maybe a)
runClosure EventF { xcomp = x } = unsafeCoerce $ runTrans x

-- | Run the continuation (the 'f' in 'x >>= f') of the current bind operation
runContinuation :: EventF -> a -> StateIO (Maybe b)
runContinuation EventF { fcomp = fs } = runTrans . (unsafeCoerce $ compose fs)

-- | Set the continuation and cons the second argument to the list of continuations.
setContinuation :: TransIO a -> (a -> TransIO b) -> [c -> TransIO c] -> StateIO ()
setContinuation b c fs =
  modify $ \EventF { fcomp = fs, .. } ->
            EventF { xcomp = b, fcomp = unsafeCoerce c: fs, .. }

withContinuation c mx = do
  EventF { fcomp = fs, .. } <- get
  put $ EventF { xcomp = mx, fcomp = unsafeCoerce c:fs, .. }
  r <- mx
  restoreStack fs
  return r

-- | Run a chain of continuations. It is up to the programmer to assure by construction that
--  each continuation type-check with the next, that the parameter type match the input of the first
-- continuation.
-- Normally this makes sense if it stop the current flow with `stop` after the invocation
runContinuations :: [a -> TransIO b] -> c -> TransIO d
runContinuations fs x = (compose $ unsafeCoerce fs) x

instance Functor TransIO where
  --   Transient $ fmap (fmap f) $ runTrans mx
  fmap f mx = do
    x <- mx
    return $ f x

instance Applicative TransIO where
  pure a  = Transient . return $ Just a

  f <*> g = Transient $ do
         rf <- liftIO $ newIORef (Nothing,[])
         rg <- liftIO $ newIORef (Nothing,[])   -- !> "NEWIOREF"

         fs  <- getContinuations

         let

             hasWait (_:Wait:_)= True
             hasWait _ = False

             appf k = Transient $  do
                   Log rec _ full <- getData `onNothing` return (Log False [] [])
                   (liftIO $ writeIORef rf  (Just k,full))
--                                !> ( show $ unsafePerformIO myThreadId) ++"APPF"
                   (x, full2)<- liftIO $ readIORef rg
                   when (hasWait  full ) $
                       -- !> (hasWait full,"full",full, "\nfull2",full2)) $
                        let full'= head full: full2
                        in (setData $ Log rec full' full')     -- !> ("result1",full')

                   return $ Just k <*> x

             appg x = Transient $  do
                   Log rec _ full <- getData `onNothing` return (Log False [] [])
                   liftIO $ writeIORef rg (Just x, full)
--                      !> ( show $ unsafePerformIO myThreadId)++ "APPG"
                   (k,full1) <- liftIO $ readIORef rf
                   when (hasWait  full) $
                       -- !> ("full", full, "\nfull1",full1)) $
                        let full'= head full: full1
                        in (setData $ Log rec full' full')   -- !> ("result2",full')

                   return $ k <*> Just x

         setContinuation f appf fs


         k <- runTrans f
                  -- !> ( show $ unsafePerformIO myThreadId)++ "RUN f"
         was <- getData `onNothing` return NoRemote
         when (was == WasParallel) $  setData NoRemote

         Log recovery _ full <- getData `onNothing` return (Log False [] [])



         if was== WasRemote  || (not recovery && was == NoRemote  && isNothing k )
--               !>  ("was,recovery,isNothing=",was,recovery, isNothing k)
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
                    --  !> ( show $ unsafePerformIO myThreadId) ++ "RUN g"
             Log recovery _ full' <- getData `onNothing` return (Log False [] [])
             liftIO $ writeIORef rg  (x,full')
             restoreStack fs
             k'' <- if was== WasParallel
                      then do
                        (k',_) <- liftIO $ readIORef rf -- since k may have been updated by a parallel f
                        return k'
                      else return k
             return $ k'' <*> x

-- | Reset the continuation stack.
-- restoreStack :: [a -> TransIO a] -> TransIO ()
restoreStack fs = modify $ \EventF{..} -> EventF { event = Nothing, fcomp = fs, ..}

readWithErr line=
     let [(v,left)] = readsPrec 0 line
     in (v   `seq` return [(v,left)])
                    `catch` (\(e::SomeException) ->
                      error ("read error of " ++ show( typeOf v) ++ " in: "++ "\""++line++"\""))


readsPrec' _= unsafePerformIO . readWithErr

-- | Dynamic serializable data for logging
data IDynamic =
    IDyns String
  | forall a. (Read a, Show a, Typeable a) => IDynamic a

instance Show IDynamic where
  show (IDynamic x) = show (show x)
  show (IDyns s)    = show s

instance Read IDynamic where
  readsPrec n str = map (first IDyns) $ readsPrec' n str

type Recover        = Bool
type CurrentPointer = [LogElem]
type LogEntries     = [LogElem]
data LogElem        = Wait | Exec | Var IDynamic
  deriving (Read, Show)
data Log            = Log Recover CurrentPointer LogEntries
  deriving Typeable
data RemoteStatus   = WasRemote | WasParallel | NoRemote
  deriving (Typeable, Eq, Show)

instance Alternative TransIO where
  empty = Transient $ return Nothing
  (<|>) = mplus

instance MonadPlus TransIO where
  mzero     = empty
  mplus x y = Transient $ do
    mx  <- runTrans x                -- !!> "RUNTRANS11111"
    was <- getData `onNothing` return NoRemote
    if was == WasRemote              -- !> was
      then return Nothing
      else case mx of
            Nothing -> runTrans y      --  !!> "RUNTRANS22222"
            justx -> return justx

-- | A synonym of empty that can be used in a monadic expression. It stops the
-- computation and executes the next alternative computation (composed with `<|>`).
stop :: Alternative m => m stopped
stop = empty

infixr 1  <***  ,  <**, **>

class AdditionalOperators m where
  -- | Executes the second operand even if the first returns empty.
  -- A normal imperative (monadic) sequence uses the operator (>>) which in the
  -- Transient monad does not execute the next operand if the previous one
  -- returns empty.
  -- Returns the second result.
  (**>) :: m a -> m b -> m b

  -- | Forces the execution of the second operand even if the first stops.
  -- It does not execute the second operand as result of internal events
  -- occurring in the first operand.
  -- Returns the first result.
  (<**) :: m a -> m b -> m a


  -- | Forces the execution of the second operand even if the first stops.
  -- The second operand is executed even when internal events happen in the
  -- first operand and it returns something.
  -- Returns the first result.
  (<***) :: m a -> m b -> m a


-- | Named version of (**>)
atEnd' :: AdditionalOperators m => m a -> m b -> m a
atEnd' = (<**)

-- | Named version of (<***)
atEnd :: AdditionalOperators m => m a -> m b -> m a
atEnd = (<***)

instance AdditionalOperators TransIO where
--(**>) :: TransIO a -> TransIO b -> TransIO b
  (**>) x y = Transient $ do
    runTrans x
    runTrans y

--(<***) :: TransIO a -> TransIO b -> TransIO a
  (<***) ma mb = Transient $ do
    fs <- getContinuations
    setContinuation ma (\x -> mb >> return x) fs
    a <- runTrans ma
    runTrans mb
    restoreStack fs
    return a

--(<**) :: TransIO a -> TransIO b -> TransIO a
  (<**) ma mb = Transient $ do
    a <- runTrans ma    -- !> "ma"
    runTrans  mb        -- !> "mb"
    return a

-- | When the first operand is an asynchronous operation, the second operand is
-- executed exactly once, when the first completes its first asynchronous
-- operation.
--
-- This is useful for spawning asynchronous or distributed tasks that are
-- singletons and that should start when the first one is set up.
--
-- For example, a streaming where the event receivers are activated before the
-- senders.
(<|) :: TransIO a -> TransIO b -> TransIO a
(<|) ma mb = Transient $ do
  fs  <- getContinuations
  ref <- liftIO $ newIORef False
  setContinuation ma (cont ref )  fs
  r <- runTrans ma
  restoreStack fs
  return r
  where cont ref x = Transient $ do
          n <- liftIO $ readIORef ref
          if n == True
          then return $ Just x
          else do liftIO $ writeIORef ref True
                  runTrans mb
                  return $ Just x

instance Monoid a => Monoid (TransIO a) where
  mappend x y = mappend <$> x <*> y
  mempty      = return mempty

-- | Set the current closure and continuation for the current statement.
setEventCont :: TransIO a -> (a -> TransIO b) -> StateIO EventF
setEventCont x f = do
  EventF { fcomp = fs, .. } <- get  -- !> "SET"
  let cont = EventF { xcomp = x, fcomp = unsafeCoerce f:fs, .. }
  put cont
  return cont

-- | Reset the closure and continuation. Remove inner binds than the
-- previous computations may have stacked in the list of continuations.
--resetEventCont :: Maybe a -> EventF -> StateIO (TransIO b -> TransIO b)
resetEventCont mx _ = do
  EventF { fcomp = fs, .. } <- get     -- !> "reset"
  let f = \mx -> case mx of
        Nothing -> empty
        Just x  -> (unsafeCoerce $ head fs) x
  put $ EventF { xcomp = f mx, fcomp = safeTail fs, .. }
  return id

--refEventCont= unsafePerformIO $ newIORef baseEffects

-- | Default Effects value.
{-# INLINE baseEffects #-}
baseEffects :: Effects
baseEffects x x' f' = do
  c  <- setEventCont x' f'
  mk <- runTrans x
  t  <- resetEventCont mk c
  return (t,mk)

instance Monad TransIO where
  return  = pure
  x >>= f = Transient $ do
  --            effects <- gets effects -- liftIO $ readIORef refEventCont
    (t,mk) <- baseEffects x x f
    t $ case mk of
      Just k  ->  runTrans (f k)
      Nothing ->  return Nothing

--instance MonadTrans (Transient ) where
--  lift mx = Transient $ mx >>= return . Just

instance MonadIO TransIO where
  liftIO x = Transient $ liftIO x >>= return . Just
  --     let x= liftIO io in x `seq` lift x

-- * Threads

waitQSemB :: IORef Int -> IO Bool
waitQSemB sem = atomicModifyIORef sem $ \n ->
  if n > 0
  then (n - 1, True)
  else (n,    False)

signalQSemB :: IORef Int -> IO ()
signalQSemB sem = atomicModifyIORef sem $ \n -> (n + 1, ())

-- | Set the maximum number of threads for a procedure. It is useful to limit the
-- parallelization of transient code that uses `parallel`, `spawn`, and `waitEvents`.
threads :: Int -> TransIO a -> TransIO a
threads n procedure = Transient $ do
  msem <- gets maxThread
  sem  <- liftIO $ newIORef n
  modify $ \s -> s { maxThread = Just sem }
  r    <- runTrans procedure
  modify $ \s -> s { maxThread = msem } -- restore it
  return r

-- | Delete all the previous children generated by the expression taken as
-- parameter and continue execution of the current thread.
oneThread :: TransIO a -> TransIO a
oneThread comp = do
  chs <- liftIO $ newTVarIO []
  r   <- comp
  modify $ \ s -> s { children = chs }
  killChilds
  return r

-- | Display the active threads in a tree-like structure, highlighting the
-- parent-child relationships between the threads.
showThreads :: TransIO empty
showThreads = do
  st' <- gets (fromJust . parent)
  liftIO $ showTree 0 st'
  stop
  where toplevel st =
          case parent st of
            Nothing -> st
            Just p  -> toplevel p

        showThreads' n rchs = do
          chs <- atomically $ readTVar rchs
          mapM_ (showTree n) chs

        showTree n ch = do
          putStr $ take n $ repeat ' '
          print $ threadId ch
          showThreads' (n + 4) $ children ch

-- | Add n threads to the limit of threads. If there is no limit, set it.
addThreads' :: Int -> TransIO ()
addThreads' n = Transient $ do
  msem <- gets maxThread
  case msem of
    Just sem -> liftIO $ modifyIORef sem $ \n' -> n + n'
    Nothing  -> do
      sem <- liftIO (newIORef n)
      modify $ \ s -> s { maxThread = Just sem }
  return $ Just ()

-- | Ensure that at least n threads are available.
ensureThreads :: Int -> TransIO ()
ensureThreads n = Transient $ do
  msem <- gets maxThread
  case msem of
    Nothing  -> return ()
    Just sem -> liftIO $ modifyIORef sem $ \n' -> if n' > n then n' else n
  return $ Just ()

{-# DEPRECATED addThreads "Use `ensureThreads` instead." #-}
addThreads :: Int -> TransIO ()
addThreads = ensureThreads

--getNonUsedThreads :: TransIO (Maybe Int)
--getNonUsedThreads= Transient $ do
--   msem <- gets maxThread
--   case msem of
--    Just sem -> liftIO $ Just <$> readIORef sem
--    Nothing -> return Nothing

setThreadImmunity :: Bool -> TransIO a -> TransIO a
setThreadImmunity immune process = Transient $ do
  st <- get
  put st { freeTh = immune }
  r  <- runTrans process
  modify $ \s -> s { freeTh = freeTh st }
  return r

-- | The threads generated in the given process will be immune to the `kill*`
-- primitives.
freeThreads :: TransIO a -> TransIO a
freeThreads process = setThreadImmunity True process

-- | The threads will be killed when the parent thread dies. That is the default.
-- This can be invoked to revert the effect of `freeThreads`
hookedThreads :: TransIO a -> TransIO a
hookedThreads process = setThreadImmunity False process

-- | Kill all the child threads of the current thread.
killChilds :: TransIO ()
killChilds = Transient $ do
  cont <- get
  liftIO $ killChildren $ children cont
  return $ Just ()

-- * extensible state: session data management

-- | Get the state data for the desired type if there is any.
getData :: (MonadState EventF m, Typeable a) => m (Maybe a)
getData = resp
  where resp = do
          dataMap <- gets mfData
          case M.lookup (typeOf $ typeResp resp) dataMap  of
            Just x  -> return . Just $ unsafeCoerce x
            Nothing -> return Nothing

        typeResp :: m (Maybe x) -> x
        typeResp = undefined

-- | getData specialized for the Transient monad. If Nothing, the monadic
-- computation does not continue.
--
-- If there is no such data, `getSData` silently stops the computation.
-- If you wish to recover from the failure, use this:
--
-- > getSData <|> error "no data"
--
-- To have the same semantics and guarantees as `get`, use a default value:
--
-- > getInt = getSData <|> return (0 :: Int)
--
-- The default value (0 in this case) has the same role as the initial value in a
-- state monad. The difference is that you can define as many `get*` as you need
-- for all your data types.
--
-- To distingish two data with the same types, use newtype definitions.
getSData :: Typeable a => TransIO a
getSData = Transient getData

-- | Set session data for the given type. This data can later be retrieved with
-- getData or getSData.
-- Note that this is data in a state monad, which means that the update only
-- affects the downstream monad execution. It is not a global, per-user, or
-- per-thread state. It is a monadic state like the one of a state monad.
setData :: (MonadState EventF m, Typeable a) => a -> m ()
setData x =
  modify $ \s -> s { mfData = M.insert (typeOf x) (unsafeCoerce x) (mfData s) }

-- | Remove session data for the given type.
delData :: (MonadState EventF m, Typeable a) => a -> m ()
delData x = modify $ \s -> s { mfData = M.delete (typeOf x) (mfData s) }

--withSData ::  ( MonadState EventF m,Typeable a) => (Maybe a -> a) -> m ()
--withSData f= modify $ \st -> st{mfData=
--    let dat = mfData st
--        mx= M.lookup typeofx dat
--        mx'= case mx of Nothing -> Nothing; Just x -> unsafeCoerce x
--        fx=  f mx'
--        typeofx= typeOf $ typeoff f
--    in  M.insert typeofx  (unsafeCoerce fx) dat}
--    where
--    typeoff :: (Maybe a -> a) -> a
--    typeoff = undefined
----

-- | Generator of identifiers that are unique within the current monadic sequence.
-- They are not unique in the whole program.
genId :: MonadState EventF m => m Int
genId = do
  s <- get
  let n = mfSequence s
  put s { mfSequence = n + 1 }
  return n

-- | Get the next unique identifier that will be generated.
getPrevId :: MonadState EventF m => m Int
getPrevId = gets mfSequence

instance Read SomeException where
  readsPrec n str = [(SomeException $ ErrorCall s, r)]
    where [(s , r)] = read str

-- | async calls

data StreamData a = SMore a | SLast a | SDone | SError SomeException
  deriving (Typeable, Show, Read)

-- | Variant of `parallel` that repeatedly executes the IO computation and kills
-- the previously created children.
--
-- It is useful in single-threaded problems where each event discards the
-- computations spawned by previous events.
waitEvents :: IO b -> TransIO b
waitEvents = waitEvents'

-- | Multi-threaded version of `waitEvents` that doesn't kill the computations
-- spawned by the previous events.
waitEvents' :: IO b -> TransIO b
waitEvents' io = do
  mr <- parallel (SMore <$> io)
  case mr of
    SMore x  -> return x
    SError e -> throw e

-- | Variant of `parallel` that executes the IO computation once, and kills the
-- previous children threads.
async :: IO b -> TransIO b
async io = do
  mr <- parallel (SLast <$> io)
  case mr of
    SLast x  -> return x
    SError e -> throw e

-- | Variant of `waitEvents` that spawns free threads. It is a little faster at
-- the cost of no thread control.
spawn :: IO b -> TransIO b
spawn io = freeThreads (waitEvents' io)

-- | Executes an IO action periodically at the given interval and returns its
-- value if it changes.
sample :: Eq a => IO a -> Int -> TransIO a
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

-- | Return empty to the current thread, and execute the IO action
-- in the new thread.
-- This IO action modifies an internal buffer and executes the closure where
-- `parallel` is located.
-- In this new execution, since the buffer is filled, `parallel` returns
-- the contents of this buffer.
-- Then it launches the continuation afterward with this new value returned by the
-- closure.
--
-- If the maximum number of threads (set with `threads`) has been reached
-- `parallel` performs the work sequentially in the current thread.
-- So `parallel` means that 'it can be parallelized if there are any threads
-- available.'
--
-- If there is a limit on the number of threads, the thread counter is increased
-- when the thread finishes, so that future calls to `parallel` can make use of it.
-- The behaviour of `parallel` depends on `StreamData`:
-- - If `SMore`, `parallel` will execute the IO action again.
-- - If `SLast`, `SDone`, or `SError`, `parallel` will finish execution.
parallel :: IO (StreamData b) -> TransIO (StreamData b)
parallel action = Transient $ do
  cont <- get                    -- !> "PARALLEL"
  case event cont of
    j@(Just _) -> do
      put cont { event = Nothing }
      return $ unsafeCoerce j
    Nothing -> do
      liftIO $ loop cont action
      was <- getData `onNothing` return NoRemote
      when (was /= WasRemote) $ setData WasParallel
      return Nothing

-- | Executes the IO action and then the continuation in the first parameter.
loop :: EventF -> IO (StreamData t) -> IO ()
loop cont' recursive = do
  chs <- liftIO $ newTVarIO []
  let cont = cont' { parent = Just cont', children = chs }
      iocont dat = do
        runStateT (runCont cont) cont { event = Just $ unsafeCoerce dat }
        return ()

      -- Execute the IO computation and then the closure continuation
      loop' = forkMaybe False cont $ do
        mdat <- threadDelay 0 -- Why?
             >> recursive `catch` \(e :: SomeException) -> return $ SError e
        case mdat of
          more@(SMore _) -> do
              forkMaybe False cont $ iocont more
              loop'
          sdata -> iocont sdata
  loop'
  return ()
  where forkMaybe True  cont process = forkMaybe' True cont process
        forkMaybe False cont process = do
          dofork <- case maxThread cont of
                      Nothing -> return True
                      Just sem -> waitQSemB sem
          forkMaybe' dofork cont process

        forkMaybe' dofork cont process =
          if dofork
          then do
            forkFinally1
              (do th <- myThreadId
                  hangThread cont' cont { threadId = th }
                  -- !>  "thread created: "++ show th
                  process)
              (handleException cont)
            return ()
          else process  -- !> "NO THREAD"

        handleException cont me = do
          case me of -- !> "THREAD END" of
            Left e -> do
              when (fromException e /= Just ThreadKilled) $ liftIO $ print e
              killChildren $ children cont
              -- !> "KILL RECEIVED" ++ (show $ unsafePerformIO myThreadId)
            Right _ ->
              when (not $ freeTh cont') $ do
                  -- if was not a free thread
                  --  if parent is alive
                  --  then remove himself from the parent list (with free)
                  --  and pass his active children to his parent
                -- TODO: Incomplete implemention. Finish this.
                th <- myThreadId
                mparent <- free th cont
                return ()
                -- pass the active children to the parent
                -- case mparent of
                --   Nothing  ->  return()
                --   Just parent -> atomically $ do
                --         chs' <- readTVar $ children cont
                --         chs  <- (readTVar $ children parent)
                --         writeTVar (children parent)$ chs ++ chs'
                --         return ()

          case maxThread cont of
            Just sem -> signalQSemB sem  --   !> "freed thread"
            Nothing -> return ()

-- | Fork a thread taking into account asynchronous & synchronous exceptions.
forkFinally1 :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally1 action andThen =
  mask $ \restore -> forkIO $ try (restore action) >>= andThen

-- | Search for the given ThreadId in the continuation context hierarchy and remove -- it from the children list of an ancestor if possible.
free :: ThreadId -> EventF -> IO (Maybe EventF)
free th env =
  maybe (return Nothing)
        (\par -> do
          let sibling = children par
          found <- atomically $ do
            sbs <- readTVar sibling
            let (sbs', found) = drop [] th sbs
            -- !!> "search "++show th ++ " in " ++ show (map threadId sbs)
            when found $ writeTVar sibling sbs'
            -- !> ("new list",map threadId sbs')
            return found
          if not found
          then free th par         -- !!> "toparent"
          else return $ Just env)
        (parent env)
   where drop processed th []  = (processed, False)
         drop processed th (ev:evts)
           | th == threadId ev = (processed ++ evts, True)
           -- WARNING: The order of the siblings is not preserved.
           -- This shouldn't matter, but if it does, look here.
           | otherwise         = drop (ev:processed) th evts

-- | If the parent is a not a free thread, add a child context to the parent context
-- atomically.
hangThread :: EventF -> EventF -> IO ()
hangThread parent child =
  when (not $ freeTh parent) $ do
    let headpths = children parent
    atomically $ do
      ths <- readTVar headpths
      writeTVar headpths $ child:ths
    -- !!>  "thread added: "++ show (threadId child)

-- | Kill all the child threads associated with the continuation context and reset
-- the children for the continuation context.
killChildren :: TVar [EventF] -> IO ()
killChildren childs  = do
--     forkIO $ do
  ths <- atomically $ do
      ths <- readTVar childs
      writeTVar childs []
      return ths
--        mapM_ killChildren ths       -- recursive not needed, event handlers do it
  mapM_ (killThread . threadId) ths
  -- !!> ("KILLEVENT " ++ show (map threadId ths) ++
  -- if length ths <20 then ""
  --   else error "long list of threads" )
--     return ()

type EventSetter eventdata response = (eventdata -> IO response) -> IO ()
type ToReturn response = IO response

-- | De-invert an event handler.
--
-- The first parameter is the setter of the event handler to be de-inverted.
-- Usually, it's the primitive provided by a framework to set an event handler.
--
-- The second parameter is the value to return to the event handler. Usually it is `return ()`.
--
-- It configures the event handler by calling the setter of the event handler with
-- the current continuation.
react
  :: Typeable eventdata
  => EventSetter eventdata response
  -> ToReturn  response
  -> TransIO eventdata
react setHandler iob = Transient $ do
  cont <- get
  case event cont of
    Nothing -> do
      liftIO $ setHandler $ \dat -> do
        runStateT (runCont cont) cont { event = Just $ unsafeCoerce dat }
        iob
      was <- getData `onNothing` return NoRemote
      when (was /= WasRemote) $ setData WasParallel
      return Nothing

    j@(Just _) -> do
      put cont { event = Nothing }
      return $ unsafeCoerce j

--          Just dat -> do
--             delData dat
--             return (Just  dat)


--    case event cont of
--     Nothing -> do
--        liftIO $ loop cont ioaction
--        was <- getData `onNothing` return NoRemote
--        when (was /= WasRemote) $ setData WasParallel
--
--        return Nothing
--     j@(Just _) -> do
--        put cont{event=Nothing}
--        return $ unsafeCoerce j

-- * Non-blocking Keyboard Input

getLineRef :: TVar (Maybe a)
getLineRef = unsafePerformIO $ newTVarIO Nothing
roption    :: MVar [a]
roption    = unsafePerformIO $ newMVar []

-- | Install a event receiver that waits for a string and triggers the continuation
-- when this string arrives.
option :: (Typeable b, Show b, Read b, Eq b) =>
     b -> String -> TransIO b
option ret message = do
  let sret = show ret
  liftIO $ putStrLn $ "Enter  " ++ sret ++ "\tto: " ++ message
  liftIO $ modifyMVar_ roption $ \msgs -> return (sret:msgs)
  waitEvents $ getLine' (== ret)
  liftIO $ putStrLn $ show ret ++ " chosen"
  return ret

-- | Validates an input entered in the keyboard in non-blocking mode. Non-blocking
-- means that the user can enter also anything else to activate the other option
-- unlike `option`, which watches continuously. Input only waits for one valid
-- response.
input :: (Typeable a, Read a,Show a) => (a -> Bool) -> String -> TransIO a
input cond prompt = Transient . liftIO $ do
  putStr prompt >> hFlush stdout
  atomically $ do
    mr <- readTVar getLineRef
    case mr of
      Nothing -> retry
      Just r  ->
        case reads1 r of
          (s, _):_ ->
            if cond s  --  !> show (cond s)
            then do
              unsafeIOToSTM $ print s
              writeTVar getLineRef Nothing -- !>"match"
              return $ Just s
            else return Nothing
          _ -> return Nothing

-- | Non-blocking `getLine` with a validator.
getLine' :: (Read a, Typeable a) => (a -> Bool) -> IO a
getLine' cond = do
  atomically $ do
    mr <- readTVar getLineRef
    case mr of
      Nothing -> retry
      Just r  ->
        case reads1 r of --  !> ("received " ++  show r ++ show (unsafePerformIO myThreadId)) of
          (s, _):_ ->
            if cond s -- !> show (cond s)
            then do
              writeTVar getLineRef Nothing -- !>"match"
              return s
            else retry
          _ -> retry

reads1 s = x
  where x = if typeOf (typeOfr x) == typeOf ""
            then unsafeCoerce [(s, "")]
            else readsPrec' 0 s
        typeOfr :: [(a, String)] -> a
        typeOfr = undefined

inputLoop :: IO a
inputLoop = inputLoop'  -- !> "started inputLoop"
--    putStrLn "Press end to exit"
  where inputLoop' = do
          r <- getLine
          processLine r
          inputLoop'

processLine :: String -> IO ()
processLine r = do
--   when (r=="end") $ atomically $ writeTVar rexit ()
  let rs = breakSlash [] r
  forM_ rs $ \r -> do
 -- if (r=="end") then exit' $ Left "terminated by user" else
    threadDelay 100000
    atomically . writeTVar getLineRef $ Just r
  where breakSlash :: [String] -> String -> [String]
        breakSlash [] "" = [""]
        breakSlash s  "" = s
        breakSlash res ('\"':s) = breakSlash (res ++ [r]) $ safeTail rest
          where (r, rest) = span (/= '\"') s
        breakSlash res s = breakSlash (res ++ [r]) $ safeTail rest
          where (r, rest) = span (/= '/') s

-- | Wait for the execution of `exit` and return the result.
-- stay :: MVar (Either String (Maybe a)) -> IO a
stay rexit = do
  mr <- takeMVar rexit
  case mr of
    Right Nothing  -> stay rexit
    Right (Just r) -> return r
    Left msg       -> putStrLn msg >> exitWith ExitSuccess

-- | Keep the main thread running, initiate the non-blocking keyboard input and
-- execute the transient computation.
--
-- It also reads a slash-separated list of string that are read by `option` and
-- `input` as if they were entered by the keyboard.
--
-- >  foo  -p  options/to/be/read/by/option/and/input

newtype Exit a = Exit a deriving Typeable

keep :: Typeable a => TransIO a -> IO a
keep mx = do
  rexit <- newEmptyMVar
  forkIO $ do
    liftIO $ putMVar rexit $ Right Nothing
    runTransient $ do
      setData $ Exit rexit
      async inputLoop
       <|> do mx  -- ; liftIO (putMVar rexit  $ Right Nothing)
                  -- to avoid "takeMVar blocked in a infinite loop" error
       <|> do option "end" "exit"
              killChilds
              exit' (Left "terminated by user" `asTypeOf` (type1 mx))
    return ()
  threadDelay 10000
  execCommandLine
  stay rexit
  where type1 :: TransIO a -> Either String (Maybe a)
        type1 = undefined

-- | Same as `keep` but doesn't initiate the asynchronous keyboard input.
-- Useful for debugging or for creating background tasks.
keep' :: Typeable a => TransIO a -> IO a
keep' mx = do
  rexit <- newEmptyMVar
  forkIO $ do
    runTransient $ do
      setData $ Exit rexit
      mx
      liftIO $ putMVar rexit $ Right Nothing
      -- to avoid takeMVar in a infinite loop
    return ()
  threadDelay 10000
  execCommandLine
  stay rexit

execCommandLine = do
  args <- getArgs
  let mindex = findIndex (\o -> o == "-p" || o == "--path" ) args
  when (isJust mindex) $ do
    let i = fromJust mindex + 1
    when (length args >= i) $ do
      let path = args !! i
      putStr "Executing: " >> print path
      processLine path

-- | Force the finalization of the main thread and thus, all the Transient blocks
-- (and the application if there is no more code).
exit :: Typeable a => a -> TransIO a
exit x = do
  Exit rexit <- getSData <|> error "exit: not the type expected" `asTypeOf` type1 x
  liftIO $ putMVar rexit . Right $ Just x
  stop
  where type1 :: a -> TransIO (Exit (MVar (Either String (Maybe a))))
        type1 = undefined

exit' :: Typeable a => Either String (Maybe a) -> TransIO a
exit' x = do
  Exit rexit <- getSData <|> error "exit: not type expected"
  liftIO $ putMVar rexit x
  stop

-- | Alternative operator for Maybe values. Used in infix mode.
onNothing :: Monad m => m (Maybe b) -> m b -> m b
onNothing iox iox' = do
  mx <- iox
  case mx of
    Just x  -> return x
    Nothing -> iox'

-- | Function used for debugging.
-- | (!>) = flip traceShow
{-# INLINE (!>) #-}
(!>) :: Show a => b -> a -> b
(!>) x y = trace (show y) x
infixr 0 !>
