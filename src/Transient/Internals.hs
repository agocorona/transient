{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Base
-- Copyright   :
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | See http://github.com/agocorona/transient
-- everithing in this module is exported in order to allow extensibility.
-----------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE Rank2Types                #-}

module Transient.Internals where



import           Control.Applicative
import           Control.Monad.State
import           Data.Dynamic
import qualified Data.Map               as M
import           Data.Monoid
import           Debug.Trace
import           System.IO.Unsafe
import           Unsafe.Coerce
import           Control.Exception
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
import           System.Exit

import qualified Data.ByteString.Char8 as BS



--{-# INLINE (!>) #-}
--(!>) :: Show a => b -> a -> b
--(!>) x y=  trace (show y) x
--infixr 0 !>





data TransIO  x = Transient  {runTrans :: StateT EventF IO (Maybe x)}
type SData= ()

type EventId= Int

type TransientIO= TransIO

data LifeCycle= Alive | Parent | Listener | Dead deriving (Eq,Show)

data EventF  = forall a b . EventF{meffects     :: ()
                                  ,event       :: Maybe SData
                                  ,xcomp       :: TransIO a
                                  ,fcomp       :: [b -> TransIO b]
                                  ,mfData      :: M.Map TypeRep SData
                                  ,mfSequence  :: Int
                                  ,threadId    :: ThreadId
                                  ,freeTh      :: Bool
                                  ,parent      :: Maybe EventF
                                  ,children    :: MVar[EventF]
                                  ,maxThread   :: Maybe (IORef Int)
                                  ,labelth     :: IORef (LifeCycle,BS.ByteString)
                                  }
                                  deriving Typeable




type Effects= forall a b c.TransIO a -> TransIO a -> (a -> TransIO b)
     -> StateIO (StateIO (Maybe c) -> StateIO (Maybe c), Maybe a)




instance MonadState EventF TransIO where
  get  = Transient $ get >>= return . Just
  put x= Transient $ put x >> return (Just ())
  state f =  Transient $ do
      s <- get
      let ~(a, s') = f s
      put s'
      return $ Just a

type StateIO= StateT EventF IO

-- | Run a "non transient" computation within the underlying state monad, so it is guaranteed
-- that the computation neither can stop neither can trigger additional events/threads
noTrans x= Transient $ x >>= return . Just

-- | Run the transient computation with a blank state
runTransient :: TransIO x -> IO (Maybe x, EventF)
runTransient t= do
  th <- myThreadId
  label <- newIORef $ (Alive,BS.pack "top")
  childs <- newMVar []
  let eventf0=  EventF () Nothing empty [] M.empty 0
          th False  Nothing  childs Nothing  label

  runStateT (runTrans t) eventf0

-- | Run the transient computation with an state
runTransState st x = runStateT (runTrans x) st

-- | Get the continuation context: closure, continuation, state, child threads etc
getCont :: TransIO EventF
getCont = Transient $ Just <$> get

-- | Run the closure and the continuation using the state data of the calling thread
runCont :: EventF -> StateIO (Maybe a)
runCont (EventF _ _ x fs _ _  _ _  _ _ _ _)= runTrans $ do
      r <- unsafeCoerce x
      compose fs r

-- | Run the closure and the continuation using his own state data
runCont' cont= runStateT (runCont cont) cont

-- | Warning: radiactive untyped stuff. handle with care
getContinuations :: StateIO [a -> TransIO b]
getContinuations= do
  EventF _ _ _ fs _ _ _ _ _ _ _ _  <- get
  return $ unsafeCoerce fs

{-
runCont cont= do
     mr <- runClosure cont
     case mr of
         Nothing -> return Nothing
         Just r -> runContinuation cont r
-}


-- | Compose a list of continuations
compose []= const empty
compose (f: fs)= \x -> f x >>= compose fs



-- | Run the closure  (the 'x'  in 'x >>= f') of the current bind operation.
runClosure :: EventF -> StateIO (Maybe a)
runClosure (EventF _ _ x _ _ _ _ _ _ _ _ _) =  unsafeCoerce $ runTrans x


-- | Run the continuation (the 'f' in 'x >>= f') of the current bind operation with the current state
runContinuation ::  EventF -> a -> StateIO (Maybe b)
runContinuation (EventF _ _ _ fs _ _ _ _  _ _ _ _) =
   runTrans . (unsafeCoerce $ compose $  fs)


setContinuation :: TransIO a -> (a -> TransIO b) -> [c -> TransIO c] -> StateIO ()
setContinuation  b c fs =  do
    (EventF eff ev _ _ d e f g h i j k) <- get
    put $ EventF eff ev b ( unsafeCoerce c: fs) d e f g h i j k

withContinuation  c mx= do
    EventF eff ev f1 fs d e f g h i j k<- get
    put $ EventF eff ev mx ( unsafeCoerce c: fs) d e f g h i j k
    r <- mx
    restoreStack fs
    return r

-- | run a chain of continuations. It is up to the programmer to assure by construction that
--  each continuation type-check with the next, that the parameter type match the input of the first
-- continuation.
-- Normally this makes sense if it stop the current flow with `stop` after the invocation
runContinuations :: [a -> TransIO b] -> c -> TransIO d
runContinuations fs x= (compose $ unsafeCoerce fs)  x

instance   Functor TransIO where
  fmap f mx=  --   Transient $ fmap (fmap f) $ runTrans mx
    do
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
--                      !> ( show $ unsafePerformIO myThreadId) ++ "RUN g"
             Log recovery _ full' <- getData `onNothing` return (Log False [] [])
             liftIO $ writeIORef rg  (x,full')
             restoreStack fs
             k'' <- if was== WasParallel
                      then do
                        (k',_) <- liftIO $ readIORef rf -- since k may have been updated by a parallel f
                        return k'
                      else return k
             return $ k'' <*> x

restoreStack fs=
       modify $ \(EventF eff _ f _ a b c d parent children g1 la) ->
               EventF eff Nothing f fs a b c d parent children g1 la

readWithErr line=
     let [(v,left)] = readsPrec 0 line
     in (v   `seq` return [(v,left)])
                    `catch` (\(e::SomeException) ->
                      error ("read error trying to read type: \"" ++ show( typeOf v) ++ "\" in:  "++" <"++ show line++"> "))


readsPrec' _= unsafePerformIO . readWithErr


class (Show a, Read a, Typeable a) => Loggable a where


instance (Show a, Read a,Typeable a) => Loggable a where

-- | Dynamic serializable data for logging
data IDynamic= IDyns String | forall a.Loggable a => IDynamic a

instance Show IDynamic where
  show (IDynamic x)= show $ show x
  show (IDyns s)= show s

instance Read IDynamic where
  readsPrec n str= map (\(x,s) -> (IDyns x,s)) $ readsPrec' n str


type Recover= Bool
type CurrentPointer= [LogElem]
type LogEntries= [LogElem]
data LogElem=   Wait | Exec | Var IDynamic deriving (Read,Show)
data Log= Log Recover  CurrentPointer LogEntries deriving (Typeable, Show)


instance Alternative TransIO where
    empty = Transient $ return  Nothing
    (<|>) = mplus


data RemoteStatus=   WasRemote | WasParallel | NoRemote deriving (Typeable, Eq, Show)

instance MonadPlus TransIO where
    mzero= empty
    mplus  x y=  Transient $ do
         mx <- runTrans x                       --  !> "RUNTRANS11111"
         was <- getData `onNothing` return NoRemote
         if was== WasRemote                     -- !> was
           then return Nothing
           else
                 case mx of
                     Nothing -> runTrans y       -- !> "RUNTRANS22222"
                     justx -> return justx




-- | A sinonym of empty that can be used in a monadic expression. it stop the
-- computation and execute the next alternative computation (composed with `<|>`)
stop :: Alternative m => m stopped
stop= empty


--instance (Num a,Eq a,Fractional a) =>Fractional (TransIO a)where
--     mf / mg = (/) <$> mf <*> mg
--     fromRational (x:%y) =  fromInteger x % fromInteger y


instance (Num a,Eq a) => Num (TransIO a) where
     fromInteger = return . fromInteger
     mf + mg = (+) <$> mf <*> mg
     mf * mg = (*) <$> mf <*> mg
     negate f = f >>= return . negate
     abs f =  f >>= return . abs
     signum f =  f >>= return . signum


class AdditionalOperators m where

    -- | Executes the second operand even if the frist return empty.
    -- A normal imperative (monadic) sequence uses the operator (>>) which in the
    -- Transient monad does not execute the next operand if the previous one return empty.
    (**>) :: m a -> m b -> m b

    -- | Forces the execution of the second operand even if the first stop. It does not execute
    -- the second operand as result of internal events occuring in the first operand.
    -- Return the first result
    (<**) :: m a -> m b -> m a

    atEnd' ::m a -> m b -> m a
    atEnd' = (<**)

    -- | Forces the execution of the second operand even if the first stop. Return the first result. The second
    -- operand is executed also when internal events happens in the first operand and it returns something
    (<***) :: m a -> m b -> m a

    atEnd :: m a -> m b -> m a
    atEnd= (<***)


instance AdditionalOperators TransIO where

--    (**>) :: TransIO a -> TransIO b -> TransIO b
    (**>) x y=  Transient $ do
              runTrans x
              runTrans y

--    (<***) :: TransIO a -> TransIO b -> TransIO a
    (<***) ma mb= Transient $ do
                  fs  <- getContinuations
                  setContinuation ma (\x -> mb >> return x)  fs
                  a <- runTrans ma
                  runTrans mb
                  restoreStack fs
                  return  a

--    (<**) :: TransIO a -> TransIO b -> TransIO a
    (<**) ma mb= Transient $ do
                  a <- runTrans ma    -- !> "ma"
                  runTrans  mb        -- !> "mb"
                  return a

infixr 1  <***  ,  <**, **>



-- | When the first operand is an asynchronous operation, the second operand is executed once (one single time)
-- when the first completes his first asyncronous operation.
--
-- This is useful for spawning asynchronous or distributed tasks that are singletons and that should start
-- when the first one is set up.
--
-- for example a streaming where the event receivers are acivated before the senders.

(<|) :: TransIO a -> TransIO b -> TransIO a
(<|)  ma mb =  Transient $ do
          fs  <- getContinuations
          ref <- liftIO $ newIORef False
          setContinuation ma (cont ref )  fs
          r <- runTrans ma
          restoreStack fs
          return  r
    where
    cont ref x= Transient $ do
          n <- liftIO $ readIORef ref
          if  n == True
            then  return $ Just x
            else do liftIO $ writeIORef ref True
                    runTrans mb
                    return $ Just x

instance Monoid a => Monoid (TransIO a) where
  mappend x y = mappend <$> x <*> y
  mempty= return mempty

-- | Set the current closure and continuation for the current statement
setEventCont ::   TransIO a -> (a -> TransIO b) -> StateIO EventF
setEventCont x f  = do

   st@(EventF eff e _ fs d n  r applic  ch rc bs la)  <- get  -- !> "SET"
   let cont=  EventF eff e x ( unsafeCoerce f : fs) d n  r applic  ch rc bs la
   put cont
   return cont

-- | Reset the closure and continuation. remove inner binds than the previous computations may have stacked
-- in the list of continuations.
--resetEventCont :: Maybe a -> EventF -> StateIO (TransIO b -> TransIO b)
resetEventCont mx _=do
   st@(EventF eff e _ fs d n  r nr  ch rc bs la)  <- get     -- !> "reset"
   let f= \mx ->  case mx of
                       Nothing -> empty
                       Just x  -> (unsafeCoerce $ head fs)  x
   put $ EventF eff e (f mx) ( tailsafe fs) d n  r nr  ch rc bs la
   return  id

tailsafe []=[]
tailsafe (x:xs)= xs

--refEventCont= unsafePerformIO $ newIORef baseEffects

{-# INLINE baseEffects #-}
baseEffects :: Effects

baseEffects x  x' f' = do
            c <-setEventCont x'  f'
            mk <- runTrans x
            t <- resetEventCont mk c
            return (t,mk)

instance Monad TransIO where

      return  = pure

      x >>= f  = Transient $ do
--            effects <- gets effects -- liftIO $ readIORef refEventCont
            (t,mk) <- baseEffects x x f
            t $ case mk of
                 Just k  ->  runTrans (f k)

                 Nothing ->  return Nothing

--instance MonadTrans (Transient ) where
--  lift mx = Transient $ mx >>= return . Just

instance MonadIO TransIO where

  liftIO mx=do
    ex <- liftIO' $ (mx >>= return . Right) `catch` (\(e :: SomeException) -> return $ Left e)
    case ex of
      Left e -> back e  -- finish $ Just e
      Right x -> return x
    where
    liftIO' x = Transient $ liftIO x >>= return . Just --     let x= liftIO io in x `seq` lift x


-- * Threads

waitQSemB sem= atomicModifyIORef sem $ \n -> if n > 0 then(n-1,True) else (n,False)
signalQSemB sem= atomicModifyIORef sem  $ \n ->  (n + 1,())

-- | Set the maximun number of threads for a procedure. It is useful to limit the
-- parallelization of transient code that uses `parallel` `spawn` and `waitEvents`
threads :: Int -> TransIO a -> TransIO a
threads n proc=  do
   msem <- gets maxThread
   sem <- liftIO $ newIORef n
   modify $ \s -> s{maxThread= Just sem}
   r <- proc <** (modify $ \s -> s{maxThread = msem}) -- restore it
   return r

-- | Delete all the previous child threads generated by the expression taken as parameter and continue execution
-- of the current thread.
oneThread :: TransIO a -> TransIO a
oneThread comp= do
   st <-  get
   chs <- liftIO $ newMVar []
   label <-  liftIO $ newIORef (Alive, BS.pack "oneThread")

   let st' = st{parent=Just st,children=   chs, labelth= label}
   liftIO $ hangThread st st'
   put st'
   x <- comp
   th<- liftIO  myThreadId                              -- !> ("FATHER:", threadId st)
   chs <- liftIO $ readMVar chs -- children st'

   liftIO $ mapM_ (killChildren1  th  )  chs
   return x
   where
   killChildren1 :: ThreadId  ->  EventF -> IO ()
   killChildren1 th state = do
       ths' <- modifyMVar (children state) $ \ths -> do
                    let (inn, ths')=  partition (\st -> threadId st == th) ths
                    return (inn, ths')


       mapM_ (killChildren1  th ) ths'
       mapM_ (killThread . threadId) ths'
--                                            !> ("KILLEVENT1 ", map threadId ths' )




-- | Add a label to the current passing threads so it can be printed by debugging calls like `showThreads`
labelState l=  do
     st <- get
     liftIO $ atomicModifyIORef (labelth st) $ \(status,_) -> ((status,BS.pack l),())

printBlock= unsafePerformIO $ newMVar ()


-- Show the tree of threads hanging from the state
showThreads :: MonadIO m => EventF -> m ()
showThreads st= liftIO $ withMVar printBlock $ const $ do

   mythread <-  myThreadId
--                                       !> "showThreads"

   putStrLn "---------Threads-----------"
   let showTree n ch=  do
         liftIO $ do
            putStr $ take n $ repeat  ' '
            (state,label) <- readIORef $ labelth ch
            if BS.null label
              then putStr . show $ threadId ch
              else do BS.putStr label ; putStr . drop 8 . show $ threadId ch
                      when (state== Dead) $ putStr " dead"
            putStrLn $ if mythread== threadId ch then  " <--" else ""

         chs <-  readMVar $ children ch
         mapM_ (showTree $ n+2) $ reverse chs

   showTree 0 st



-- | Return the state of the thread that initiated the transient computation
topState :: TransIO EventF
topState = do
      st <- get
      return $ toplevel st
  where
  toplevel st= do
      case parent st of
        Nothing ->  st
        Just p -> toplevel p

-- | Return the state variable of the type desired with which a thread, identified by his number in the treee was initiated
showState :: (Typeable a, MonadIO m, Alternative m) => String -> EventF  -> m  (Maybe a)
showState th top = resp
  where
  resp=  do
     let thstring= drop 9 . show $ threadId top
     if thstring == th  then getstate top else do
       sts <-  liftIO $ readMVar $ children top
       foldl (<|>)  empty $ map (showState th) sts
       where
       getstate st=
            case M.lookup ( typeOf $ typeResp resp ) $ mfData st of
              Just x  -> return . Just $ unsafeCoerce x
              Nothing -> return Nothing
       typeResp :: m (Maybe x) -> x
       typeResp= undefined


-- | Add n threads to the limit of threads. If there is no limit, it set it
addThreads' :: Int -> TransIO ()
addThreads' n= noTrans $ do
   msem <- gets maxThread
   case msem of
    Just sem -> liftIO $ modifyIORef sem $ \n' -> n + n'
    Nothing  -> do
        sem <- liftIO (newIORef n)
        modify $ \ s -> s{maxThread= Just sem}
   return  ()

-- | Assure that at least there are n threads available
addThreads n= noTrans $ do
   msem <- gets maxThread
   case msem of
     Nothing -> return ()
     Just sem ->  liftIO $ modifyIORef sem $ \n' -> if n' > n then n' else  n
   return  ()
--getNonUsedThreads :: TransIO (Maybe Int)
--getNonUsedThreads= Transient $ do
--   msem <- gets maxThread
--   case msem of
--    Just sem -> liftIO $ Just <$> readIORef sem
--    Nothing -> return Nothing


-- | The threads generated in the process passed as parameter will not be killed by `kill*`
--  primitives.
--
-- Since there is no thread control, the application run slightly faster.
freeThreads :: TransIO a -> TransIO a
freeThreads proc= Transient $ do
     st <- get
     put st{freeTh= True}
     r <- runTrans proc
     modify $ \s -> s{freeTh= freeTh st}
     return r

-- | The threads will be killed when the parent thread dies. That is the default.
-- This can be invoked to revert the effect of `freeThreads`
hookedThreads :: TransIO a -> TransIO a
hookedThreads proc= Transient $ do
     st <- get
     put st{freeTh= False}
     r <- runTrans proc
     modify $ \st -> st{freeTh= freeTh st}
     return r

-- | kill all the child threads of the current thread
killChilds :: TransIO()
killChilds= noTrans $  do
   cont <- get

   liftIO $ do
      killChildren $ children cont
      writeIORef (labelth cont) (Alive,mempty)     -- !> (threadId cont,"relabeled")
   return ()

-- | Kill the  current thread and the childs
killBranch= noTrans $ do
        st <- get
        liftIO $ killBranch' st

-- | Kill the childs and the thread of an state
killBranch' cont= liftIO $ do

        killChildren $ children cont
        let thisth= threadId cont
        let mparent= parent cont
        when (isJust mparent) $ modifyMVar_ (children $ fromJust mparent)
                              $ \sts  -> return $ filter (\st -> threadId st /= thisth) sts
        killThread $ thisth


-- * extensible state: session data management

-- | Get the state data for the desired type if there is any.
getData ::  (MonadState EventF m,Typeable a) =>  m (Maybe a)
getData =  resp where
 resp= gets mfData >>= \list  ->
    case M.lookup ( typeOf $ typeResp resp ) list  of
      Just x  -> return . Just $ unsafeCoerce x
      Nothing -> return Nothing
 typeResp :: m (Maybe x) -> x
 typeResp= undefined


-- | getData specialized for the Transient monad. if Nothing, the
-- monadic computation does not continue.
--
-- If there is no such data, `getSData`  silently stop the computation.
-- That may or may not be the desired behaviour.
-- To make sure that this does not get unnoticed, use this construction:
--
-- >  getSData <|> error "no data"
--
-- To have the same semantics and guarantees than `get`, use a default value:
--
-- > getInt= getSData <|> return (0 :: Int)
--
-- The default value (0 in this case) has the same role than the initial value in a state monad.
-- The difference is that you can define as many `get` as you need for all your data types.
--
-- To distingish two data with the same types, use newtype definitions.
getSData ::  Typeable a => TransIO  a
getSData= Transient getData

-- | Synonym for `getSData`
getState ::  Typeable a => TransIO  a
getState= getSData

-- | Set session data for this type. retrieved with getData or getSData
-- Note that this is data in a state monad, that means that the update only affect downstream
-- in the monad execution. it is not a global state neither a per user or per thread state
-- it is a monadic state like the one of a state monad.
setData ::  (MonadState EventF m, Typeable a) => a -> m ()
setData  x=
  let t= typeOf x in  modify $ \st -> st{mfData= M.insert  t (unsafeCoerce x) (mfData st)}


-- | Modify state data. It accept a function that get the current state (if exist) as parameter.
-- The state will be deleted or changed depending on function result
modifyData :: (MonadState EventF m, Typeable a)  => (Maybe a -> Maybe a) -> m ()
modifyData f=  modify $ \st -> st{mfData=
       let  t= typeOf $ typeResp f
       in M.alter alterf  t  (mfData st)}
   where
   typeResp :: (Maybe a -> b) -> a
   typeResp= undefined
   alterf mx =
      let x' = case mx of
                  Just x  -> Just $ unsafeCoerce x
                  Nothing -> Nothing
      in   unsafeCoerce $ f x'

-- | Synonym for modifyData
modifyState :: (MonadState EventF m, Typeable a)  => (Maybe a -> Maybe a) -> m ()
modifyState= modifyData

-- | Synonym for `setData`
setState  ::  (MonadState EventF m, Typeable a) => a -> m ()
setState= setData

delData :: ( MonadState EventF m,Typeable a) => a -> m ()
delData x=  modify $ \st -> st{mfData= M.delete (typeOf x ) (mfData st)}

delState :: ( MonadState EventF m,Typeable a) => a -> m ()
delState= delData



-- | Generator of identifiers that are unique withing the current monadic sequence
-- They are not unique in the whole program.
genId :: MonadState EventF m =>  m Int
genId= do
      st <- get
      let n= mfSequence st
      put st{mfSequence= n+1}
      return n

getPrevId :: MonadState EventF m =>  m Int
getPrevId= do
      n <- gets mfSequence
      return n

instance Read SomeException where
   readsPrec n str=
      let [(s , r)]= read str in [(SomeException $ ErrorCall s,r)]

-- | Async calls

data StreamData a=  SMore a | SLast a | SDone | SError SomeException deriving (Typeable, Show,Read)


-- | Variant of `parallel` that repeatedly executes the IO computation without end
--
waitEvents ::   IO b -> TransIO b
waitEvents io= do
   mr <- parallel (SMore <$> io)
   case mr of
     SMore x -> return x
     SError e -> back  e

-- | Variant of `parallel` that execute the IO computation once
async  ::  IO b -> TransIO b
async io= do
   mr <- parallel  (SLast <$> io)
   case mr of
     SLast x -> return x
     SError e -> back e

-- | in an alternative computation it executes an async operations synchronously.
-- This means that the alternatives do not execute until the async operation finishes.
-- Do not use in Applicatives.
sync :: TransIO a -> TransIO a
sync x= do
  setData WasRemote
  r <- x
  delData WasRemote
  return r

-- | `spawn= freeThreads . waitEvents`
spawn= freeThreads . waitEvents

-- | Executes an IO action each certain interval of time and return his value if it changes
sample :: Eq a => IO a -> Int -> TransIO a
sample action interval= do
       v <-  liftIO action
       prev <- liftIO $ newIORef v
       waitEvents (loop action prev) <|> async (return v)
       where
       loop action prev= loop'
        where
        loop'= do
            threadDelay interval
            v <- action
            v' <- readIORef prev
            if v /= v' then writeIORef prev v >> return v else  loop'


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


-- |  Return empty to the current thread and execute the IO action in a new thread.
-- When the IO action returns, the transient computation continues with this value as the result
-- The IO action may be re-executed or not depending on the result. So parallel can spawn any
-- number of threads/results.
--
-- If the maximum number of threads, set with `threads` has been reached  `parallel` perform
-- the work sequentially, in the current thread.
-- So `parallel` means that 'it can be parallelized if there are thread available'
--
-- if there is a limitation of threads, when a thread finish, the counter of threads available
-- is increased so another `parallel` can make use of it.
--
-- The behaviour of `parallel` depend on `StreamData`; If `SMore`, `parallel` will excute again the
-- IO action. With `SLast`, `SDone` and `SError`, `parallel` will not repeat the IO action anymore.
parallel  ::    IO (StreamData b) -> TransIO (StreamData b)
parallel  ioaction= Transient $ do
    cont <- get                    -- !> "PARALLEL"
    case event cont of
         j@(Just _) -> do
            put cont{event=Nothing}
            return $ unsafeCoerce j
         Nothing -> do
            liftIO $ atomicModifyIORef (labelth cont) $ \(_,lab) -> ((Parent,lab),())
            liftIO $ loop  cont ioaction
            was <- getData `onNothing` return NoRemote
            when (was /= WasRemote) $ setData WasParallel


--            th <- liftIO myThreadId
--            return () !> ("finish",th)
            return Nothing


-- Execute the IO action and the continuation
loop ::  EventF -> IO (StreamData t) -> IO ()
loop  parentc  rec  = forkMaybe parentc $ \cont ->  do

      -- execute the IO computation and then the closure-continuation
  liftIO $ atomicModifyIORef (labelth cont) $ const ((Listener,BS.pack "wait"),())
  let loop'=   do
         mdat <- rec `catch` \(e :: SomeException) -> return $ SError e
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
             dofork <- waitQSemB sem
             if dofork then  forkIt parent proc else proc parent


  forkIt parent  proc= do
     chs <- liftIO $ newMVar []

     label <- newIORef (Alive, BS.pack "work")
     let cont = parent{parent=Just parent,children=   chs, labelth= label}

     forkFinally1 (do
         th <- myThreadId
         let cont'= cont{threadId=th}
         when(not $ freeTh parent )$ hangThread parent   cont'
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


  forkFinally1 :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
  forkFinally1 action and_then =
       mask $ \restore ->  forkIO $ try (restore action) >>= and_then

  free th env= do
--       return ()                                                !> ("freeing",th,"in",threadId env)
       let sibling=  children env

       (sbs',found) <- modifyMVar sibling $ \sbs -> do
                   let (sbs', found) = drop [] th  sbs
                   return (sbs',(sbs',found))

--       sbs <- takeMVar sibling
--       let (sbs', found) = drop [] th  sbs   -- !> "search "++show th ++ " in " ++ show (map threadId sbs)

       if found
         then do
--           putMVar sibling sbs'
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





-- | De-invert an event handler.
--
-- The first parameter is the setter of the event handler  to be
-- deinverted. Usually it is the primitive provided by a framework to set an event handler
--
-- the second parameter is the value to return to the event handler. Usually it is `return()`
--
-- it configures the event handler by calling the setter of the event
-- handler with the current continuation
react
  :: Typeable eventdata
  => ((eventdata ->  IO response) -> IO ())
  -> IO  response
  -> TransIO eventdata
react setHandler iob= Transient $ do
        cont    <- get
        case event cont of
          Nothing -> do
            liftIO $ setHandler $ \dat ->do
              runStateT (runCont cont) cont{event= Just $ unsafeCoerce dat}
              iob
            was <- getData `onNothing` return NoRemote
            when (was /= WasRemote) $ setData WasParallel
            return Nothing

          j@(Just _) -> do
            put cont{event=Nothing}
            return $ unsafeCoerce j



-- * non-blocking keyboard input

getLineRef= unsafePerformIO $ newTVarIO Nothing


roption= unsafePerformIO $ newMVar []

-- | Install a event receiver that wait for a string and trigger the continuation when this string arrives.
option :: (Typeable b, Show b, Read b, Eq b) =>
     b -> String -> TransIO b
option ret message= do
    let sret= show ret

    liftIO $ putStrLn $ "Enter  "++sret++"\tto: " ++ message
    liftIO $ modifyMVar_ roption $ \msgs-> return $ sret:msgs
    waitEvents  $ getLine' (==ret)
    liftIO $ putStr "option: " >> putStrLn (show ret)
    return ret


-- | Validates an input entered in the keyboard in non blocking mode. non blocking means that
-- the user can enter also anything else to activate other option
-- unlike `option`, wich watch continuously, input only wait for one valid response
input :: (Typeable a, Read a,Show a) => (a -> Bool) -> String -> TransIO a
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
           atomically $ writeTVar getLineRef Nothing
           processLine r
           inputLoop

processLine r= do

   let rs = breakSlash [] r

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

-- | Keep the main thread running, initiate the non blocking keyboard input and execute
-- the transient computation.
--
-- It also read a slash-separated list of string that are read by
-- `option` and `input` as if they were entered by the keyboard
--
-- >  foo  -p  options/to/be/read/by/option/and/input
keep :: Typeable a => TransIO a -> IO (Maybe a)
keep mx = do

   liftIO $ hSetBuffering stdout LineBuffering
   rexit <- newEmptyMVar
   forkIO $ do
--       liftIO $ putMVar rexit  $ Right Nothing
       runTransient $ do
           st <- get

           setData $ Exit rexit
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

-- | Same than `keep` but do not initiate the asynchronous keyboard input.
-- Useful for debugging or for creating background tasks, as well as to embed the Transient monad
-- inside another computation. It returns either the value returned by `exit`.
-- or Nothing, when there is no more threads running
keep' :: Typeable a => TransIO a -> IO  (Maybe a)
keep' mx  = do
   liftIO $ hSetBuffering stdout LineBuffering
   rexit <- newEmptyMVar
   forkIO $ do
           runTransient $ do
              setData $ Exit rexit
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

-- | Force the finalization of the main thread and thus, all the Transient block (and the application
-- if there is no more code)
exit :: Typeable a => a -> TransIO a
exit x= do
  Exit rexit <- getSData <|> error "exit: not the type expected"  `asTypeOf` type1 x
  liftIO $  putMVar  rexit  $ Just x
  stop
  where
  type1 :: a -> TransIO (Exit (MVar (Maybe a)))
  type1= undefined



-- | Alternative operator for maybe values. Used  in infix mode
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



-- | Assures that backtracking will not go further back
backCut :: (Typeable reason, Show reason) => reason -> TransientIO ()
backCut reason= Transient $ do
     delData $ Backtrack (Just reason)  []
     return $ Just ()

undoCut ::  TransientIO ()
undoCut = backCut ()

-- | The second parameter will be executed when backtracking
{-# NOINLINE onBack #-}
onBack :: (Typeable b, Show b) => TransientIO a -> ( b -> TransientIO a) -> TransientIO a
onBack ac bac = registerBack (typeof bac) $ Transient $ do
     Backtrack mreason _  <- getData `onNothing` backStateOf (typeof bac)
     runTrans $ case mreason of
                  Nothing     -> ac
                  Just reason -> bac reason
     where
     typeof :: (b -> TransIO a) -> b
     typeof = undefined

onUndo ::  TransientIO a -> TransientIO a -> TransientIO a
onUndo x y= onBack x (\() -> y)


-- | Register an action that will be executed when backtracking
{-# NOINLINE registerUndo #-}
registerBack :: (Typeable b, Show b) => b -> TransientIO a -> TransientIO a
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


registerUndo :: TransientIO a -> TransientIO a
registerUndo f= registerBack ()  f

-- | backtracking is stopped. the exection continues forward from this point on.
forward :: (Typeable b, Show b) => b -> TransIO ()
forward reason= Transient $ do
    Backtrack _ stack <- getData `onNothing`  (backStateOf reason)
    setData $ Backtrack(Nothing `asTypeOf` Just reason)  stack
    return $ Just ()

retry= forward ()

noFinish= forward (FinishReason Nothing)

-- | Execute backtracking. It execute the registered actions in reverse order.
--
-- If the backtracking flag is changed the flow proceed  forward from that point on.
--
-- If the backtrack stack is finished or undoCut executed, `undo` will stop.
back :: (Typeable b, Show b) => b -> TransientIO a
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

undo ::  TransIO a
undo= back ()


------ finalization

newtype FinishReason= FinishReason (Maybe SomeException) deriving (Typeable, Show)

-- | Initialize the event variable for finalization.
-- all the following computations in different threads will share it
-- it also isolate this event from other branches that may have his own finish variable
initFinish= backCut (FinishReason Nothing)

-- | Set a computation to be called when the finish event happens
onFinish :: ((Maybe SomeException) ->TransIO ()) -> TransIO ()
onFinish f= onFinish' (return ()) f


-- | Set a computation to be called when the finish event happens this only apply for
onFinish' ::TransIO a ->((Maybe SomeException) ->TransIO a) -> TransIO a
onFinish' proc f= proc `onBack`   \(FinishReason reason) ->
    f reason


-- | Trigger the event, so this closes all the resources
finish :: Maybe SomeException -> TransIO a
finish reason= back (FinishReason reason)



-- | trigger finish when the stream of data ends
checkFinalize v=
   case v of
      SDone ->  stop
      SLast x ->  return x
      SError e -> back  e
      SMore x -> return x

------ exceptions ---
-- | When a exception is produced in the continuation, the handler is executed.
-- | handlers are executed Last in first out.
onException :: Exception e => (e -> TransIO ()) -> TransIO ()
onException f= onAnyException $ \e ->
    case fromException e of
       Nothing -> return ()
       Just e'  -> f e'
  where
  onAnyException :: (SomeException ->TransIO ()) -> TransIO ()
  onAnyException  f= (return ()) `onBack` f

-- | stop the backtracking mechanism to execute further handlers
cutExceptions= backCut (undefined :: SomeException)

-- | Resume to normal execution at this point
continue = forward (undefined :: SomeException)


