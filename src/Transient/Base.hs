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
-----------------------------------------------------------------------------

module Transient.Base(
-- * The Monad
TransIO(..), TransientIO
-- * Running the monad
,keep, keep', stop

-- * input
,option, input, exit

-- * Asynchronous operations
,async,waitEvents, spawn, parallel, sample
,react

-- * State management
,setState, setData, getState, getSData,getData,delState,delData, modifyData,modifyState

-- * Thread management
, threads,addThreads, freeThreads, hookedThreads,oneThread, killChilds

-- * Additional operators
, (**>), (<**),(<***), (<|)

-- * exceptions

,onException, cutExceptions, continue

-- * Utilities
, StreamData(..)
,genId
)

where


import    Transient.Internals
