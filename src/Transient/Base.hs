{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Base
-- Copyright   :  (c) 2014-2016 Alberto G. Corona
-- License     :  MIT-style (see the file LICENSE)
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | See http://github.com/transient-haskell/transient
-----------------------------------------------------------------------------
module Transient.Base (
  TransIO(..), TransientIO,
  keep, keep', stop,
  option, input, exit,
  async, waitEvents, spawn, parallel, sample,
  react, setData, getData, getSData, delData,
  threads, addThreads, freeThreads, hookedThreads, oneThread, killChilds,
  (**>), (<**), (<***), (<|),
  StreamData(..), genId
  ) where

import Transient.Internals

type TransientIO = TransIO
