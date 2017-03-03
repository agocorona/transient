![Transient logo](https://github.com/transient-haskell/transient/raw/master/logo.png)
=========

[![Hackage](https://img.shields.io/hackage/v/transient.svg)](http://hackage.haskell.org/package/transient)
[![Stackage LTS](http://stackage.org/package/transient/badge/lts)](http://stackage.org/lts/package/transient)
[![Stackage Nightly](http://stackage.org/package/transient/badge/nightly)](http://stackage.org/nightly/package/transient)
[![Build Status](https://api.travis-ci.org/transient-haskell/transient.png?branch=master)](https://travis-ci.org/transient-haskell/transient)
[![Gitter](https://badges.gitter.im/theam/haskell-do.svg)](https://gitter.im/Transient-Transient-Universe-HPlay/Lobby?utm_source=share-link&utm_medium=link&utm_campaign=share-link)

NOTE: distributed computing and web primitives have been moved to [transient-universe](https://github.com/agocorona/transient-universe) and [ghcjs-hplay](https://github.com/agocorona/ghcjs-hplay)


## Some feedback on `transient`:

1. Rahul Muttineni @rahulmutt nov. 09 2016 03:40  Lead developper of ETA (the JVM Haskell compiler)

   *It's a bit mind bending in that it's like using a higher-level list monad, but it's very, very cool. For beginning Haskellers, what would be really useful is a visualisation of what happens when you do various distributed/parallel stuff.* **It's almost shocking how effortlessly you can run computations across threads/nodes.**

   *The cool part is the composability in the distributed setting. *You can make higher-order monadic functions that allow you to compose & reuse a long chain of distributed transactions via `wormhole` and `teleport`*. Another benefit is that the transaction becomes first class and* **you can see exactly what's going on in one place** *instead of distributing the logic across actors making the code equivalent to event callbacks, as you've stated.*

  https://gitter.im/Transient-Transient-Universe-HPlay/Lobby?at=58228caa35e6cf054773303b

## What is Transient?

One of the biggest dreams of software engineering is unrestricted composability.

This may be put in these terms:

let `ap1` and `ap2` two applications with arbitrary complexity, with all effects including multiple threads, asynchronous IO, indeterminism, events and perhaps, distributed computing.

Then the combinations:

     - ap1 <|> ap2          -- Alternative expression
     - ap1 >>= \x -> ap2    -- monadic sequence
     - ap1 <> ap2           -- monoidal expression
     - (,) <$> ap1 <*> ap2  -- Applicative expression

are possible if the types match, and generate new applications that are composable as well.

Transient does exactly that.

The operators `<$>` `<*>` and `<>` express concurrency, the operator `<|>` express parallelism and `>>=` for sequencing of threads and/or distributed processes. So even in the presence of these effects and others, everything is composable.

For this purpose transient is an extensible effects monad with all major effects and primitives for parallelism, events, asynchronous IO, early termination, non-determinism logging and distributed computing. Since it is possible to extend it with more effects without adding monad transformers, the composability is assured.

Documentation
=============

The [Wiki](https://github.com/agocorona/transient/wiki) is more user oriented

My video sessions in [livecoding.tv](https://www.livecoding.tv/agocorona/videos/) not intended as tutorials or presentations, but show some of the latest features running.

The articles are more technical:

- [Philosophy, async, parallelism, thread control, events, Session state](https://www.fpcomplete.com/user/agocorona/EDSL-for-hard-working-IT-programmers?show=tutorials)
- [Backtracking and undoing IO transactions](https://www.fpcomplete.com/user/agocorona/the-hardworking-programmer-ii-practical-backtracking-to-undo-actions?show=tutorials)
- [Non-deterministic list like processing, multithreading](https://www.fpcomplete.com/user/agocorona/beautiful-parallel-non-determinism-transient-effects-iii?show=tutorials)
- [Distributed computing](https://www.fpcomplete.com/user/agocorona/moving-haskell-processes-between-nodes-transient-effects-iv?show=tutorials)
- [Publish-Subscribe variables](https://www.schoolofhaskell.com/user/agocorona/publish-subscribe-variables-transient-effects-v)
- [Distributed streaming, map-reduce](https://www.schoolofhaskell.com/user/agocorona/estimation-of-using-distributed-computing-streaming-transient-effects-vi-1)

These articles contain executable examples (not now, since the site no longer support the execution of haskell snippets).
