module Transient.Utils where

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (x:xs) = xs
