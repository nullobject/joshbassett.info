---
title: Shiki demo
layout: ../layouts/main.astro
---

# Joshua Bassett

This is an article about the `dunai` package:

```haskell
module Main (main) where

import Control.Monad (guard)
import Control.Monad.Trans.MSF (catchMaybe, embed_)
import Data.Char (toUpper)
import Data.MonadicStreamFunction (Arrow (..), arrM, liftBaseM, (>>>))

main :: IO ()
main = do
  embed_ ((a `catchMaybe` b) >>> c) ['a' .. 'z']
  putChar '\n'
  where
    a = arrM $ \x -> guard (x <= 'm') >> return x
    b = arr toUpper
    c = liftBaseM putChar
```
