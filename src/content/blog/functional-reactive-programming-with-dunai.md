---
title: Functional Reactive Programming with Dunai
date: 2024-06-27T00:00:00.000Z
---
[Dunai](https://github.com/ivanperez-keera/dunai) is a reactive programming library for Haskell. It is based on the concept of Monadic Stream Functions (MSFs), which are a generalization of the Arrowized Functional Reactive Programming (AFRP) model.

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

Running the program:

```text
$ stack run
abcdefghijklmNOPQRSTUVWXYZ
```

