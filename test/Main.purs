module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Console.BrowserSpecific (timeEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.ST (runST)
import Control.Monad.ST.LazyRef (defer, force)
import Data.Array ((..), foldl)

main :: forall e. Eff (exception :: EXCEPTION, console :: CONSOLE | e) Unit
main = runST do
  l <- defer \_ -> foldl (*) 1 (1 .. 32)
  timeEff "first force" $ force l >>= logShow
  timeEff "second" $ force l >>= logShow
  timeEff "third" $ force l >>= logShow
