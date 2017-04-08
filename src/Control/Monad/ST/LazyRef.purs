module Control.Monad.ST.LazyRef (LazySTRef, defer, force) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, STRef, newSTRef, readSTRef, writeSTRef)
import Data.Newtype (class Newtype)

newtype Deferred b = Deferred forall a. a -> b

mkDeferred :: forall b. (forall a. a -> b) -> Deferred b
mkDeferred = Deferred

runDeferred :: forall b. Deferred b -> b
runDeferred (Deferred f) = f unit

deferredConst :: forall b. b -> Deferred b
deferredConst = Deferred <<< const

newtype LazySTRef h a = LazySTRef (STRef h (Deferred a))
derive instance newtypeLazySTRef :: Newtype (LazySTRef h a) _
instance showLazySTRef :: Show (LazySTRef h a) where
  show _ = "<LazySTRef>"

defer :: forall a h r.
         (forall s. s -> a) -> Eff (st :: ST h | r) (LazySTRef h a)
defer f = LazySTRef <$> newSTRef (mkDeferred f)

force :: forall a h r. LazySTRef h a -> Eff (st :: ST h | r) a
force (LazySTRef r) = do
  f <- readSTRef r
  let forced = runDeferred f
  _ <- writeSTRef r (deferredConst forced)
  pure forced
