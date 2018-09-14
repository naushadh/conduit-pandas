module Lib where

import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Maybe as Maybe
import           Data.Tuple (swap)

sayHello :: IO ()
sayHello = putStrLn "Hello"

type Source m a = C.ConduitT () a m ()

leftJoin
  :: (Monad m)
  => (a -> b -> Bool)
  -> Source m a
  -> Source m b
  -> Source m (a, Maybe b)
leftJoin test sA sB
  =  crossProduct sA sB
  .| CC.map (uncurry go)
  where
    go a b = if test a b then (a, Just b) else (a, Nothing)

rightJoin
  :: (Monad m)
  => (a -> b -> Bool)
  -> Source m a
  -> Source m b
  -> Source m (Maybe a, b)
rightJoin test sA sB = leftJoin (flip test) sB sA .| CC.map swap

innerJoin
  :: (Monad m)
  => (a -> b -> Bool)
  -> Source m a
  -> Source m b
  -> Source m (a, b)
innerJoin test sA sB
  =  crossProduct sA sB
  .| CC.map (uncurry go)
  .| CC.filter Maybe.isJust
  .| CC.map Maybe.fromJust
  where
    go a b = if test a b then Just (a, b) else Nothing

data These a b
  = This a
  | That b
  | These a b
  deriving Show

outerJoin
  :: (Monad m, Eq k)
  => (a -> k)
  -> (b -> k)
  -> Source m a
  -> Source m b
  -> Source m (These a b)
outerJoin kA kB sA sB
  = void $ C.sequenceConduits [sL, sJ, sR]
  where
    test a b = kA a == kB b
    sJ = innerJoin test sA sB .| CC.map (uncurry These)
    antiTest a b = not $ test a b
    sL = leftJoin antiTest sA sB .| CC.map (This . fst)
    sR = rightJoin antiTest sA sB .| CC.map (That . snd)

crossProduct
  :: Monad m
  => Source m a
  -> Source m b
  -> Source m (a,b)
crossProduct sA sB = do
  lA <- lift $ sA `C.connect` CC.length
  lB <- lift $ sB `C.connect` CC.length
  let sA' = sA .| repeatEachN lB
  let sB' = repeatAllN lA sB
  C.getZipSource $ (,) <$> C.ZipSource sA' <*> C.ZipSource sB'

repeatEachN :: Monad m => Int -> C.ConduitT a a m ()
repeatEachN n = CC.concatMap (replicate n)

repeatAllN :: Monad m => Int -> C.ConduitT i o m r -> C.ConduitT i o m ()
repeatAllN n src = void $ C.sequenceConduits (replicate n src)
