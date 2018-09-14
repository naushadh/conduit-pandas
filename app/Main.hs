{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Lib
import           Data.Text (Text)
import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

main :: IO ()
main = Lib.innerJoin joinOn customers orders `C.connect` sink
  where
    joinOn c o = entityKey c == orderCustomerId (entityVal o)
    compact c o = (entityKey c, entityKey o)
    sink = CL.chunksOf 100 .| CL.mapM_ (mapM_ (print . uncurry compact))

data Entity a
  = Entity
  { entityKey :: Int
  , entityVal :: a
  } deriving Show

data Customer
  = Customer
  { customerName :: Text
  , customerAddress :: Text
  } deriving Show

data Order
  = Order
  { orderCustomerId :: Int
  , orderMessage :: Text
  } deriving Show

customers :: Monad m => Lib.Source m (Entity Customer)
customers = CL.sourceList go
  where
    mkCustomer i = Entity i $ Customer "someName" "someAddress"
    go = mkCustomer <$> [1..1000]

orders :: Monad m => Lib.Source m (Entity Order)
orders = CL.sourceList go
  where
    mkOrder i = Entity i $ Order (if i `mod` 3 == 0 then i else 0) "someMessage"
    go = mkOrder <$> [1..1000]
