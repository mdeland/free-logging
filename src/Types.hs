{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}

module Types where

import GHC.Generics (Generic)
import           Control.Monad.Free
import           Data.Aeson               (ToJSON, FromJSON)
import           Data.Text
import qualified Data.HashMap              as HM

type Key = Text
type Value = Int

data KV = KV
  { key :: Key
  , value :: Value
  } deriving (Generic, FromJSON)

instance ToJSON KV

-- the map and a default value
data Counter = Counter (HM.Map Key Value) Value

data CounterF a = WriteKey Key Value a
                | ReadKey Key (KV -> a)
                | DeleteKey Key a
                | Print a
                  deriving Functor

type CounterAPI = Free CounterF

data LogF a = Log String a
                deriving Functor
