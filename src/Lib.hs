{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RankNTypes        #-}

module Lib
    ( startServer
    ) where

import GHC.Generics (Generic)

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Aeson                (ToJSON)
import           Data.Functor.Sum
import           Data.Hashable
import qualified Data.HashMap              as HM
import           Data.IORef
import           Data.Maybe
import           Data.Monoid               (mconcat)
import           Data.Text
import           Data.Text.Encoding
import           GHC.Generics              (Generic)
import           Network.Wai               (Application)
import qualified Network.Wai.Handler.Warp  as Warp
import           Servant
import           System.IO
import           System.IO.Unsafe


startServer :: IO ()
startServer = Warp.run 3000 app



data Key = Key Text
  deriving (Eq, Ord, Generic, Hashable, Show)

instance FromText Key
    where fromText t = Just (Key t)

-- the map and a default value
data Counter t = Counter (IORef (HM.Map Key t)) t

data CounterF t a = WriteKey (Counter t) Key t (Counter t -> a)
                | ReadKey (Counter t) Key (t -> a)
                | DeleteKey (Counter t) Key (Counter t -> a)
                | Print (Counter t) a
                  deriving Functor

type CounterAPI t = Free (CounterF t)


-- public API
readKey :: Key -> Counter t -> CounterAPI t t
readKey key counter = liftF $ ReadKey counter key id

writeKey :: Key -> t -> Counter t -> CounterAPI t (Counter t)
writeKey key v counter = liftF $ WriteKey counter key v id

printCounter :: Counter t -> CounterAPI t ()
printCounter counter = liftF $ Print counter ()

-- interpreter
runProgram :: (Show t) => CounterAPI t a -> IO a
runProgram = foldFree runCounterF

-- implementation

runCounterF :: (Show t) => CounterF t a -> IO a
runCounterF (ReadKey c k f') = return $ f' $ rdCounter c k
runCounterF (WriteKey c k v f') = return $ f' $ wtCounter c k v
runCounterF (Print c f') = ptCounter c >> return f'

rdCounter :: Counter t -> Key -> IO t
rdCounter (Counter c df) k = fromMaybe df (HM.lookup k c)

wtCounter :: Counter t -> Key -> t -> IO (Counter t)
wtCounter (Counter c df) k v = do
    cc <- readIORef c
    hm <- HM.insert k v cc
    newC <- Counter hm df
    writeIORef c newC cc

ptCounter :: Show t => Counter t -> IO ()
ptCounter (Counter c _) = do
    cc <- readIORef c
    print $ HM.toList cc


addOne :: Counter Int -> Key -> CounterAPI Int ()
addOne c k = do
    x <- readKey k c
    let newX = x + 1
    newCounter <- writeKey k newX c
    printCounter newCounter

-- initialize and run
runC :: IO ()
runC = do
    let k = Key "matt"
    -- let c = Counter (HM.fromList [(k, 42)]) 0
    runProgram $ addOne globalCounter k


--- part 2, logging

data LogF a = Log String a
                deriving Functor

logI :: (Show t) => CounterF t a -> Free LogF ()
logI (ReadKey _ k _) = liftF $ Log ("** read key: " ++ show k) ()
logI (WriteKey _ k v _) = liftF $ Log ("** write to key: " ++ show k ++ " value: " ++ show v) ()
logI _ = return ()

loggingCounterI :: (Show t) => CounterF t a -> Free (Sum LogF (CounterF t)) a
loggingCounterI op = toLeft (logI op) *> toRight (liftF op)

toLeft :: (Functor f, Functor g) => Free f a -> Free (Sum f g) a
toLeft = hoistFree InL
toRight :: (Functor f, Functor g) => Free g a -> Free (Sum f g) a
toRight = hoistFree InR

runLoggingProgram :: (Show t) => Free (Sum LogF (CounterF t)) a -> IO a
runLoggingProgram = foldFree f
  where
    f :: (Show t) => (Sum LogF (CounterF t)) a -> IO a
    f (InL op) = g op
    f (InR op) = h op

    g :: LogF a -> IO a
    g (Log s a) = putStrLn s >> return a

    h :: (Show t) => (CounterF t) a -> IO a
    h = runCounterF


runCL :: IO ()
runCL = do
    let k = Key "matt"
    -- let c = Counter (HM.fromList [(k, 42)]) 0
    runLoggingProgram $ foldFree loggingCounterI $ addOne globalCounter k


apiReadKey :: Counter Int -> Key -> Free (Sum LogF (CounterF Int)) Int
apiReadKey c k = foldFree loggingCounterI $ readKey k c

-- the data
globalCounter :: Counter Int
{-# NOINLINE globalCounter #-}
globalCounter =
    let k = Key "matt"
        hm = unsafePerformIO $ newIORef (HM.fromList [(Key "matt", 42)])
    in Counter hm 0

-- the app

app :: Application
app = serve api server

-- the API, or the routes

type Api = "counter" :> Capture "tz" Key :> Get '[JSON] Int

-- Server Api == EitherT ServantErr IO [User]
server :: Server Api
server = do
    -- let c = Counter (newIORef (HM.fromList [(k, 42)])) 0
    liftIO . runLoggingProgram . apiReadKey globalCounter

api :: Proxy Api
api = Proxy
