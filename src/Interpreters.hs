{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RankNTypes        #-}

module Interpreters where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Aeson               (ToJSON, FromJSON)
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

import Types

-- the database
globalCounter :: IORef Counter
{-# NOINLINE globalCounter #-}
globalCounter =
    let k = "matt"
        hm = HM.fromList [(k, 42)]
    in unsafePerformIO $ newIORef (Counter hm 0)


runLoggingProgram :: Free (Sum LogF CounterF) a -> IO a
runLoggingProgram = foldFree f
  where
    f :: (Sum LogF CounterF) a -> IO a
    f (InL op) = g op
    f (InR op) = h op

    g :: LogF a -> IO a
    g (Log s a) = putStrLn s >> return a

    h :: CounterF a -> IO a
    h = runCounterF

------------------------------------------------------------
-- COUNTER
------------------------------------------------------------

-- public API
readKey :: Key -> CounterAPI KV
readKey key = liftF $ ReadKey key id

writeKey :: Key -> Value -> CounterAPI ()
writeKey key v = liftF $ WriteKey key v ()

printCounter :: CounterAPI ()
printCounter = liftF $ Print ()

-- interpreter
runProgram :: CounterAPI a -> IO a
runProgram = foldFree runCounterF

-- implementation

runCounterF :: CounterF a -> IO a
runCounterF (ReadKey k f') =  do
    x <- rdCounter k
    return $ f' x
runCounterF (WriteKey k v f') = do
    wtCounter k v
    return f'
runCounterF (Print f') = do
    pCounter
    return f'

rdCounter :: Key -> IO KV
rdCounter k = do
    (Counter hm df) <- readIORef globalCounter
    return $ KV k (fromMaybe df (HM.lookup k hm))

wtCounter :: Key -> Value -> IO ()
wtCounter k v = do
    (Counter hm df) <- readIORef globalCounter
    modifyIORef globalCounter (ins k v)
  where
    ins :: Key -> Value -> Counter -> Counter
    ins k v (Counter hm df) =
      let hm' = HM.insert k v hm
      in Counter hm' df

pCounter :: IO ()
pCounter = do
    (Counter c df)  <- readIORef globalCounter
    print $ HM.toList c

{-
addOne :: Key -> CounterAPI ()
addOne k = do
    KV k v <- readKey k
    let newV = v + 1
    writeKey k newV
    printCounter

-- initialize and run
runC :: IO ()
runC = do
    let k = "matt"
    runProgram $ addOne k

runCL :: IO ()
runCL = do
    let k = "matt"
    runLoggingProgram $ foldFree loggingCounterI $ addOne k
-}

------------------------------------------------------------
-- LOGGING
------------------------------------------------------------

loggingCounterI :: CounterF a -> Free (Sum LogF CounterF) a
loggingCounterI op = toLeft (logI op) *> toRight (liftF op)

logI :: CounterF a -> Free LogF ()
logI (ReadKey k _) = liftF $ Log ("** read key: " ++ show k) ()
logI (WriteKey k v _) = liftF $ Log ("** write to key: " ++ show k ++ " value: " ++ show v) ()
logI _ = return ()

toLeft :: (Functor f, Functor g) => Free f a -> Free (Sum f g) a
toLeft = hoistFree InL
toRight :: (Functor f, Functor g) => Free g a -> Free (Sum f g) a
toRight = hoistFree InR

