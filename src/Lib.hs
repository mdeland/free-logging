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
import Interpreters


startServer :: IO ()
startServer = Warp.run 3000 app

apiReadKey ::  Key -> Free (Sum LogF CounterF) KV
apiReadKey = readKey

apiWriteKey :: KV -> Free (Sum LogF CounterF) ()
apiWriteKey (KV k v) = foldFree loggingCounterI $ writeKey k v

-- the app

app :: Application
app = serve api server

-- the API, or the routes

type Api = "counter" :> Capture "k" Key :> Get '[JSON] KV
      :<|> "counter" :> ReqBody '[JSON] KV :> Post '[JSON] ()

server :: Server Api
server =
    liftIO . runLoggingProgram . apiReadKey
    :<|> liftIO . runLoggingProgram . apiWriteKey

api :: Proxy Api
api = Proxy
