-- | Module where WAI application is assembled
module Kaissa.Bot.Server(
    serverApp
  ) where

import Data.Proxy
import Kaissa.Bot.API
import Kaissa.Bot.Config
import Kaissa.Bot.Monad
import Servant.Server

-- | Full Server API
type ServerAPI = KaissaAPI

-- | WAI application of server
serverApp :: ServerEnv -> Application
serverApp e = serve (Proxy :: Proxy ServerAPI) $ enter (serverMToHandler e) $
  kaissaServer

-- | Implementation of main API
kaissaServer :: ServerT KaissaAPI ServerM
kaissaServer = dummyHanlder

-- | Temporary handler to take place in API
dummyHanlder :: ServerM ()
dummyHanlder = return ()
