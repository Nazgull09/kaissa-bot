-- | Here is main monad of server is declared. Each handler operates in the
-- monad.
module Kaissa.Bot.Monad(
    ServerM
  , ServerEnv
  , newServerEnv
  , runServerM
  , serverMToHandler
  -- * Helpers
  , getConfig
  ) where

import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Kaissa.Bot.Config
import Network.HTTP.Client      (newManager, Manager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Servant.Server

-- | Environment of 'ServerM' monad
data ServerEnv = ServerEnv {
  -- | Stored server config
  serverConfig  :: !Config
  -- | HTTPS client manager
, serverManager :: !Manager
}

-- | Create fresh server environment
newServerEnv :: Config -> IO ServerEnv
newServerEnv cfg = do
  mng <- newManager tlsManagerSettings
  pure ServerEnv {
      serverConfig  = cfg
    , serverManager = mng
    }

-- | Main monad of server, each handler of API operates in the monad.
newtype ServerM a = ServerM { unServerM :: ReaderT ServerEnv (LoggingT Handler) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO)

-- | Helper for 'MonadBaseControl' instance
newtype StMServerM a = StMServerM { unStMServerM :: StM (ReaderT ServerEnv (LoggingT Handler)) a }

-- | Allow unlifting IO arguments of functions into the monad
instance MonadBaseControl IO ServerM where
  type StM ServerM a = StMServerM a
  liftBaseWith f = ServerM $ liftBaseWith $ \q -> f (fmap StMServerM . q . unServerM)
  restoreM = ServerM . restoreM . unStMServerM

-- | Getting server configuration
getConfig :: ServerM Config
getConfig = ServerM $ asks serverConfig

-- | Getting manager for connecting another servers
getManager :: ServerM Manager
getManager = ServerM $ asks serverManager

-- | Execute server monad to default servant handler
runServerM :: ServerEnv -> ServerM a -> Handler a
runServerM e m = runStdoutLoggingT $ runReaderT (unServerM m) e

-- | Transformation from 'ServerM' monad to 'Handler'
serverMToHandler :: ServerEnv -> ServerM :~> Handler
serverMToHandler e = NT (runServerM e)
