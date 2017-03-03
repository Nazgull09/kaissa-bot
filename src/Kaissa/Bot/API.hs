-- | Module where all API definitions are located
module Kaissa.Bot.API(
    KaissaAPI
  ) where

import Servant.API

-- | Main API of the server
type KaissaAPI = "dummy" :> Get '[JSON] ()
