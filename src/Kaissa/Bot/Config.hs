-- | Here is parsable config file is declared and utilities to load it from file.
module Kaissa.Bot.Config(
    Config
  , configToken
  , readConfig
  ) where

import Control.Lens
import Control.Monad
import Data.Text
import Data.Yaml
import Data.Yaml.Config
import GHC.Generics

-- | Server configuration loaded from YAML
data Config = Config {
  -- | Telegram API Bot Token
  _configToken :: !Text
} deriving (Generic)

makeLenses ''Config

instance FromJSON Config where
  parseJSON (Object o) = Config
    <$> o .: "token"
  parseJSON _ = mzero

-- | Read config from file
readConfig :: FilePath -> IO Config
readConfig f = loadYamlSettings [f] [] useEnv
