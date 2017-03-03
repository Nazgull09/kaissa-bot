-- | Here is parsable config file is declared and utilities to load it from file.
module Kaissa.Bot.Config(
    Config
  , configToken
  , configPollTimeout
  , configPollLimit
  , configDetailedLogging
  , configPort 
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
  _configToken           :: !Text
  -- | Telegram API polling timeout in seconds (0 is for testing)
, _configPollTimeout     :: !(Maybe Int)
  -- | Telegram API polling limits in count of messages
, _configPollLimit       :: !(Maybe Int)
  -- | Whether or not to log in detail requests
, _configDetailedLogging :: !Bool
  -- | Which port to use to host API
, _configPort            :: !Int
} deriving (Generic)

makeLenses ''Config

instance FromJSON Config where
  parseJSON (Object o) = Config
    <$> o .: "token"
    <*> o .:? "poll-timeout"
    <*> o .:? "poll-limit"
    <*> o .: "detailed-logging"
    <*> o .: "port"
  parseJSON _ = mzero

-- | Read config from file
readConfig :: FilePath -> IO Config
readConfig f = loadYamlSettings [f] [] useEnv
