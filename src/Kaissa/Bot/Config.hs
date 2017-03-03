-- | Here is parsable config file is declared and utilities to load it from file.
module Kaissa.Bot.Config(
    Config
  ) where

import GHC.Generics
import Control.Lens

data Config = Config {

} deriving (Generic)

makeLenses ''Config
