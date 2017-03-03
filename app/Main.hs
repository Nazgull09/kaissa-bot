module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Monoid
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Options.Applicative

import Kaissa.Bot

-- | Argument line options
data Options = Options {
  -- | Path to config, if not set, the app will not start
  configPath :: FilePath
}

-- | Command line parser
optionsParser :: Parser Options
optionsParser = Options
  <$> strOption (
         long "conf"
      <> metavar "CONFIG"
      <> help "Path to configuration file"
    )

-- | Execute server with given options
runServer :: Options -> IO ()
runServer Options{..} = do
  cfg <- readConfig configPath
  env <- newServerEnv cfg (liftIO . print)
  let logger = makeLogger $ cfg ^. configDetailedLogging
  run (cfg ^. configPort) $ logger $ serverApp env
  where
    makeLogger b = if b
      then logStdoutDev
      else logStdout

main :: IO ()
main = execParser opts >>= runServer
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Telegram bot for testing NLP algorithms"
     <> header "kaissa-bot - Telegram bot for testing NLP algorithms" )
