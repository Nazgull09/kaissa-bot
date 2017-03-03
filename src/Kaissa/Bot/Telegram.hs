-- | Telegram utilities
module Kaissa.Bot.Telegram(
    testHelloRespond
  ) where

import Control.Monad 
import Data.Monoid
import Kaissa.Bot.Monad
import Web.Telegram.API.Bot.API
import Web.Telegram.API.Bot.API.Updates
import Web.Telegram.API.Bot.Data
import Web.Telegram.API.Bot.Requests
import Web.Telegram.API.Bot.Responses

testHelloRespond :: Update -> ServerM ()
testHelloRespond Update { message = Just msg } = do
  let user = maybe "всем" user_first_name $ from msg
      respond = sendMessageRequest (showt $ chat_id . chat $ msg ) ("Привет " <> user <> "!")
  void $ runTelegram' $ sendMessageM respond
testHelloRespond _ = pure ()
