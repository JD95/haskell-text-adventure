module Lib where

import Conversation
import Data.Text
import Prelude()
import Protolude

convo :: Conversation ()
convo = do
  choice $ do
    path "Hi!" $ do
      npc ["Gee you're nice!"]
      player "Geewiz! Thanks!"
    path "Fuck off!" $ npc ["Why I'd never!"]
  npc ["Good bye!"]
