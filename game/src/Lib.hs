module Lib where

import Prelude()
import Protolude
import Conversation
import Data.Text

convo :: Conversation ()
convo = do
  choice $ do
    path "Hi!" $ do
      npc ["Gee you're nice!"]
      player "Geewiz! Thanks!"
    path "Fuck off!" $ do npc ["Why I'd never!"]
  npc ["Good bye!"]
