module Lib where

import Prelude()
import Protolude
import Conversation
import Data.Text

convo :: Conversation ()
convo = do
    npc ["Hello there!"]
    choice $ do
        path "Hi!" $ do
            npc ["Gee you're nice!"]
            response "I try to be kind!"
            npc [ "Well it's working out for you!"
                , "Take a discount!" ]
            response "Geewiz! Thanks!"
        path "Fuck off!" $ do
            npc ["Someone has a stick up their butt."]
            response "You'll have more than a stick up your's if you don't shut up."
            npc ["Why I'd never!"]
    npc ["Good bye!"]
