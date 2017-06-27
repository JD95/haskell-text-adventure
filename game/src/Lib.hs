{-# LANGUAGE TypeApplications, OverloadedStrings #-}

module Lib where

import Conversation
import Data.Text

import Control.Monad
import Control.Concurrent
import System.IO
import System.Directory
import Data.Foldable
import Control.Exception
import qualified Data.ByteString.Lazy as B
import Foreign.ForeignPtr

import qualified SDL as SDL
import SDL.Init
import qualified SDL.Mixer as Mix


{-
withSound :: IO a -> IO a
withSound = bracket_ init cleanup
  where
    init = do
        initialize [SDL.InitAudio]
        getError >>= traverse_ putStrLn
        Mix.openAudio Mix.defaultAudio Mix.AudioS16LSB 2  4096

    cleanup = do
        Mix.closeAudio
        SDL.quit
-}

playSound :: IO ()
playSound = do
  initialize [SDL.InitAudio]
  Mix.withAudio (Mix.Audio 44100 Mix.FormatS16_Sys Mix.Stereo) 512 $ do
    mus <- Mix.load @Mix.Chunk "Edge of the World.wav"
    print "Loaded!"
    Mix.playForever mus
    Mix.playingMusic >>= print
    line <- getLine
    print line



convo :: Conversation ()
convo = do
  choice $ do
    path "Hi!" $ do
      npc ["Gee you're nice!"]
      player "Geewiz! Thanks!"
    path "Fuck off!" $ npc ["Why I'd never!"]
  npc ["Good bye!"]
