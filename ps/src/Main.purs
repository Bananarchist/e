module Main where

import Prelude

import Audio.WebAudio.BaseAudioContext (newAudioContext, createOscillator, createGain, destination)
import Audio.WebAudio.GainNode (setGain)
import Audio.WebAudio.Oscillator (OscillatorType(..), setFrequency, setOscillatorType, startOscillator)
import Audio.WebAudio.Types (connect)
import Effect (Effect)
import Effect.Console (log)
import Web.HTML (window)
import Web.HTML.Window (document)
import Note (frequencyForNote)

import Data.Tuple (Tuple(..))
import Data.Map as Map
import Data.Maybe (maybe)


main :: Effect Unit
main = do
  -- w <- window
  -- doc <- document w
  ctx <- newAudioContext
  
  osc <- createOscillator ctx
  setOscillatorType Square osc
  setFrequency 27.50 osc

  gn <- createGain ctx
  setGain 0.5 gn

  connect osc gn
  connect gn =<< destination ctx

  startOscillator 0.0 osc

