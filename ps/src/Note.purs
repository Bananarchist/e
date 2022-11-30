module Note (frequencyForNote) where

import Prelude

import Data.Map as Map
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Data.Number (pow)

noteMap :: Map.Map String Number
noteMap =
  Map.fromFoldable
    [ ( Tuple "A" 27.50)
    , ( Tuple "A#" 29.14)
    , ( Tuple "Bb" 29.14)
    , ( Tuple "B" 30.87)
    , ( Tuple "C" 16.35)
    , ( Tuple "C#" 17.32)
    , ( Tuple "Db" 17.32)
    , ( Tuple "D" 18.35)
    , ( Tuple "D#" 19.45)
    , ( Tuple "Eb" 19.45)
    , ( Tuple "E" 20.60)
    , ( Tuple "F" 21.83)
    , ( Tuple "F#" 23.12)
    , ( Tuple "Gb" 23.12)
    , ( Tuple "G" 24.50)
    , ( Tuple "G#" 25.96)
    , ( Tuple "Ab" 25.96)
    ]

frequencyForNote :: String -> Number -> Number
frequencyForNote note octave =
  maybe 27.50 ((pow 2.0 octave) * _) $ Map.lookup note noteMap


