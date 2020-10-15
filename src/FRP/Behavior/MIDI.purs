module FRP.Behavior.MIDI
  ( midi
  ) where

import Prelude

import Data.List (List)
import Data.Map (Map)
import FRP.Behavior (Behavior, behavior)
import FRP.Event.MIDI (MIDI, withMidi, MIDIEventInTime)

-- | A `Behavior` which reports the current midi program
midi :: MIDI -> Behavior (Map String (List MIDIEventInTime))
midi m = behavior \e -> map (\{ value, midi: midi_ } -> value midi_) (withMidi m e)
