module FRP.Behavior.Audio.Example.MidiIn where

import Prelude
import Data.Array (head)
import Data.List ((:), List(..))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty ((:|))
import Data.Tuple (snd)
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, CanvasInfo, defaultExporter, gain', runInBrowser, runInBrowser_, sinOsc, speaker, speaker')
import FRP.Behavior.MIDI (midi)
import FRP.Event.MIDI (MIDI, MIDIEvent(..), MIDIEventInTime, getMidi, midiAccess)
import Math (pi, sin)

simpleOnOff :: M.Map String (List MIDIEventInTime) -> Boolean
simpleOnOff m =
  fromMaybe false
    ( do
        h <- head (M.toUnfoldable m)
        pure $ go (snd h)
    )
  where
  go :: List MIDIEventInTime -> Boolean
  go Nil = false

  go ({ event: (NoteOn _ _ _) } : rest) = true

  go ({ event: (NoteOff _ _ _) } : rest) = false

  go (_ : rest) = go rest

scene :: MIDI -> Number -> Behavior (AudioUnit D1)
scene midiIn time = f <$> (midi midiIn)
  where
  rad = pi * time

  f md =
    speaker'
      (gain' (if simpleOnOff md then 0.6 else 0.0) $ sinOsc 440.0)

run macc =
  runInBrowser_ do
    md <- getMidi macc
    pure (scene md)

macc = midiAccess

exporter = defaultExporter

main :: Effect Unit
main = pure unit
