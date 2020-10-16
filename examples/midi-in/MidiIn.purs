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
import FRP.Behavior.Audio (AudioContext, AudioInfo, AudioUnit, CanvasInfo, Exporter, Time'AudioInstructions(..), VisualInfo, defaultExporter, gain', runInBrowser, runInBrowser_, sinOsc, speaker, speaker')
import FRP.Behavior.MIDI (midi)
import FRP.Event.MIDI (MIDI, MIDIAccess, MIDIEvent(..), MIDIEventInTime, getMidi, midiAccess)
import Foreign.Object (Object)
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

run ::
  forall microphone track buffer floatArray periodicWave.
  MIDIAccess ->
  Unit ->
  Int ->
  Int ->
  AudioContext ->
  AudioInfo (Object microphone) (Object track) (Object buffer) (Object floatArray) (Object periodicWave) ->
  VisualInfo ->
  Exporter Unit Time'AudioInstructions ->
  Effect (Effect Unit)
run max =
  runInBrowser_ do
    md <- getMidi max
    pure (scene md)

macc = midiAccess

exporter = defaultExporter

main :: Effect Unit
main = pure unit
