module FRP.Behavior.Audio.Example.Exporter where

import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Typelevel.Num (D1)
import Effect (Effect)
import Effect.Class.Console (log)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioInfo, AudioUnit, CanvasInfo, Exporter, Instruction, RunInBrowserAudioUnit, Time'AudioInstructions(..), VisualInfo, defaultExporter, gain', microphone, runInBrowser, sinOsc, speaker)
import Foreign.Object (Object)
import Math (pi, sin)

scene :: Number -> Behavior (AudioUnit D1)
scene time =
  let
    rad = pi * time
  in
    pure
      $ speaker
          ( (gain' 0.1 $ sinOsc (440.0 + (10.0 * sin (2.3 * rad))))
              :| (gain' 0.25 $ sinOsc (235.0 + (10.0 * sin (1.7 * rad))))
              : (gain' 0.2 $ sinOsc (337.0 + (10.0 * sin rad)))
              : (gain' 0.1 $ sinOsc (530.0 + (19.0 * (5.0 * sin rad))))
              : Nil
          )

{-
accumulator ->
    Int ->
    Int ->
    AudioContext ->
    AudioInfo (Object microphone) (Object track) (Object buffer) (Object floatArray) (Object periodicWave) ->
    VisualInfo ->
    Exporter env exportable ->
    Effect (Effect Unit)
    -}
run ::
  forall microphone track buffer floatArray periodicWave.
  Unit ->
  Int ->
  Int ->
  AudioContext ->
  AudioInfo (Object microphone) (Object track) (Object buffer) (Object floatArray) (Object periodicWave) ->
  VisualInfo ->
  Exporter String Time'AudioInstructions ->
  Effect (Effect Unit)
run = runInBrowser scene

exporter :: Exporter String Time'AudioInstructions
exporter =
  { acquire: pure "hello"
  -- this prints to the log, but it can be used for sending audio instructions
  -- anywhere, ie to an external MIDI device
  , use:
      \env (Time'AudioInstructions time instr) -> do
        log env
        log $ show time
        log $ show instr
  , release: \env -> log ("releasing " <> env)
  }

main :: Effect Unit
main = pure unit
