module FRP.Behavior.Audio.Example.DupSplit where

-- tests everything in the readme to make sure it works!
import Prelude
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioInfo, AudioUnit, EngineInfo, VisualInfo, Exporter, defaultExporter, dup1, microphone, runInBrowser, sinOsc, speaker')
import Foreign.Object (Object)
import Math (pi, sin)

sceneDup :: Number -> Behavior (AudioUnit D1)
sceneDup t = pure (speaker' $ dup1 microphone (\u -> (sinOsc (5.0 + 10.0 * (sin (0.2 * t * pi))) * u)))

run ::
  forall microphone track buffer floatArray periodicWave.
  Unit ->
  AudioContext ->
  EngineInfo ->
  AudioInfo (Object microphone) (Object track) (Object buffer) (Object floatArray) (Object periodicWave) ->
  VisualInfo ->
  Exporter Unit ->
  Effect (Effect Unit)
run = runInBrowser sceneDup

exporter = defaultExporter :: Exporter Unit

main :: Effect Unit
main = pure unit
