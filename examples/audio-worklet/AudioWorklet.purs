module FRP.Behavior.Audio.Example.AudioWorklet where

import Prelude
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioInfo, AudioUnit, EngineInfo, VisualInfo, Exporter, audioWorkletAggregator, audioWorkletGenerator, audioWorkletProcessor, defaultExporter, gain', runInBrowser, sinOsc, speaker')
import Foreign.Object (Object)
import Foreign.Object as O
import Math (pi, sin)

scene :: Number -> Behavior (AudioUnit D1)
scene time =
  pure
    $ speaker'
        ( audioWorkletGenerator
            "white-noise-processor"
            (O.singleton "customGain" $ 0.05 + (0.05 * sin (0.1 * rad)))
        )
  where
  rad = time * pi

scene1 :: Number -> Behavior (AudioUnit D1)
scene1 time =
  pure
    $ speaker'
        ( audioWorkletProcessor
            "gain-processor"
            (O.singleton "customGain" $ 0.05 + (0.05 * sin (1.0 * rad)))
            (sinOsc 220.0)
        )
  where
  rad = time * pi

scene1p :: Number -> Behavior (AudioUnit D1)
scene1p time =
  pure
    $ speaker'
        ( gain' (0.05 + (0.05 * sin (1.0 * rad)))
            (sinOsc 220.0)
        )
  where
  rad = time * pi

scene2 :: Number -> Behavior (AudioUnit D1)
scene2 time =
  pure
    $ speaker'
        ( gain' 0.2
            ( audioWorkletAggregator
                "add-processor"
                (O.empty)
                (sinOsc 220.0)
                (sinOsc 440.0)
            )
        )
  where
  rad = time * pi

scene2p :: Number -> Behavior (AudioUnit D1)
scene2p time =
  pure
    $ speaker'
        ( gain' 0.2
            ( (sinOsc 220.0)
                + (sinOsc 440.0)
            )
        )
  where
  rad = time * pi

run ::
  forall microphone track buffer floatArray periodicWave.
  Unit ->
  AudioContext ->
  EngineInfo ->
  AudioInfo (Object microphone) (Object track) (Object buffer) (Object floatArray) (Object periodicWave) ->
  VisualInfo ->
  Exporter Unit Unit ->
  Effect (Effect Unit)
run = runInBrowser scene

exporter = defaultExporter :: Exporter Unit Unit

main :: Effect Unit
main = pure unit
