module FRP.Behavior.Audio.Example.AudioWorklet where

import Prelude
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, audioWorkletGenerator, audioWorkletProcessor, gain', runInBrowser, sinOsc, speaker')
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

run = runInBrowser scene1p

main :: Effect Unit
main = pure unit
