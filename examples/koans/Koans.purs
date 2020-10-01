module FRP.Behavior.Audio.Example.Koans where

import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Typelevel.Num (D1, D2)
import Data.Vec ((+>), empty)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, CanvasInfo, Oversample(..), delay, dup1, gain', merger, panner, periodicOsc, play, runInBrowser, sawtoothOsc, sinOsc, speaker, speaker', squareOsc, traingleOsc, waveShaper)
import Math (pi, sin)

-- constant
nothing :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
nothing _ _ _ = pure zero

-- triangle
triangle :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
triangle _ _ _ = pure $ speaker' (gain' 0.3 $ traingleOsc 420.0)

-- saw
saw :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
saw _ _ _ = pure $ speaker' (gain' 0.3 $ sawtoothOsc 420.0)

-- fixed periodic wave
pdfix :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
pdfix _ _ _ = pure $ speaker' (gain' 0.3 $ periodicOsc "funtimes" 325.0)

-- fixed periodic wave
wsh :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
wsh _ _ _ = pure $ speaker' (gain' 0.3 (waveShaper "waveshaperCurve" FourX $ (play "forest")))

-- square
square :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
square _ _ _ = pure $ speaker' (gain' 0.3 $ squareOsc 420.0)

-- delay
wait :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
wait _ _ time =
  let
    rad = pi * time
  in
    pure
      $ speaker
          ( delay 1.0 (gain' 0.1 $ sinOsc (440.0 + (10.0 * sin (2.3 * rad))))
              :| delay 2.0 (gain' 0.25 $ sinOsc (235.0 + (10.0 * sin (1.7 * rad))))
              : delay 3.0 (gain' 0.2 $ sinOsc (337.0 + (10.0 * sin rad)))
              : (gain' 0.1 $ sinOsc (530.0 + (19.0 * (5.0 * sin rad))))
              : Nil
          )

-- mul
ringMod :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
ringMod _ _ _ =
  pure
    $ speaker'
        ( (gain' 0.5 $ sinOsc (440.0))
            * (gain' 0.5 $ sinOsc (30.0))
        )

-- panner, merger, dup
pan :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D2)
pan _ _ time =
  pure
    $ dup1
        ( (gain' 0.2 $ sinOsc 110.0)
            + (gain' 0.1 $ sinOsc 220.0)
        ) \mono ->
        speaker
          $ ( (panner (sin rad) (merger (mono +> mono +> empty)))
                :| Nil
            )
  where
  rad = pi * time

run = runInBrowser wsh

main :: Effect Unit
main = pure unit
