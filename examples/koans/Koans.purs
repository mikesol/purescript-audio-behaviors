module FRP.Behavior.Audio.Example.Koans where

import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Typelevel.Num (D1, D2)
import Data.Vec ((+>), empty)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, CanvasInfo, Oversample(..), allpass, bandpass, convolver, delay, dup1, dynamicsCompressor, gain', highpass, highshelf, loopBuf, lowpass, lowshelf, merger, notch, panner, peaking, periodicOsc, play, playBuf, runInBrowser, sawtoothOsc, sinOsc, speaker, speaker', squareOsc, traingleOsc, waveShaper)
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

-- comp
comp :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
comp _ _ _ = pure $ speaker' (gain' 0.3 (dynamicsCompressor (-50.0) 40.0 12.0 0.0 0.25 $ (play "forest")))

-- verb
verb :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
verb _ _ _ = pure $ speaker' (gain' 0.3 (convolver "moo" $ (play "forest")))

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

-- filters
f0 :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
f0 _ _ _ = pure $ speaker' (gain' 0.3 (lowpass 350.0 1.0 $ (play "forest")))

f1 :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
f1 _ _ _ = pure $ speaker' (gain' 0.3 (highpass 350.0 1.0 $ (play "forest")))

f2 :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
f2 _ _ _ = pure $ speaker' (gain' 0.3 (lowshelf 350.0 0.0 $ (play "forest")))

f3 :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
f3 _ _ _ = pure $ speaker' (gain' 0.3 (highshelf 350.0 0.0 $ (play "forest")))

f4 :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
f4 _ _ _ = pure $ speaker' (gain' 0.3 (bandpass 350.0 1.0 $ (play "forest")))

f5 :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
f5 _ _ _ = pure $ speaker' (gain' 0.3 (allpass 350.0 1.0 $ (play "forest")))

f6 :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
f6 _ _ _ = pure $ speaker' (gain' 0.3 (peaking 350.0 1.0 0.0 $ (play "forest")))

f7 :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
f7 _ _ _ = pure $ speaker' (gain' 0.3 (notch 350.0 1.0 $ (play "forest")))

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

-- pb
pb :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
pb _ _ _ = pure $ speaker' (gain' 0.3 (playBuf "moo" 1.0))

-- lb
lb :: forall a. a -> CanvasInfo -> Number -> Behavior (AudioUnit D1)
lb _ _ _ = pure $ speaker' (gain' 0.3 (loopBuf "moo" 1.0 0.0 0.5))

run = runInBrowser lb

main :: Effect Unit
main = pure unit
