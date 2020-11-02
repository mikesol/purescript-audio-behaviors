module FRP.Behavior.Audio.Example.Koans where

import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D1, D2)
import Data.Vec ((+>), empty)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioInfo, AudioParameterTransition(..), AudioUnit, EngineInfo, Exporter, Oversample(..), VisualInfo, allpass, bandpass, convolver, defaultExporter, defaultParam, delay, dup1, dynamicsCompressor, g'add, g'bandpass, g'delay, g'gain, gain', graph, highpass, highshelf, iirFilter, loopBuf, loopBufT, lowpass, lowshelf, merger, microphone, notch, panner, pannerMono, pannerVars', peaking, periodicOsc, play, playBuf, playBufWithOffset, playBuf_, play_, runInBrowser, sawtoothOsc, sinOsc, spatialPanner, speaker, speaker', squareOsc, triangleOsc, waveShaper)
import Foreign.Object (Object)
import Math (pi, sin)
import Record.Extra (SLProxy(..), SNil)
import Type.Data.Graph (type (:/))

-- constant
nothing :: Number -> Behavior (AudioUnit D1)
nothing _ = pure zero

-- triangle
triangle :: Number -> Behavior (AudioUnit D1)
triangle _ = pure $ speaker' (gain' 0.3 $ triangleOsc 420.0)

-- saw
saw :: Number -> Behavior (AudioUnit D1)
saw _ = pure $ speaker' (gain' 0.3 $ sawtoothOsc 420.0)

-- fixed periodic wave
pdfix :: Number -> Behavior (AudioUnit D1)
pdfix _ = pure $ speaker' (gain' 0.3 $ periodicOsc "funtimes" 325.0)

-- fixed periodic wave
wsh :: Number -> Behavior (AudioUnit D1)
wsh _ = pure $ speaker' (gain' 0.3 (waveShaper "waveshaperCurve" FourX $ (play "forest")))

-- square
square :: Number -> Behavior (AudioUnit D1)
square _ = pure $ speaker' (gain' 0.3 $ squareOsc 420.0)

-- comp
comp :: Number -> Behavior (AudioUnit D1)
comp _ = pure $ speaker' (gain' 0.3 (dynamicsCompressor (-50.0) 40.0 12.0 0.0 0.25 $ (play "forest")))

-- verb
verb :: Number -> Behavior (AudioUnit D1)
verb _ = pure $ speaker' (gain' 0.3 (convolver "moo" $ (play "forest")))

-- delay
wait :: Number -> Behavior (AudioUnit D1)
wait time =
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
ringMod :: Number -> Behavior (AudioUnit D1)
ringMod _ =
  pure
    $ speaker'
        ( (gain' 0.5 $ sinOsc (440.0))
            * (gain' 0.5 $ sinOsc (30.0))
        )

-- filters
f0 :: Number -> Behavior (AudioUnit D1)
f0 _ = pure $ speaker' (gain' 0.3 (lowpass 350.0 1.0 $ (play "forest")))

f1 :: Number -> Behavior (AudioUnit D1)
f1 _ = pure $ speaker' (gain' 0.3 (highpass 350.0 1.0 $ (play "forest")))

f2 :: Number -> Behavior (AudioUnit D1)
f2 _ = pure $ speaker' (gain' 0.3 (lowshelf 350.0 0.0 $ (play "forest")))

f3 :: Number -> Behavior (AudioUnit D1)
f3 _ = pure $ speaker' (gain' 0.3 (highshelf 350.0 0.0 $ (play "forest")))

f4 :: Number -> Behavior (AudioUnit D1)
f4 _ = pure $ speaker' (gain' 0.3 (bandpass 350.0 1.0 $ (play "forest")))

f5 :: Number -> Behavior (AudioUnit D1)
f5 _ = pure $ speaker' (gain' 0.3 (allpass 350.0 1.0 $ (play "forest")))

f6 :: Number -> Behavior (AudioUnit D1)
f6 _ = pure $ speaker' (gain' 0.3 (peaking 350.0 1.0 0.0 $ (play "forest")))

f7 :: Number -> Behavior (AudioUnit D1)
f7 _ = pure $ speaker' (gain' 0.3 (notch 350.0 1.0 $ (play "forest")))

iir :: Number -> Behavior (AudioUnit D1)
iir _ =
  pure
    $ speaker'
        ( gain' 0.3
            ( iirFilter
                (0.00020298 +> 0.0004059599 +> 0.00020298 +> empty)
                (1.0126964558 +> (-1.9991880801) +> 0.9873035442 +> empty)
                (play "forest")
            )
        )

-- panner, merger, dup
pan :: Number -> Behavior (AudioUnit D2)
pan time =
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

panMono :: Number -> Behavior (AudioUnit D2)
panMono time =
  pure
    $ speaker'
        ( pannerMono (sin rad)
            ( (gain' 0.2 $ sinOsc 110.0)
                + (gain' 0.1 $ sinOsc 220.0)
            )
        )
  where
  rad = pi * time

-- spatialPanner
span :: Number -> Behavior (AudioUnit D2)
span t =
  pure
    $ speaker'
        ( spatialPanner
            pannerVars'
              { positionX = sin (0.2 * rad)
              , positionY = sin (0.1 * rad)
              , positionZ = sin (0.3 * rad)
              , orientationX = sin (0.05 * rad)
              , orientationY = sin (0.15 * rad)
              , orientationZ = sin (0.19 * rad)
              }
            (play_ "f0" "forest")
        )
  where
  rad = pi * t

-- pb
pb :: Number -> Behavior (AudioUnit D1)
pb _ = pure $ speaker' (gain' 0.3 (playBuf "moo" 1.0))

-- pb
pbO :: Number -> Behavior (AudioUnit D1)
pbO _ = pure $ speaker' (gain' 0.3 (playBufWithOffset "moo" 1.0 1.0))

-- lb
lb :: Number -> Behavior (AudioUnit D1)
lb _ = pure $ speaker' (gain' 0.3 (loopBuf "moo" 1.0 0.0 0.5))

-- transitions
-- lb
tran :: AudioParameterTransition -> Number -> Behavior (AudioUnit D1)
tran transition time =
  pure
    $ speaker'
        ( gain' 0.3
            ( loopBufT
                "moo"
                (defaultParam { param = 1.0 + 0.3 * (sin (0.2 * pi * time)), transition = transition })
                0.0
                0.0
            )
        )

-- on off
onoff :: Number -> Behavior (AudioUnit D1)
onoff t =
  pure
    $ speaker
        ( zero
            :| (if t < 1.0 then (pure $ gain' 0.3 (play_ "f0" "forest")) else Nil <> (if t > 3.0 then (pure $ gain' 0.3 (play_ "f1" "forest")) else Nil))
            <> Nil
        )

-- on off
onoffb :: Number -> Behavior (AudioUnit D1)
onoffb t =
  pure
    $ speaker
        ( zero
            :| (if t < 9.0 then (pure $ gain' 0.3 (playBuf_ "f0" "moo" 1.0)) else Nil)
            <> (if t > 3.0 then (pure $ gain' 0.3 (playBuf_ "f1" "moo" 1.0)) else Nil)
        )

-- feedback
feedback :: Number -> Behavior (AudioUnit D1)
feedback _ =
  pure
    ( speaker'
        $ ( graph
              { aggregators:
                  { out: Tuple g'add (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple g'add (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain 0.5) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay 0.2) (SProxy :: SProxy "filt")
                  , filt: Tuple (g'bandpass 440.0 1.0) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: microphone
                  }
              }
          )
    )

run ::
  forall microphone track buffer floatArray periodicWave.
  Unit ->
  AudioContext ->
  EngineInfo ->
  AudioInfo (Object microphone) (Object track) (Object buffer) (Object floatArray) (Object periodicWave) ->
  VisualInfo ->
  Exporter Unit Unit ->
  Effect (Effect Unit)
run = runInBrowser (tran ExponentialRamp)

exporter = defaultExporter :: Exporter Unit Unit

main :: Effect Unit
main = pure unit
