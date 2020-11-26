module FRP.Behavior.Audio.Example.Stress0 where

-- with four oscillators and several gains, we start to hear pretty bad jank
-- named units clears it up completely!
import Prelude
import Data.Array (range)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioInfo, AudioParameter, AudioUnit, EngineInfo, Exporter, VisualInfo, RecorderSignature, defaultExporter, evalPiecewise, gain', gainT', gainT_', gain_', runInBrowser, sinOsc, sinOsc_, speaker, speaker_)
import Foreign.Object (Object)

pwf0 :: Array (Tuple Number Number)
pwf0 =
  join
    $ map
        ( \i ->
            map
              ( \(Tuple f s) ->
                  Tuple (f + 0.11 * toNumber i) s
              )
              [ Tuple 0.0 0.0, Tuple 0.02 0.7, Tuple 0.06 0.2 ]
        )
        (range 0 400)

pwf1 :: Array (Tuple Number Number)
pwf1 =
  join
    $ map
        ( \i ->
            map
              ( \(Tuple f s) ->
                  Tuple (f + 0.13 * toNumber i) s
              )
              [ Tuple 0.0 0.0, Tuple 0.02 0.7, Tuple 0.06 0.2 ]
        )
        (range 0 400)

pwf2 :: Array (Tuple Number Number)
pwf2 =
  join
    $ map
        ( \i ->
            map
              ( \(Tuple f s) ->
                  Tuple (f + 0.15 * toNumber i) s
              )
              [ Tuple 0.0 0.0, Tuple 0.02 0.7, Tuple 0.06 0.2 ]
        )
        (range 0 400)

pwf3 :: Array (Tuple Number Number)
pwf3 =
  join
    $ map
        ( \i ->
            map
              ( \(Tuple f s) ->
                  Tuple (f + 0.9 * toNumber i) s
              )
              [ Tuple 0.0 0.0, Tuple 0.03 0.7, Tuple 0.07 0.2 ]
        )
        (range 0 400)

kr = 20.0 / 1000.0 :: Number -- the control rate in seconds, or 66.66667 Hz

sceneThatHitsDeadline :: Behavior Number -> Behavior (AudioUnit D1)
sceneThatHitsDeadline time = f <$> time
  where
  f s =
    speaker
      ( (gain' 0.1 (gainT' (epwf pwf0 s) $ sinOsc 440.0))
          :| Nil
      )

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

scene :: Number -> Behavior (AudioUnit D1)
scene s =
  pure
    $ speaker
        ( (gain' 0.1 (gainT' (epwf pwf0 s) $ sinOsc 440.0))
            :| (gain' 0.1 (gainT' (epwf pwf1 s) $ sinOsc 660.0))
            : (gain' 0.1 (gainT' (epwf pwf2 s) $ sinOsc 990.0))
            : (gain' 0.1 (gainT' (epwf pwf3 s) $ sinOsc 220.0))
            : Nil
        )

sceneN :: Number -> Behavior (AudioUnit D1)
sceneN s =
  pure
    $ speaker_ "speaker"
        ( (gain_' "g0" 0.1 (gainT_' "gt0" (epwf pwf0 s) $ sinOsc_ "s0" 440.0))
            :| (gain_' "g1" 0.1 (gainT_' "gt1" (epwf pwf1 s) $ sinOsc_ "s1" 660.0))
            : (gain_' "g2" 0.1 (gainT_' "gt2" (epwf pwf2 s) $ sinOsc_ "s2" 990.0))
            : (gain_' "g3" 0.1 (gainT_' "gt3" (epwf pwf3 s) $ sinOsc_ "s3" 220.0))
            : Nil
        )

sceneNN :: Number -> Behavior (AudioUnit D1)
sceneNN s =
  pure
    $ speaker_ "speaker"
        ( (gain_' "g0" 0.1 (gainT_' "gt0" (epwf pwf0 s) $ sinOsc_ "s0" 440.0))
            :| (gain_' "g1" 0.1 (gainT_' "gt1" (epwf pwf1 s) $ sinOsc_ "s1" 660.0))
            : (gain_' "g2" 0.1 (gainT_' "gt2" (epwf pwf2 s) $ sinOsc_ "s2" 990.0))
            : (gain_' "g3" 0.1 (gainT_' "gt3" (epwf pwf3 s) $ sinOsc_ "s3" 220.0))
            : (gain_' "g4" 0.05 (gainT_' "gt4" (epwf pwf1 s) $ sinOsc_ "s4" 1210.0))
            : (gain_' "g5" 0.025 (gainT_' "gt5" (epwf pwf0 s) $ sinOsc_ "s5" 1580.0))
            : Nil
        )

run ::
  forall microphone recorder track buffer floatArray periodicWave.
  Unit ->
  AudioContext ->
  EngineInfo ->
  AudioInfo (Object microphone) (Object (RecorderSignature recorder)) (Object track) (Object buffer) (Object floatArray) (Object periodicWave) ->
  VisualInfo ->
  Exporter Unit Unit ->
  Effect (Effect Unit)
run = runInBrowser sceneNN

exporter = defaultExporter :: Exporter Unit Unit

main :: Effect Unit
main = pure unit
