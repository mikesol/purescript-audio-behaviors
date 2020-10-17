module FRP.Behavior.Audio.Example.Stress0 where

-- with four oscillators and several gains, we start to hear pretty bad jank
-- named units clears it up completely!
import Prelude
import Data.Array (head, last, range, span)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioInfo, AudioParameter(..), AudioUnit, Exporter, Time'AudioInstructions(..), VisualInfo, defaultExporter, gain', gainT', gainT_', gain_', runInBrowser, sinOsc, sinOsc_, speaker, speaker_)
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

split :: ∀ t12 t13. Ord t12 ⇒ t12 → Array (Tuple t12 t13) → { init ∷ Array (Tuple t12 t13), rest ∷ Array (Tuple t12 t13) }
split s p = span ((s >= _) <<< fst) p

gn :: Number → Array (Tuple Number Number) → AudioParameter Number
gn s p =
  let
    ht = split s p
  in
    let
      left = fromMaybe (Tuple 0.0 0.0) $ last ht.init
    in
      let
        right = fromMaybe (Tuple 101.0 0.0) $ head ht.rest
      in
        -- if we are in a control cycle with a peak or trough
        -- we lock to that
        -- otherwise, we interpolate
        if (fst right - s) < kr then
          AudioParameter { param: (snd right), timeOffset: (fst right - s) }
        else
          let
            m = (snd right - snd left) / (fst right - fst left)
          in
            let
              b = (snd right - (m * fst right))
            in
              AudioParameter { param: (m * s + b), timeOffset: 0.0 }

sceneThatHitsDeadline :: Behavior Number -> Behavior (AudioUnit D1)
sceneThatHitsDeadline time = f <$> time
  where
  f s =
    speaker
      ( (gain' 0.1 (gainT' (gn s pwf0) $ sinOsc 440.0))
          :| Nil
      )

scene :: Number -> Behavior (AudioUnit D1)
scene s =
  pure
    $ speaker
        ( (gain' 0.1 (gainT' (gn s pwf0) $ sinOsc 440.0))
            :| (gain' 0.1 (gainT' (gn s pwf1) $ sinOsc 660.0))
            : (gain' 0.1 (gainT' (gn s pwf2) $ sinOsc 990.0))
            : (gain' 0.1 (gainT' (gn s pwf3) $ sinOsc 220.0))
            : Nil
        )

sceneN :: Number -> Behavior (AudioUnit D1)
sceneN s =
  pure
    $ speaker_ "speaker"
        ( (gain_' "g0" 0.1 (gainT_' "gt0" (gn s pwf0) $ sinOsc_ "s0" 440.0))
            :| (gain_' "g1" 0.1 (gainT_' "gt1" (gn s pwf1) $ sinOsc_ "s1" 660.0))
            : (gain_' "g2" 0.1 (gainT_' "gt2" (gn s pwf2) $ sinOsc_ "s2" 990.0))
            : (gain_' "g3" 0.1 (gainT_' "gt3" (gn s pwf3) $ sinOsc_ "s3" 220.0))
            : Nil
        )

sceneNN :: Number -> Behavior (AudioUnit D1)
sceneNN s =
  pure
    $ speaker_ "speaker"
        ( (gain_' "g0" 0.1 (gainT_' "gt0" (gn s pwf0) $ sinOsc_ "s0" 440.0))
            :| (gain_' "g1" 0.1 (gainT_' "gt1" (gn s pwf1) $ sinOsc_ "s1" 660.0))
            : (gain_' "g2" 0.1 (gainT_' "gt2" (gn s pwf2) $ sinOsc_ "s2" 990.0))
            : (gain_' "g3" 0.1 (gainT_' "gt3" (gn s pwf3) $ sinOsc_ "s3" 220.0))
            : (gain_' "g4" 0.05 (gainT_' "gt4" (gn s pwf1) $ sinOsc_ "s4" 1210.0))
            : (gain_' "g5" 0.025 (gainT_' "gt5" (gn s pwf0) $ sinOsc_ "s5" 1580.0))
            : Nil
        )

run ::
  forall microphone track buffer floatArray periodicWave.
  Unit ->
  Int ->
  Int ->
  AudioContext ->
  AudioInfo (Object microphone) (Object track) (Object buffer) (Object floatArray) (Object periodicWave) ->
  VisualInfo ->
  Exporter Unit Time'AudioInstructions ->
  Effect (Effect Unit)
run = runInBrowser sceneNN

exporter = defaultExporter

main :: Effect Unit
main = pure unit
