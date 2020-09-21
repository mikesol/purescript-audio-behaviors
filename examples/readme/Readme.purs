module FRP.Behavior.Audio.Example.Readme where

-- tests everything in the readme to make sure it works!
import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Typelevel.Num (D1, D2)
import Effect (Effect)
import Data.Vec ((+>), empty)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, Instruction, merger, panner, gain', microphone, play, runInBrowser, sinOsc, speaker, dup1, speaker')
import FRP.Behavior.Audio as Aud
import Foreign (Foreign)
import Math (pi, sin)

scene0 :: Behavior Number -> Behavior (AudioUnit D1)
scene0 _ = pure (speaker' $ (gain' 0.5 $ sinOsc 440.0))

scene1 :: Behavior Number -> Behavior (AudioUnit D1)
scene1 _ =
  pure
    ( speaker
        $ ( (gain' 0.2 $ sinOsc 110.0)
              :| (gain' 0.1 $ sinOsc 220.0)
              : microphone
              : Nil
          )
    )

scene2 :: Behavior Number -> Behavior (AudioUnit D1)
scene2 _ =
  pure
    ( speaker
        $ ( (gain' 0.2 $ sinOsc 110.0)
              :| (gain' 0.1 $ sinOsc 220.0)
              : (gain' 0.5 $ (play "forest"))
              : microphone
              : Nil
          )
    )

scene3 :: Behavior Number -> Behavior (AudioUnit D2)
scene3 _ =
  pure
    $ dup1
        ( (gain' 0.2 $ sinOsc 110.0)
            + (gain' 0.1 $ sinOsc 220.0)
            + microphone
        ) \mono ->
        speaker
          $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                :| (gain' 0.5 $ (play "forest"))
                : Nil
            )

scene4 :: Behavior Number -> Behavior (AudioUnit D2)
scene4 time = f <$> time
  where
  f s =
    let
      rad = pi * s
    in
      dup1
        ( (gain' 0.2 $ sinOsc (110.0 + (3.0 * sin (0.5 * rad))))
            + (gain' 0.1 $ sinOsc 220.0)
            + microphone
        ) \mono ->
        speaker
          $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                :| (gain' 0.5 $ (play "forest"))
                : Nil
            )

type Sources
  = { forest :: Foreign
    }

run ::
  Int ->
  Int ->
  Foreign ->
  Foreign ->
  Sources ->
  Array Foreign ->
  (Number -> Array Instruction -> Foreign -> Foreign -> Sources -> Array Foreign -> Effect (Array Foreign)) ->
  Effect (Effect Unit)
run = runInBrowser scene2

touchAudio :: Number -> Array Instruction → Foreign → Foreign → Sources → Array Foreign → Effect (Array Foreign)
touchAudio = Aud.touchAudio

makeWorkers :: Int -> Effect (Array Foreign)
makeWorkers = Aud.makeWorkers

main :: Effect Unit
main = pure unit
