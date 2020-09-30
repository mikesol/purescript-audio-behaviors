module FRP.Behavior.Audio.Example.HelloWorld where

import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, Instruction, gain', runInBrowser, sinOsc, speaker)
import FRP.Behavior.Audio as Aud
import Foreign (Foreign)
import Math (pi, sin)

scene :: Behavior Number -> Behavior (AudioUnit D1)
scene time = f <$> time
  where
  f s =
    let
      rad = pi * s
    in
      speaker
        $ ( (gain' 0.1 $ sinOsc (440.0 + (10.0 * sin (2.3 * rad))))
              :| (gain' 0.25 $ sinOsc (235.0 + (10.0 * sin (1.7 * rad))))
              : (gain' 0.2 $ sinOsc (337.0 + (10.0 * sin rad)))
              : (gain' 0.1 $ sinOsc (530.0 + (19.0 * (5.0 * sin rad))))
              : Nil
          )

type Sources
  = {}

run ::
  Int ->
  Int ->
  Foreign ->
  Foreign ->
  Sources ->
  (Number -> Array Instruction -> Foreign -> Foreign -> Sources -> Array Foreign -> Effect (Array Foreign)) ->
  Effect (Effect Unit)
run = runInBrowser scene

touchAudio :: Number -> Array Instruction → Foreign → Foreign → Sources → Array Foreign → Effect (Array Foreign)
touchAudio = Aud.touchAudio

main :: Effect Unit
main = pure unit
