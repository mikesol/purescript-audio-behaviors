module FRP.Behavior.Audio.Example.HelloWorld where

import Prelude
import Data.Newtype (unwrap)
import Data.Typelevel.Num (D1)
import Effect (Effect)
import Data.NonEmpty ((:|))
import Data.List ((:), List(..))
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, Instruction, gain', runInBrowser, sinOsc, speaker)
import FRP.Behavior.Audio as Aud
import FRP.Behavior.Time (seconds)
import Foreign (Foreign)
import Math (pi, sin)

scene :: Behavior (AudioUnit D1)
scene = f <$> (unwrap <$> seconds)
  where
  f s =
    let
      rad = pi * s
    in
      speaker
        $ ( (gain' 0.1 $ sinOsc (440.0 + (10.0 * sin (2.3 * rad))))
              :| (gain' 0.25 $ sinOsc (235.0 + (10.0 * sin (1.7 * rad))))
              : (gain' 0.2 $ sinOsc (337.0 + (10.0 * sin rad)))
              : Nil
          )

run ::
  Int ->
  Foreign ->
  Array Foreign ->
  (Number -> Array Instruction -> Foreign -> Array Foreign -> Effect (Array Foreign)) ->
  Effect (Effect Unit)
run = runInBrowser scene

touchAudio :: Array Instruction → Foreign → Array Foreign → Effect (Array Foreign)
touchAudio = Aud.touchAudio

main :: Effect Unit
main = pure unit
