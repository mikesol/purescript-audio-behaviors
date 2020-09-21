module FRP.Behavior.Audio.Example.Readme where

-- tests everything in the readme to make sure it works!
import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, Instruction, gain', runInBrowser, sinOsc, speaker, speaker')
import FRP.Behavior.Audio as Aud
import Foreign (Foreign)
import Math (pi, sin)

scene0 :: Behavior Number -> Behavior (AudioUnit D1)
scene0 _ = pure (speaker' $ sinOsc 440.0)

type Sources
  = {}

run ::
  Int ->
  Int ->
  Foreign ->
  Foreign ->
  Sources ->
  Array Foreign ->
  (Number -> Array Instruction -> Foreign -> Foreign -> Sources -> Array Foreign -> Effect (Array Foreign)) ->
  Effect (Effect Unit)
run = runInBrowser scene0

touchAudio :: Number -> Array Instruction → Foreign → Foreign → Sources → Array Foreign → Effect (Array Foreign)
touchAudio = Aud.touchAudio

makeWorkers :: Int -> Effect (Array Foreign)
makeWorkers = Aud.makeWorkers

main :: Effect Unit
main = pure unit
