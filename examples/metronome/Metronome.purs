module FRP.Behavior.Audio.Example.Metronome where

import Prelude
import Data.Array (head, last, range, span)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, Instruction, gain', runInBrowser, sinOsc, speaker')
import FRP.Behavior.Audio as Aud
import Foreign (Foreign)

pwf :: Array (Tuple Number Number)
pwf =
  join
    $ map
        ( \i ->
            map
              ( \(Tuple f s) ->
                  Tuple (f + toNumber i) s
              )
              [ Tuple 0.0 0.0, Tuple 0.1 0.9, Tuple 0.3 0.3 ]
        )
        (range 0 100)

scene :: Behavior Number -> Behavior (AudioUnit D1)
scene time = f <$> time
  where
  split s = span ((s >= _) <<< fst) pwf

  gn s =
    let
      ht = split s
    in
      let
        left = fromMaybe (Tuple 0.0 0.0) $ last ht.init
      in
        let
          right = fromMaybe (Tuple 101.0 0.0) $ head ht.rest
        in
          let
            m = (snd right - snd left) / (fst right - fst left)
          in
            let
              b = (snd right - (m * fst right))
            in
              m * s + b

  f s = speaker' (gain' (gn s) $ sinOsc 440.0)

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
run = runInBrowser scene

touchAudio :: Number -> Array Instruction → Foreign → Foreign → Sources → Array Foreign → Effect (Array Foreign)
touchAudio = Aud.touchAudio

makeWorkers :: Int -> Effect (Array Foreign)
makeWorkers = Aud.makeWorkers

main :: Effect Unit
main = pure unit
