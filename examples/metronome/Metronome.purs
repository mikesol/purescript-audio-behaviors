module FRP.Behavior.Audio.Example.Metronome where

import Prelude
import Data.Array (head, last, range, span)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, gain', gainT', runInBrowser, sinOsc, speaker')

-- a piecewise function that creates an attack/release/sustain envelope
-- at a periodicity of every 0.9 seconds
pwf :: Array (Tuple Number Number)
pwf =
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

kr = 20.0 / 1000.0 :: Number -- the control rate in seconds, or 66.66667 Hz

scene :: Number -> Behavior (AudioUnit D1)
scene time = pure $ speaker' (gain' 0.1 (gainT' (gn time) $ sinOsc 440.0))
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

run = runInBrowser scene

main :: Effect Unit
main = pure unit
