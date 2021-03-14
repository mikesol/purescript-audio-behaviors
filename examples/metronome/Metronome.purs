module FRP.Behavior.Audio.Example.Metronome where

import Prelude
import Data.Array (range)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, Exporter, Run, defaultExporter, evalPiecewise, gain', gainT', runInBrowser, sinOsc, speaker')

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
scene time =
  pure
    $ speaker'
        (gain' 0.1 (gainT' (evalPiecewise kr pwf time) $ sinOsc 440.0))

run :: Run Unit Unit
run = runInBrowser scene

exporter = defaultExporter :: Exporter Unit Unit

main :: Effect Unit
main = pure unit
