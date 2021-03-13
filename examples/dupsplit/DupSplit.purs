module FRP.Behavior.Audio.Example.DupSplit where

-- tests everything in the readme to make sure it works!
import Prelude
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioInfo, AudioUnit, EngineInfo, Exporter, VisualInfo, RecorderSignature, defaultExporter, dup1, microphone, runInBrowser, sinOsc, speaker')
import Foreign.Object (Object)
import Math (pi, sin)

sceneDup :: Number -> Behavior (AudioUnit D1)
sceneDup t = pure (speaker' $ dup1 microphone (\u -> (sinOsc (5.0 + 10.0 * (sin (0.2 * t * pi))) * u)))

run = runInBrowser sceneDup

exporter = defaultExporter :: Exporter Unit Unit

main :: Effect Unit
main = pure unit
