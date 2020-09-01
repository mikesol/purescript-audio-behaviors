module Client.Main where

import Prelude
import Data.Array (zipWith)
import Effect (Effect)
import FRP.Behavior.Audio (AudioProcessor, SampleFrame, makeAudioWorkletProcessor)

frameZip :: SampleFrame -> SampleFrame -> SampleFrame
frameZip = zipWith (zipWith (+))

mulSampleFrame :: Number -> SampleFrame -> SampleFrame
mulSampleFrame n = map (map (_ * n))

echoProcessor :: AudioProcessor {}
echoProcessor _ audio params = do
  frameZip
    <$> ( frameZip
          <$> audio 0.0 -- source delayed by 0.0 sec
          <*> (map (mulSampleFrame 0.1) $ audio 0.25) -- 0.1 amp, delayed by 0.25 sec
      )
    <*> (map (mulSampleFrame 0.05) $ audio 0.5) -- 0.05 amp, delayed by 0.5 sec

main :: Effect Unit
main = makeAudioWorkletProcessor "simple-echo" 3.0 {} echoProcessor
