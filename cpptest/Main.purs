module Main where

import Prelude
import Data.Array (replicate, zipWith)
import Effect (Effect)
import Effect.Class.Console (log)
import FRP.Behavior.Audio (AudioProcessor, SampleFrame, audioIO)

frameZip :: SampleFrame -> SampleFrame -> SampleFrame
frameZip = zipWith (zipWith (+))

mulSampleFrame :: Number -> SampleFrame -> SampleFrame
mulSampleFrame n = map (map (_ * n))

simpleProcessor :: forall (r :: # Type). AudioProcessor r
simpleProcessor _ audio params = mulSampleFrame 0.25 <$> (audio 0.0)

delayProcessor :: forall (r :: # Type). AudioProcessor r
delayProcessor _ audio params = frameZip <$> (mulSampleFrame 0.25 <$> (audio 0.0)) <*> (mulSampleFrame 0.5 <$> (audio 1.0))

main ∷ Effect Unit
main = do
  let
    audioIn = [ [ replicate 16 0.125 <> replicate 16 1.0 ] ]
  aud <- audioIO delayProcessor 16 16 {} 0 audioIn
  log $ show aud
