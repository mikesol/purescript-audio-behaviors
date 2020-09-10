module Test.Main where

import Prelude
import Data.Array (range, replicate, zipWith)
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Behavior.Audio (AudioProcessor, SampleFrame, audioIO, audioIOInterleaved)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

frameZip :: SampleFrame -> SampleFrame -> SampleFrame
frameZip = zipWith (zipWith (+))

mulSampleFrame :: Number -> SampleFrame -> SampleFrame
mulSampleFrame n = map (map (_ * n))

simpleProcessor :: forall (r :: # Type). AudioProcessor r
simpleProcessor _ audio params = mulSampleFrame 0.25 <$> (audio 0.0)

delayProcessor :: forall (r :: # Type). AudioProcessor r
delayProcessor _ audio params = frameZip <$> (mulSampleFrame 0.25 <$> (audio 0.0)) <*> (mulSampleFrame 0.5 <$> (audio 1.0))

main ∷ Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Simple audio" do
          it "should multiply correctly" do
            aud <- liftEffect $ audioIO simpleProcessor 44100 16 {} 0 [ [ replicate 16 1.0 ] ]
            aud `shouldEqual` [ [ replicate 16 0.25 ] ]
          it "should multiply correctly when sink exists" do
            aud <- liftEffect $ audioIO simpleProcessor 44100 16 {} 0 [ [ replicate 128 0.125 <> replicate 16 1.0 ] ]
            aud `shouldEqual` [ [ replicate 16 0.25 ] ]
        describe "Simple audio" do
          it "should add delay" do
            aud <- liftEffect $ audioIO delayProcessor 16 16 {} 0 [ [ replicate 16 0.125 <> replicate 16 1.0 ] ]
            aud `shouldEqual` [ [ replicate 16 0.3125 ] ]
        describe "Interleaved simple audio" do
          it "should multiply correctly" do
            aud <-
              liftEffect
                $ audioIOInterleaved
                    delayProcessor
                    44100
                    {}
                    0
                    2
                    16
                    ( map ((_ / 16.0) <<< toNumber) (range 0 15)
                        <> map ((_ / 32.0) <<< toNumber) (range 0 15)
                        <> map ((_ / 64.0) <<< toNumber) (range 0 15)
                        <> map ((_ / 128.0) <<< toNumber) (range 0 15)
                    )
            aud
              `shouldEqual`
                ( map ((_ * 0.25 / 64.0) <<< toNumber) (range 0 15)
                    <> map ((_ * 0.25 / 128.0) <<< toNumber) (range 0 15)
                )
        describe "Interleaved delayed audio" do
          it "should add delay" do
            aud <-
              liftEffect
                $ audioIOInterleaved
                    delayProcessor
                    16
                    {}
                    0
                    2
                    16
                    ( map ((_ / 16.0) <<< toNumber) (range 0 15)
                        <> map ((_ / 32.0) <<< toNumber) (range 0 15)
                        <> map ((_ / 64.0) <<< toNumber) (range 0 15)
                        <> map ((_ / 128.0) <<< toNumber) (range 0 15)
                    )
            aud
              `shouldEqual`
                ( map
                    ((\i -> (i * 0.25 / 64.0) + (i * 0.5 / 16.0)) <<< toNumber)
                    (range 0 15)
                    <> map
                        ((\i -> (i * 0.25 / 128.0) + (i * 0.5 / 32.0)) <<< toNumber)
                        (range 0 15)
                )
