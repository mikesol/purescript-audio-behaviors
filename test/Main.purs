module Test.Main where

import Prelude
import Data.Array (range, replicate, zipWith)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as DL
import Data.Map (fromFoldable)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Data.NonEmpty as NE
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Behavior.Audio (AudioProcessor, AudioUnit'(..), SampleFrame, audioIO, audioIOInterleaved, audioToPtr, gain, sinOsc, speaker)
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
        describe "Audio tree" do
          it "should correctly transform simple tree" do
            let
              tree =
                audioToPtr
                  ( speaker
                      ( (sinOsc 440.0)
                          :| (DL.fromFoldable (replicate 3 (sinOsc 440.0)))
                      )
                  )
            tree
              `shouldEqual`
                { flat:
                    ( fromFoldable
                        [ (Tuple 0 { au: Speaker', iChan: 1, next: Nil, oChan: 1, prev: (4 : 3 : 2 : 1 : Nil), ptr: 0 })
                        , (Tuple 1 { au: (SinOsc' 440.0), iChan: 1, next: (0 : Nil), oChan: 1, prev: Nil, ptr: 1 })
                        , (Tuple 2 { au: (SinOsc' 440.0), iChan: 1, next: (0 : Nil), oChan: 1, prev: Nil, ptr: 2 })
                        , (Tuple 3 { au: (SinOsc' 440.0), iChan: 1, next: (0 : Nil), oChan: 1, prev: Nil, ptr: 3 })
                        , (Tuple 4 { au: (SinOsc' 440.0), iChan: 1, next: (0 : Nil), oChan: 1, prev: Nil, ptr: 4 })
                        ]
                    )
                , init: (4 : 3 : 2 : 1 : Nil)
                , len: 5
                , p: { au: Speaker', iChan: 1, next: Nil, oChan: 1, prev: (4 : 3 : 2 : 1 : Nil), ptr: 0 }
                }
          it "should correctly transform dense tree" do
            let
              tree =
                audioToPtr
                  $ speaker
                      ( NE.singleton
                          ( ( gain 1.0
                                ( (sinOsc 440.0)
                                    :| (DL.fromFoldable (replicate 2 (sinOsc 441.0)))
                                )
                            )
                              + ( gain 0.9
                                    ( (sinOsc 442.0)
                                        :| (DL.fromFoldable (replicate 2 (sinOsc 443.0)))
                                    )
                                )
                          )
                      )
            tree
              `shouldEqual`
                { flat:
                    ( fromFoldable
                        [ (Tuple 0 { au: Speaker', iChan: 1, next: Nil, oChan: 1, prev: (1 : Nil), ptr: 0 })
                        , (Tuple 1 { au: Add', iChan: 1, next: (0 : Nil), oChan: 1, prev: (6 : 2 : Nil), ptr: 1 })
                        , (Tuple 2 { au: (Gain' 1.0), iChan: 1, next: (1 : Nil), oChan: 1, prev: (5 : 4 : 3 : Nil), ptr: 2 })
                        , ( Tuple 3
                              { au:
                                  ( SinOsc'
                                      440.0
                                  )
                              , iChan: 1
                              , next: (2 : Nil)
                              , oChan: 1
                              , prev: Nil
                              , ptr: 3
                              }
                          )
                        , (Tuple 4 { au: (SinOsc' 441.0), iChan: 1, next: (2 : Nil), oChan: 1, prev: Nil, ptr: 4 })
                        , (Tuple 5 { au: (SinOsc' 441.0), iChan: 1, next: (2 : Nil), oChan: 1, prev: Nil, ptr: 5 })
                        , (Tuple 6 { au: (Gain' 0.9), iChan: 1, next: (1 : Nil), oChan: 1, prev: (9 : 8 : 7 : Nil), ptr: 6 })
                        , (Tuple 7 { au: (SinOsc' 442.0), iChan: 1, next: (6 : Nil), oChan: 1, prev: Nil, ptr: 7 })
                        , (Tuple 8 { au: (SinOsc' 443.0), iChan: 1, next: (6 : Nil), oChan: 1, prev: Nil, ptr: 8 })
                        , ( Tuple 9
                              { au: (SinOsc' 443.0), iChan: 1, next: (6 : Nil), oChan: 1, prev: Nil, ptr: 9 }
                          )
                        ]
                    )
                , init:
                    ( 9 : 8 : 7 : 5
                        : 4
                        : 3
                        : Nil
                    )
                , len: 10
                , p: { au: Speaker', iChan: 1, next: Nil, oChan: 1, prev: (1 : Nil), ptr: 0 }
                }
