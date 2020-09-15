module Test.Main where

import Prelude
import Data.Array (range, replicate, zipWith)
import Data.Int (toNumber)
import Data.List (List(..), (:), tail, length)
import Data.List as DL
import Data.Map (fromFoldable)
import Data.Map as M
import Data.Vec ((+>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty ((:|), NonEmpty(..))
import Data.Tuple (Tuple(..))
import Data.Vec as V
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Behavior.Audio (AudioProcessor, AudioUnit'(..), AudioUnit''(..), SampleFrame, Status(..), audioGrouper, audioIO, audioIOInterleaved, audioToPtr, gain, gain', merger, sinOsc, speaker, speaker', splitter, swap)
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
                        [ (Tuple 0 { au: Speaker', iChan: 1, name: Nothing, next: (fromFoldable []), oChan: 1, prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 1), (Tuple 3 1), (Tuple 4 1) ]), ptr: 0, status: On })
                        , (Tuple 1 { au: (SinOsc' 440.0), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), oChan: 1, prev: (fromFoldable [ (Tuple 1 0) ]), ptr: 1, status: On })
                        , (Tuple 2 { au: (SinOsc' 440.0), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), oChan: 1, prev: (fromFoldable [ (Tuple 2 0) ]), ptr: 2, status: On })
                        , (Tuple 3 { au: (SinOsc' 440.0), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), oChan: 1, prev: (fromFoldable [ (Tuple 3 0) ]), ptr: 3, status: On })
                        , (Tuple 4 { au: (SinOsc' 440.0), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), oChan: 1, prev: (fromFoldable [ (Tuple 4 0) ]), ptr: 4, status: On })
                        ]
                    )
                , len: 5
                , p:
                    { au: Speaker'
                    , iChan: 1
                    , name: Nothing
                    , next: (fromFoldable [])
                    , oChan: 1
                    , prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 1), (Tuple 3 1), (Tuple 4 1) ])
                    , ptr: 0
                    , status: On
                    }
                }
          it "should correctly transform dense tree" do
            let
              tree =
                audioToPtr
                  $ speaker'
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
            tree
              `shouldEqual`
                { flat:
                    ( fromFoldable
                        [ (Tuple 0 { au: Speaker', iChan: 1, name: Nothing, next: (fromFoldable []), oChan: 1, prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 2), (Tuple 7 3), (Tuple 8 3), (Tuple 9 3) ]), ptr: 0, status: On })
                        , (Tuple 1 { au: Add', iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1), (Tuple 3 2), (Tuple 4 2), (Tuple 5 2), (Tuple 6 1), (Tuple 7 2), (Tuple 8 2), (Tuple 9 2) ]), ptr: 1, status: On })
                        , (Tuple 2 { au: (Gain' 1.0), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1), (Tuple 1 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1) ]), ptr: 2, status: On })
                        , (Tuple 3 { au: (SinOsc' 440.0), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), oChan: 1, prev: (fromFoldable [ (Tuple 3 0) ]), ptr: 3, status: On })
                        , (Tuple 4 { au: (SinOsc' 441.0), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), oChan: 1, prev: (fromFoldable [ (Tuple 4 0) ]), ptr: 4, status: On })
                        , (Tuple 5 { au: (SinOsc' 441.0), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), oChan: 1, prev: (fromFoldable [ (Tuple 5 0) ]), ptr: 5, status: On })
                        , (Tuple 6 { au: (Gain' 0.9), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1), (Tuple 1 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 6 0), (Tuple 7 1), (Tuple 8 1), (Tuple 9 1) ]), ptr: 6, status: On })
                        , (Tuple 7 { au: (SinOsc' 442.0), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 6 1) ]), oChan: 1, prev: (fromFoldable [ (Tuple 7 0) ]), ptr: 7, status: On })
                        , (Tuple 8 { au: (SinOsc' 443.0), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 6 1) ]), oChan: 1, prev: (fromFoldable [ (Tuple 8 0) ]), ptr: 8, status: On })
                        , (Tuple 9 { au: (SinOsc' 443.0), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 6 1) ]), oChan: 1, prev: (fromFoldable [ (Tuple 9 0) ]), ptr: 9, status: On })
                        ]
                    )
                , len: 10
                , p:
                    { au: Speaker'
                    , iChan: 1
                    , name: Nothing
                    , next: (fromFoldable [])
                    , oChan: 1
                    , prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 2), (Tuple 7 3), (Tuple 8 3), (Tuple 9 3) ])
                    , ptr: 0
                    , status: On
                    }
                }
          it "should correctly split" do
            let
              tree =
                audioToPtr
                  $ splitter
                      ( swap
                          (0 +> 0 +> 0 +> V.empty)
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
                      ( \v ->
                          speaker'
                            ( gain' 0.5
                                $ ( merger
                                      (gain' 0.3 (V.head v) :| map (gain' 0.3) (fromMaybe Nil $ tail (V.toUnfoldable v)))
                                  )
                            )
                      )
            tree
              `shouldEqual`
                { flat:
                    ( fromFoldable
                        [ (Tuple 0 { au: Speaker', iChan: 1, name: Nothing, next: (fromFoldable []), oChan: 1, prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 0, status: On })
                        , (Tuple 1 { au: (Gain' 0.5), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1), (Tuple 3 2), (Tuple 4 2), (Tuple 5 2), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 1, status: On })
                        , (Tuple 2 { au: Merger', iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1), (Tuple 1 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 2, status: On })
                        , (Tuple 3 { au: (Gain' 0.3), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 3 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 3, status: On })
                        , (Tuple 4 { au: (Gain' 0.3), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 4 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 4, status: On })
                        , (Tuple 5 { au: (Gain' 0.3), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 5 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 5, status: On })
                        , (Tuple 6 { au: Splitter', iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 3), (Tuple 2 2), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1) ]), oChan: 1, prev: (fromFoldable [ (Tuple 6 0), (Tuple 7 1), (Tuple 8 2), (Tuple 9 3), (Tuple 10 4), (Tuple 11 4), (Tuple 12 4), (Tuple 13 3), (Tuple 14 4), (Tuple 15 4), (Tuple 16 4) ]), ptr: 6, status: On })
                        , (Tuple 7 { au: Swap', iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2) ]), oChan: 3, prev: (fromFoldable [ (Tuple 7 0), (Tuple 8 1), (Tuple 9 2), (Tuple 10 3), (Tuple 11 3), (Tuple 12 3), (Tuple 13 2), (Tuple 14 3), (Tuple 15 3), (Tuple 16 3) ]), ptr: 7, status: On })
                        , (Tuple 8 { au: Add', iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2), (Tuple 7 0) ]), oChan: 3, prev: (fromFoldable [ (Tuple 8 0), (Tuple 9 1), (Tuple 10 2), (Tuple 11 2), (Tuple 12 2), (Tuple 13 1), (Tuple 14 2), (Tuple 15 2), (Tuple 16 2) ]), ptr: 8, status: On })
                        , (Tuple 9 { au: (Gain' 1.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 8 0) ]), oChan: 3, prev: (fromFoldable [ (Tuple 9 0), (Tuple 10 1), (Tuple 11 1), (Tuple 12 1) ]), ptr: 9, status: On })
                        , (Tuple 10 { au: (SinOsc' 440.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 10 0) ]), ptr: 10, status: On })
                        , (Tuple 11 { au: (SinOsc' 441.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 11 0) ]), ptr: 11, status: On })
                        , (Tuple 12 { au: (SinOsc' 441.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 12 0) ]), ptr: 12, status: On })
                        , (Tuple 13 { au: (Gain' 0.9), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 8 0) ]), oChan: 3, prev: (fromFoldable [ (Tuple 13 0), (Tuple 14 1), (Tuple 15 1), (Tuple 16 1) ]), ptr: 13, status: On })
                        , (Tuple 14 { au: (SinOsc' 442.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 14 0) ]), ptr: 14, status: On })
                        , (Tuple 15 { au: (SinOsc' 443.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 15 0) ]), ptr: 15, status: On })
                        , (Tuple 16 { au: (SinOsc' 443.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 16 0) ]), ptr: 16, status: On })
                        ]
                    )
                , len: 17
                , p:
                    { au: Splitter'
                    , iChan: 3
                    , name: Nothing
                    , next: (fromFoldable [ (Tuple 0 4), (Tuple 1 3), (Tuple 2 2), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1) ])
                    , oChan: 1
                    , prev:
                        ( fromFoldable
                            [ (Tuple 6 0)
                            , (Tuple 7 1)
                            , (Tuple 8 2)
                            , (Tuple 9 3)
                            , (Tuple 10 4)
                            , (Tuple 11 4)
                            , (Tuple 12 4)
                            , (Tuple 13 3)
                            , (Tuple 14 4)
                            , (Tuple 15 4)
                            , (Tuple 16 4)
                            ]
                        )
                    , ptr: 6
                    , status: On
                    }
                }
        describe "Audio grouper" do
          it "should group correctly" do
            let
              ag =
                audioGrouper
                  ( ({ au: Speaker', iChan: 1, name: Nothing, next: (fromFoldable []), oChan: 1, prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 0, status: On })
                      : ({ au: (Gain' 0.5), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1), (Tuple 3 2), (Tuple 4 2), (Tuple 5 2), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 1, status: On })
                      : ({ au: Merger', iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1), (Tuple 1 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 2, status: On })
                      : ({ au: (Gain' 0.3), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 3 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 3, status: On })
                      : ({ au: (Gain' 0.3), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 4 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 4, status: On })
                      : ({ au: (Gain' 0.3), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 5 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 5, status: On })
                      : ({ au: Splitter', iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 3), (Tuple 2 2), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1) ]), oChan: 1, prev: (fromFoldable [ (Tuple 6 0), (Tuple 7 1), (Tuple 8 2), (Tuple 9 3), (Tuple 10 4), (Tuple 11 4), (Tuple 12 4), (Tuple 13 3), (Tuple 14 4), (Tuple 15 4), (Tuple 16 4) ]), ptr: 6, status: On })
                      : ({ au: Swap', iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2) ]), oChan: 3, prev: (fromFoldable [ (Tuple 7 0), (Tuple 8 1), (Tuple 9 2), (Tuple 10 3), (Tuple 11 3), (Tuple 12 3), (Tuple 13 2), (Tuple 14 3), (Tuple 15 3), (Tuple 16 3) ]), ptr: 7, status: On })
                      : ({ au: Add', iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2), (Tuple 7 0) ]), oChan: 3, prev: (fromFoldable [ (Tuple 8 0), (Tuple 9 1), (Tuple 10 2), (Tuple 11 2), (Tuple 12 2), (Tuple 13 1), (Tuple 14 2), (Tuple 15 2), (Tuple 16 2) ]), ptr: 8, status: On })
                      : ({ au: (Gain' 1.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 8 0) ]), oChan: 3, prev: (fromFoldable [ (Tuple 9 0), (Tuple 10 1), (Tuple 11 1), (Tuple 12 1) ]), ptr: 9, status: On })
                      : ({ au: (SinOsc' 440.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 10 0) ]), ptr: 10, status: On })
                      : ({ au: (SinOsc' 441.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 11 0) ]), ptr: 11, status: On })
                      : ({ au: (SinOsc' 441.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 12 0) ]), ptr: 12, status: On })
                      : ({ au: (Gain' 0.9), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 8 0) ]), oChan: 3, prev: (fromFoldable [ (Tuple 13 0), (Tuple 14 1), (Tuple 15 1), (Tuple 16 1) ]), ptr: 13, status: On })
                      : ({ au: (SinOsc' 442.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 14 0) ]), ptr: 14, status: On })
                      : ({ au: (SinOsc' 443.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 15 0) ]), ptr: 15, status: On })
                      : ({ au: (SinOsc' 443.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 16 0) ]), ptr: 16, status: On })
                      : Nil
                  )
            length (DL.fromFoldable ag) `shouldEqual` 8
            ag
              `shouldEqual`
                (fromFoldable [ (Tuple { iChan: 1, name: Nothing, oChan: 1, tag: Gain'' } (NonEmpty { au: (Gain' 0.5), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1), (Tuple 3 2), (Tuple 4 2), (Tuple 5 2), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 1, status: On } ({ au: (Gain' 0.3), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 3 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 3, status: On } : { au: (Gain' 0.3), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 4 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 4, status: On } : { au: (Gain' 0.3), iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 5 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 5, status: On } : Nil))), (Tuple { iChan: 1, name: Nothing, oChan: 1, tag: Merger'' } (NonEmpty { au: Merger', iChan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1), (Tuple 1 0) ]), oChan: 1, prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 2, status: On } Nil)), (Tuple { iChan: 1, name: Nothing, oChan: 1, tag: Speaker'' } (NonEmpty { au: Speaker', iChan: 1, name: Nothing, next: (fromFoldable []), oChan: 1, prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8) ]), ptr: 0, status: On } Nil)), (Tuple { iChan: 3, name: Nothing, oChan: 1, tag: Splitter'' } (NonEmpty { au: Splitter', iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 3), (Tuple 2 2), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1) ]), oChan: 1, prev: (fromFoldable [ (Tuple 6 0), (Tuple 7 1), (Tuple 8 2), (Tuple 9 3), (Tuple 10 4), (Tuple 11 4), (Tuple 12 4), (Tuple 13 3), (Tuple 14 4), (Tuple 15 4), (Tuple 16 4) ]), ptr: 6, status: On } Nil)), (Tuple { iChan: 3, name: Nothing, oChan: 3, tag: Add'' } (NonEmpty { au: Add', iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2), (Tuple 7 0) ]), oChan: 3, prev: (fromFoldable [ (Tuple 8 0), (Tuple 9 1), (Tuple 10 2), (Tuple 11 2), (Tuple 12 2), (Tuple 13 1), (Tuple 14 2), (Tuple 15 2), (Tuple 16 2) ]), ptr: 8, status: On } Nil)), (Tuple { iChan: 3, name: Nothing, oChan: 3, tag: Gain'' } (NonEmpty { au: (Gain' 1.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 8 0) ]), oChan: 3, prev: (fromFoldable [ (Tuple 9 0), (Tuple 10 1), (Tuple 11 1), (Tuple 12 1) ]), ptr: 9, status: On } ({ au: (Gain' 0.9), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 8 0) ]), oChan: 3, prev: (fromFoldable [ (Tuple 13 0), (Tuple 14 1), (Tuple 15 1), (Tuple 16 1) ]), ptr: 13, status: On } : Nil))), (Tuple { iChan: 3, name: Nothing, oChan: 3, tag: SinOsc'' } (NonEmpty { au: (SinOsc' 440.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 10 0) ]), ptr: 10, status: On } ({ au: (SinOsc' 441.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 11 0) ]), ptr: 11, status: On } : { au: (SinOsc' 441.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 12 0) ]), ptr: 12, status: On } : { au: (SinOsc' 442.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 14 0) ]), ptr: 14, status: On } : { au: (SinOsc' 443.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 15 0) ]), ptr: 15, status: On } : { au: (SinOsc' 443.0), iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), oChan: 3, prev: (fromFoldable [ (Tuple 16 0) ]), ptr: 16, status: On } : Nil))), (Tuple { iChan: 3, name: Nothing, oChan: 3, tag: Swap'' } (NonEmpty { au: Swap', iChan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2) ]), oChan: 3, prev: (fromFoldable [ (Tuple 7 0), (Tuple 8 1), (Tuple 9 2), (Tuple 10 3), (Tuple 11 3), (Tuple 12 3), (Tuple 13 2), (Tuple 14 3), (Tuple 15 3), (Tuple 16 3) ]), ptr: 7, status: On } Nil)) ])
