module Test.Basic where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (range, replicate, zipWith)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as DL
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|), NonEmpty(..))
import Data.Tuple (Tuple(..), snd)
import Data.Typelevel.Num (d3)
import Data.Vec as V
import Effect.Aff (Error)
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Behavior.Audio (AudioProcessor, AudioUnit'(..), AudioUnit''(..), SampleFrame, Status(..), audioGrouper, audioIO, audioIOInterleaved, audioToPtr, gain, gain', makeProgram, merger, sinOsc, speaker, speaker', splitter)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

frameZip :: SampleFrame -> SampleFrame -> SampleFrame
frameZip = zipWith (zipWith (+))

mulSampleFrame :: Number -> SampleFrame -> SampleFrame
mulSampleFrame n = map (map (_ * n))

simpleProcessor :: forall (r :: # Type). AudioProcessor r
simpleProcessor _ audio params = mulSampleFrame 0.25 <$> (audio 0.0)

delayProcessor :: forall (r :: # Type). AudioProcessor r
delayProcessor _ audio params = frameZip <$> (mulSampleFrame 0.25 <$> (audio 0.0)) <*> (mulSampleFrame 0.5 <$> (audio 1.0))

basicTestSuite :: ∀ eff m. Monad m ⇒ Bind eff ⇒ MonadEffect eff ⇒ MonadThrow Error eff ⇒ SpecT eff Unit m Unit
basicTestSuite =
  describe "basic suite" do
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
                    [ (Tuple 0 { au: Speaker', chan: 1, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 1), (Tuple 3 1), (Tuple 4 1) ]), ptr: 0, status: On })
                    , (Tuple 1 { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 1 0) ]), ptr: 1, status: On })
                    , (Tuple 2 { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 2 0) ]), ptr: 2, status: On })
                    , (Tuple 3 { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 3 0) ]), ptr: 3, status: On })
                    , (Tuple 4 { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 4 0) ]), ptr: 4, status: On })
                    ]
                )
            , len: 5
            , p:
                { au: Speaker'
                , chan: 1
                , name: Nothing
                , next: (fromFoldable [])
                , prev:
                    ( fromFoldable
                        [ (Tuple 0 0)
                        , (Tuple 1 1)
                        , (Tuple 2 1)
                        , (Tuple 3 1)
                        , (Tuple 4 1)
                        ]
                    )
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
                    [ (Tuple 0 { au: Speaker', chan: 1, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 2), (Tuple 7 3), (Tuple 8 3), (Tuple 9 3) ]), ptr: 0, status: On })
                    , (Tuple 1 { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1), (Tuple 3 2), (Tuple 4 2), (Tuple 5 2), (Tuple 6 1), (Tuple 7 2), (Tuple 8 2), (Tuple 9 2) ]), ptr: 1, status: On })
                    , (Tuple 2 { au: (Gain' 1.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1) ]), ptr: 2, status: On })
                    , (Tuple 3 { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 3 0) ]), ptr: 3, status: On })
                    , (Tuple 4 { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 4 0) ]), ptr: 4, status: On })
                    , (Tuple 5 { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 5 0) ]), ptr: 5, status: On })
                    , (Tuple 6 { au: (Gain' 0.9), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 6 0), (Tuple 7 1), (Tuple 8 1), (Tuple 9 1) ]), ptr: 6, status: On })
                    , (Tuple 7 { au: (SinOsc' 442.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 6 1) ]), prev: (fromFoldable [ (Tuple 7 0) ]), ptr: 7, status: On })
                    , (Tuple 8 { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 6 1) ]), prev: (fromFoldable [ (Tuple 8 0) ]), ptr: 8, status: On })
                    , (Tuple 9 { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 6 1) ]), prev: (fromFoldable [ (Tuple 9 0) ]), ptr: 9, status: On })
                    ]
                )
            , len: 10
            , p:
                { au: Speaker'
                , chan: 1
                , name: Nothing
                , next: (fromFoldable [])
                , prev:
                    ( fromFoldable
                        [ (Tuple 0 0)
                        , (Tuple 1 1)
                        , (Tuple 2 2)
                        , (Tuple 3 3)
                        , (Tuple 4 3)
                        , (Tuple 5 3)
                        , (Tuple 6 2)
                        , (Tuple 7 3)
                        , (Tuple 8 3)
                        , (Tuple 9 3)
                        ]
                    )
                , ptr: 0
                , status: On
                }
            }
      it "should act as value-based copy instead of duplicate" do
        let
          sp =
            audioToPtr
              $ splitter
                  (sinOsc 440.0)
                  ( \v0 ->
                      speaker
                        ((V.head v0) :| ((V.head v0) : (V.head v0) : Nil))
                  )
        sp
          `shouldEqual`
            { flat:
                ( fromFoldable
                    [ (Tuple 0 { au: Speaker', chan: 1, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2) ]), ptr: 0, status: On })
                    , (Tuple 1 { au: Splitter', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1) ]), ptr: 1, status: On })
                    , (Tuple 2 { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 2 0) ]), ptr: 2, status: On })
                    ]
                )
            , len: 3
            , p:
                { au: Splitter'
                , chan: 1
                , name: Nothing
                , next: (fromFoldable [ (Tuple 0 1) ])
                , prev:
                    ( fromFoldable
                        [ (Tuple 1 0)
                        , (Tuple 2 1)
                        ]
                    )
                , ptr: 1
                , status: On
                }
            }
      it "should ignore split given to split" do
        let
          sp =
            audioToPtr
              $ splitter
                  (sinOsc 440.0)
                  ( \v0 ->
                      splitter
                        (V.head v0)
                        ( \v1 ->
                            splitter
                              (V.head v1)
                              ( \v2 ->
                                  speaker
                                    ((V.head v0) :| ((V.head v1) : (V.head v2) : Nil))
                              )
                        )
                  )
        sp
          `shouldEqual`
            { flat:
                ( fromFoldable
                    [ (Tuple 0 { au: Speaker', chan: 1, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2) ]), ptr: 0, status: On })
                    , (Tuple 1 { au: Splitter', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1) ]), ptr: 1, status: On })
                    , (Tuple 2 { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 2 0) ]), ptr: 2, status: On })
                    ]
                )
            , len: 3
            , p:
                { au: Splitter'
                , chan: 1
                , name: Nothing
                , next: (fromFoldable [ (Tuple 0 1) ])
                , prev:
                    ( fromFoldable
                        [ (Tuple 1 0)
                        , (Tuple 2 1)
                        ]
                    )
                , ptr: 1
                , status: On
                }
            }
      it "should correctly split" do
        let
          tree =
            audioToPtr
              $ splitter
                  ( merger
                      $ V.replicate d3
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
                                  (map (gain' 0.3) v)
                              )
                        )
                  )
        tree
          `shouldEqual`
            { flat:
                ( fromFoldable
                    [ (Tuple 0 { au: Speaker', chan: 3, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 0, status: On })
                    , (Tuple 1 { au: (Gain' 0.5), chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1), (Tuple 3 2), (Tuple 4 2), (Tuple 5 2), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 1, status: On })
                    , (Tuple 2 { au: Merger', chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 2, status: On })
                    , (Tuple 3 { au: (Gain' 0.3), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 3 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 3, status: On })
                    , (Tuple 4 { au: (Gain' 0.3), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 4 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 4, status: On })
                    , (Tuple 5 { au: (Gain' 0.3), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 5 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 5, status: On })
                    , (Tuple 6 { au: Splitter', chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 3), (Tuple 2 2), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 6 0), (Tuple 7 1), (Tuple 8 2), (Tuple 9 3), (Tuple 10 4), (Tuple 11 4), (Tuple 12 4), (Tuple 13 3), (Tuple 14 4), (Tuple 15 4), (Tuple 16 4), (Tuple 17 2), (Tuple 18 3), (Tuple 19 4), (Tuple 20 4), (Tuple 21 4), (Tuple 22 3), (Tuple 23 4), (Tuple 24 4), (Tuple 25 4), (Tuple 26 2), (Tuple 27 3), (Tuple 28 4), (Tuple 29 4), (Tuple 30 4), (Tuple 31 3), (Tuple 32 4), (Tuple 33 4), (Tuple 34 4) ]), ptr: 6, status: On })
                    , (Tuple 7 { au: Merger', chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 5), (Tuple 1 4), (Tuple 2 3), (Tuple 3 2), (Tuple 4 2), (Tuple 5 2), (Tuple 6 1) ]), prev: (fromFoldable [ (Tuple 7 0), (Tuple 8 1), (Tuple 9 2), (Tuple 10 3), (Tuple 11 3), (Tuple 12 3), (Tuple 13 2), (Tuple 14 3), (Tuple 15 3), (Tuple 16 3), (Tuple 17 1), (Tuple 18 2), (Tuple 19 3), (Tuple 20 3), (Tuple 21 3), (Tuple 22 2), (Tuple 23 3), (Tuple 24 3), (Tuple 25 3), (Tuple 26 1), (Tuple 27 2), (Tuple 28 3), (Tuple 29 3), (Tuple 30 3), (Tuple 31 2), (Tuple 32 3), (Tuple 33 3), (Tuple 34 3) ]), ptr: 7, status: On })
                    , (Tuple 8 { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 5), (Tuple 2 4), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 2), (Tuple 7 1) ]), prev: (fromFoldable [ (Tuple 8 0), (Tuple 9 1), (Tuple 10 2), (Tuple 11 2), (Tuple 12 2), (Tuple 13 1), (Tuple 14 2), (Tuple 15 2), (Tuple 16 2) ]), ptr: 8, status: On })
                    , (Tuple 9 { au: (Gain' 1.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 7), (Tuple 1 6), (Tuple 2 5), (Tuple 3 4), (Tuple 4 4), (Tuple 5 4), (Tuple 6 3), (Tuple 7 2), (Tuple 8 1) ]), prev: (fromFoldable [ (Tuple 9 0), (Tuple 10 1), (Tuple 11 1), (Tuple 12 1) ]), ptr: 9, status: On })
                    , (Tuple 10 { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 10 0) ]), ptr: 10, status: On })
                    , (Tuple 11 { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 11 0) ]), ptr: 11, status: On })
                    , (Tuple 12 { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 12 0) ]), ptr: 12, status: On })
                    , (Tuple 13 { au: (Gain' 0.9), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 7), (Tuple 1 6), (Tuple 2 5), (Tuple 3 4), (Tuple 4 4), (Tuple 5 4), (Tuple 6 3), (Tuple 7 2), (Tuple 8 1) ]), prev: (fromFoldable [ (Tuple 13 0), (Tuple 14 1), (Tuple 15 1), (Tuple 16 1) ]), ptr: 13, status: On })
                    , (Tuple 14 { au: (SinOsc' 442.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), prev: (fromFoldable [ (Tuple 14 0) ]), ptr: 14, status: On })
                    , (Tuple 15 { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), prev: (fromFoldable [ (Tuple 15 0) ]), ptr: 15, status: On })
                    , (Tuple 16 { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), prev: (fromFoldable [ (Tuple 16 0) ]), ptr: 16, status: On })
                    , (Tuple 17 { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 5), (Tuple 2 4), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 2), (Tuple 7 1) ]), prev: (fromFoldable [ (Tuple 17 0), (Tuple 18 1), (Tuple 19 2), (Tuple 20 2), (Tuple 21 2), (Tuple 22 1), (Tuple 23 2), (Tuple 24 2), (Tuple 25 2) ]), ptr: 17, status: On })
                    , (Tuple 18 { au: (Gain' 1.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 7), (Tuple 1 6), (Tuple 2 5), (Tuple 3 4), (Tuple 4 4), (Tuple 5 4), (Tuple 6 3), (Tuple 7 2), (Tuple 17 1) ]), prev: (fromFoldable [ (Tuple 18 0), (Tuple 19 1), (Tuple 20 1), (Tuple 21 1) ]), ptr: 18, status: On })
                    , (Tuple 19 { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 17 2), (Tuple 18 1) ]), prev: (fromFoldable [ (Tuple 19 0) ]), ptr: 19, status: On })
                    , (Tuple 20 { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 17 2), (Tuple 18 1) ]), prev: (fromFoldable [ (Tuple 20 0) ]), ptr: 20, status: On })
                    , (Tuple 21 { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 17 2), (Tuple 18 1) ]), prev: (fromFoldable [ (Tuple 21 0) ]), ptr: 21, status: On })
                    , (Tuple 22 { au: (Gain' 0.9), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 7), (Tuple 1 6), (Tuple 2 5), (Tuple 3 4), (Tuple 4 4), (Tuple 5 4), (Tuple 6 3), (Tuple 7 2), (Tuple 17 1) ]), prev: (fromFoldable [ (Tuple 22 0), (Tuple 23 1), (Tuple 24 1), (Tuple 25 1) ]), ptr: 22, status: On })
                    , (Tuple 23 { au: (SinOsc' 442.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 17 2), (Tuple 22 1) ]), prev: (fromFoldable [ (Tuple 23 0) ]), ptr: 23, status: On })
                    , (Tuple 24 { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 17 2), (Tuple 22 1) ]), prev: (fromFoldable [ (Tuple 24 0) ]), ptr: 24, status: On })
                    , (Tuple 25 { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 17 2), (Tuple 22 1) ]), prev: (fromFoldable [ (Tuple 25 0) ]), ptr: 25, status: On })
                    , (Tuple 26 { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 5), (Tuple 2 4), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 2), (Tuple 7 1) ]), prev: (fromFoldable [ (Tuple 26 0), (Tuple 27 1), (Tuple 28 2), (Tuple 29 2), (Tuple 30 2), (Tuple 31 1), (Tuple 32 2), (Tuple 33 2), (Tuple 34 2) ]), ptr: 26, status: On })
                    , (Tuple 27 { au: (Gain' 1.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 7), (Tuple 1 6), (Tuple 2 5), (Tuple 3 4), (Tuple 4 4), (Tuple 5 4), (Tuple 6 3), (Tuple 7 2), (Tuple 26 1) ]), prev: (fromFoldable [ (Tuple 27 0), (Tuple 28 1), (Tuple 29 1), (Tuple 30 1) ]), ptr: 27, status: On })
                    , (Tuple 28 { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 26 2), (Tuple 27 1) ]), prev: (fromFoldable [ (Tuple 28 0) ]), ptr: 28, status: On })
                    , (Tuple 29 { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 26 2), (Tuple 27 1) ]), prev: (fromFoldable [ (Tuple 29 0) ]), ptr: 29, status: On })
                    , (Tuple 30 { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 26 2), (Tuple 27 1) ]), prev: (fromFoldable [ (Tuple 30 0) ]), ptr: 30, status: On })
                    , (Tuple 31 { au: (Gain' 0.9), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 7), (Tuple 1 6), (Tuple 2 5), (Tuple 3 4), (Tuple 4 4), (Tuple 5 4), (Tuple 6 3), (Tuple 7 2), (Tuple 26 1) ]), prev: (fromFoldable [ (Tuple 31 0), (Tuple 32 1), (Tuple 33 1), (Tuple 34 1) ]), ptr: 31, status: On })
                    , (Tuple 32 { au: (SinOsc' 442.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 26 2), (Tuple 31 1) ]), prev: (fromFoldable [ (Tuple 32 0) ]), ptr: 32, status: On })
                    , (Tuple 33 { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 26 2), (Tuple 31 1) ]), prev: (fromFoldable [ (Tuple 33 0) ]), ptr: 33, status: On })
                    , (Tuple 34 { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 4), (Tuple 7 3), (Tuple 26 2), (Tuple 31 1) ]), prev: (fromFoldable [ (Tuple 34 0) ]), ptr: 34, status: On })
                    ]
                )
            , len: 35
            , p:
                { au: Splitter'
                , chan: 3
                , name: Nothing
                , next: (fromFoldable [ (Tuple 0 4), (Tuple 1 3), (Tuple 2 2), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1) ])
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
                        , (Tuple 17 2)
                        , (Tuple 18 3)
                        , (Tuple 19 4)
                        , (Tuple 20 4)
                        , (Tuple 21 4)
                        , (Tuple 22 3)
                        , (Tuple 23 4)
                        , (Tuple 24 4)
                        , (Tuple 25 4)
                        , (Tuple 26 2)
                        , (Tuple 27 3)
                        , (Tuple 28 4)
                        , (Tuple 29 4)
                        , (Tuple 30 4)
                        , (Tuple 31 3)
                        , (Tuple 32 4)
                        , (Tuple 33 4)
                        , (Tuple 34 4)
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
            (audioGrouper <<< DL.fromFoldable <<< map snd)
              ( [ (Tuple 0 { au: Speaker', chan: 3, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 0, status: On })
                , (Tuple 1 { au: (Gain' 0.5), chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 0) ]), prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1), (Tuple 3 2), (Tuple 4 2), (Tuple 5 2), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 1, status: On })
                , (Tuple 2 { au: Merger', chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 1), (Tuple 1 0) ]), prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 2, status: On })
                , (Tuple 3 { au: (Gain' 0.3), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), prev: (fromFoldable [ (Tuple 3 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 3, status: On })
                , (Tuple 4 { au: (Gain' 0.3), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), prev: (fromFoldable [ (Tuple 4 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 4, status: On })
                , (Tuple 5 { au: (Gain' 0.3), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), prev: (fromFoldable [ (Tuple 5 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 5, status: On })
                , (Tuple 6 { au: Splitter', chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 3), (Tuple 2 2), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 6 0), (Tuple 7 1), (Tuple 8 2), (Tuple 9 3), (Tuple 10 4), (Tuple 11 4), (Tuple 12 4), (Tuple 13 3), (Tuple 14 4), (Tuple 15 4), (Tuple 16 4), (Tuple 17 2), (Tuple 18 3), (Tuple 19 4), (Tuple 20 4), (Tuple 21 4), (Tuple 22 3), (Tuple 23 4), (Tuple 24 4), (Tuple 25 4), (Tuple 26 2), (Tuple 27 3), (Tuple 28 4), (Tuple 29 4), (Tuple 30 4), (Tuple 31 3), (Tuple 32 4), (Tuple 33 4), (Tuple 34 4) ]), ptr: 6, status: On })
                , (Tuple 7 { au: Merger', chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 3), (Tuple 2 4), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 1) ]), prev: (fromFoldable [ (Tuple 7 0), (Tuple 8 1), (Tuple 9 2), (Tuple 10 3), (Tuple 11 3), (Tuple 12 3), (Tuple 13 2), (Tuple 14 3), (Tuple 15 3), (Tuple 16 3), (Tuple 17 1), (Tuple 18 2), (Tuple 19 3), (Tuple 20 3), (Tuple 21 3), (Tuple 22 2), (Tuple 23 3), (Tuple 24 3), (Tuple 25 3), (Tuple 26 1), (Tuple 27 2), (Tuple 28 3), (Tuple 29 3), (Tuple 30 3), (Tuple 31 2), (Tuple 32 3), (Tuple 33 3), (Tuple 34 3) ]), ptr: 7, status: On })
                , (Tuple 8 { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2), (Tuple 7 0) ]), prev: (fromFoldable [ (Tuple 8 0), (Tuple 9 1), (Tuple 10 2), (Tuple 11 2), (Tuple 12 2), (Tuple 13 1), (Tuple 14 2), (Tuple 15 2), (Tuple 16 2) ]), ptr: 8, status: On })
                , (Tuple 9 { au: (Gain' 1.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 8 0) ]), prev: (fromFoldable [ (Tuple 9 0), (Tuple 10 1), (Tuple 11 1), (Tuple 12 1) ]), ptr: 9, status: On })
                , (Tuple 10 { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 10 0) ]), ptr: 10, status: On })
                , (Tuple 11 { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 11 0) ]), ptr: 11, status: On })
                , (Tuple 12 { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 12 0) ]), ptr: 12, status: On })
                , (Tuple 13 { au: (Gain' 0.9), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 8 0) ]), prev: (fromFoldable [ (Tuple 13 0), (Tuple 14 1), (Tuple 15 1), (Tuple 16 1) ]), ptr: 13, status: On })
                , (Tuple 14 { au: (SinOsc' 442.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), prev: (fromFoldable [ (Tuple 14 0) ]), ptr: 14, status: On })
                , (Tuple 15 { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), prev: (fromFoldable [ (Tuple 15 0) ]), ptr: 15, status: On })
                , (Tuple 16 { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), prev: (fromFoldable [ (Tuple 16 0) ]), ptr: 16, status: On })
                , (Tuple 17 { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2), (Tuple 7 0) ]), prev: (fromFoldable [ (Tuple 17 0), (Tuple 18 1), (Tuple 19 2), (Tuple 20 2), (Tuple 21 2), (Tuple 22 1), (Tuple 23 2), (Tuple 24 2), (Tuple 25 2) ]), ptr: 17, status: On })
                , (Tuple 18 { au: (Gain' 1.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 17 0) ]), prev: (fromFoldable [ (Tuple 18 0), (Tuple 19 1), (Tuple 20 1), (Tuple 21 1) ]), ptr: 18, status: On })
                , (Tuple 19 { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 18 1) ]), prev: (fromFoldable [ (Tuple 19 0) ]), ptr: 19, status: On })
                , (Tuple 20 { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 18 1) ]), prev: (fromFoldable [ (Tuple 20 0) ]), ptr: 20, status: On })
                , (Tuple 21 { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 18 1) ]), prev: (fromFoldable [ (Tuple 21 0) ]), ptr: 21, status: On })
                , (Tuple 22 { au: (Gain' 0.9), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 17 0) ]), prev: (fromFoldable [ (Tuple 22 0), (Tuple 23 1), (Tuple 24 1), (Tuple 25 1) ]), ptr: 22, status: On })
                , (Tuple 23 { au: (SinOsc' 442.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 22 1) ]), prev: (fromFoldable [ (Tuple 23 0) ]), ptr: 23, status: On })
                , (Tuple 24 { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 22 1) ]), prev: (fromFoldable [ (Tuple 24 0) ]), ptr: 24, status: On })
                , (Tuple 25 { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 22 1) ]), prev: (fromFoldable [ (Tuple 25 0) ]), ptr: 25, status: On })
                , (Tuple 26 { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2), (Tuple 7 0) ]), prev: (fromFoldable [ (Tuple 26 0), (Tuple 27 1), (Tuple 28 2), (Tuple 29 2), (Tuple 30 2), (Tuple 31 1), (Tuple 32 2), (Tuple 33 2), (Tuple 34 2) ]), ptr: 26, status: On })
                , (Tuple 27 { au: (Gain' 1.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 26 0) ]), prev: (fromFoldable [ (Tuple 27 0), (Tuple 28 1), (Tuple 29 1), (Tuple 30 1) ]), ptr: 27, status: On })
                , (Tuple 28 { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 27 1) ]), prev: (fromFoldable [ (Tuple 28 0) ]), ptr: 28, status: On })
                , (Tuple 29 { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 27 1) ]), prev: (fromFoldable [ (Tuple 29 0) ]), ptr: 29, status: On })
                , (Tuple 30 { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 27 1) ]), prev: (fromFoldable [ (Tuple 30 0) ]), ptr: 30, status: On })
                , (Tuple 31 { au: (Gain' 0.9), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 26 0) ]), prev: (fromFoldable [ (Tuple 31 0), (Tuple 32 1), (Tuple 33 1), (Tuple 34 1) ]), ptr: 31, status: On })
                , (Tuple 32 { au: (SinOsc' 442.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 31 1) ]), prev: (fromFoldable [ (Tuple 32 0) ]), ptr: 32, status: On })
                , (Tuple 33 { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 31 1) ]), prev: (fromFoldable [ (Tuple 33 0) ]), ptr: 33, status: On })
                , (Tuple 34 { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 31 1) ]), prev: (fromFoldable [ (Tuple 34 0) ]), ptr: 34, status: On })
                ]
              )
        ag `shouldEqual` (fromFoldable [ (Tuple { chan: 1, name: Nothing, tag: Add'' } (NonEmpty { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2), (Tuple 7 0) ]), prev: (fromFoldable [ (Tuple 8 0), (Tuple 9 1), (Tuple 10 2), (Tuple 11 2), (Tuple 12 2), (Tuple 13 1), (Tuple 14 2), (Tuple 15 2), (Tuple 16 2) ]), ptr: 8, status: On } ({ au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2), (Tuple 7 0) ]), prev: (fromFoldable [ (Tuple 17 0), (Tuple 18 1), (Tuple 19 2), (Tuple 20 2), (Tuple 21 2), (Tuple 22 1), (Tuple 23 2), (Tuple 24 2), (Tuple 25 2) ]), ptr: 17, status: On } : { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2), (Tuple 7 0) ]), prev: (fromFoldable [ (Tuple 26 0), (Tuple 27 1), (Tuple 28 2), (Tuple 29 2), (Tuple 30 2), (Tuple 31 1), (Tuple 32 2), (Tuple 33 2), (Tuple 34 2) ]), ptr: 26, status: On } : Nil))), (Tuple { chan: 1, name: Nothing, tag: Gain'' } (NonEmpty { au: (Gain' 0.3), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), prev: (fromFoldable [ (Tuple 3 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 3, status: On } ({ au: (Gain' 0.3), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), prev: (fromFoldable [ (Tuple 4 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 4, status: On } : { au: (Gain' 0.3), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), prev: (fromFoldable [ (Tuple 5 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 5, status: On } : { au: (Gain' 1.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 8 0) ]), prev: (fromFoldable [ (Tuple 9 0), (Tuple 10 1), (Tuple 11 1), (Tuple 12 1) ]), ptr: 9, status: On } : { au: (Gain' 0.9), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 8 0) ]), prev: (fromFoldable [ (Tuple 13 0), (Tuple 14 1), (Tuple 15 1), (Tuple 16 1) ]), ptr: 13, status: On } : { au: (Gain' 1.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 17 0) ]), prev: (fromFoldable [ (Tuple 18 0), (Tuple 19 1), (Tuple 20 1), (Tuple 21 1) ]), ptr: 18, status: On } : { au: (Gain' 0.9), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 17 0) ]), prev: (fromFoldable [ (Tuple 22 0), (Tuple 23 1), (Tuple 24 1), (Tuple 25 1) ]), ptr: 22, status: On } : { au: (Gain' 1.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 26 0) ]), prev: (fromFoldable [ (Tuple 27 0), (Tuple 28 1), (Tuple 29 1), (Tuple 30 1) ]), ptr: 27, status: On } : { au: (Gain' 0.9), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 26 0) ]), prev: (fromFoldable [ (Tuple 31 0), (Tuple 32 1), (Tuple 33 1), (Tuple 34 1) ]), ptr: 31, status: On } : Nil))), (Tuple { chan: 1, name: Nothing, tag: SinOsc'' } (NonEmpty { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 10 0) ]), ptr: 10, status: On } ({ au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 11 0) ]), ptr: 11, status: On } : { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 12 0) ]), ptr: 12, status: On } : { au: (SinOsc' 442.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), prev: (fromFoldable [ (Tuple 14 0) ]), ptr: 14, status: On } : { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), prev: (fromFoldable [ (Tuple 15 0) ]), ptr: 15, status: On } : { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), prev: (fromFoldable [ (Tuple 16 0) ]), ptr: 16, status: On } : { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 18 1) ]), prev: (fromFoldable [ (Tuple 19 0) ]), ptr: 19, status: On } : { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 18 1) ]), prev: (fromFoldable [ (Tuple 20 0) ]), ptr: 20, status: On } : { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 18 1) ]), prev: (fromFoldable [ (Tuple 21 0) ]), ptr: 21, status: On } : { au: (SinOsc' 442.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 22 1) ]), prev: (fromFoldable [ (Tuple 23 0) ]), ptr: 23, status: On } : { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 22 1) ]), prev: (fromFoldable [ (Tuple 24 0) ]), ptr: 24, status: On } : { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 22 1) ]), prev: (fromFoldable [ (Tuple 25 0) ]), ptr: 25, status: On } : { au: (SinOsc' 440.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 27 1) ]), prev: (fromFoldable [ (Tuple 28 0) ]), ptr: 28, status: On } : { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 27 1) ]), prev: (fromFoldable [ (Tuple 29 0) ]), ptr: 29, status: On } : { au: (SinOsc' 441.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 27 1) ]), prev: (fromFoldable [ (Tuple 30 0) ]), ptr: 30, status: On } : { au: (SinOsc' 442.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 31 1) ]), prev: (fromFoldable [ (Tuple 32 0) ]), ptr: 32, status: On } : { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 31 1) ]), prev: (fromFoldable [ (Tuple 33 0) ]), ptr: 33, status: On } : { au: (SinOsc' 443.0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 31 1) ]), prev: (fromFoldable [ (Tuple 34 0) ]), ptr: 34, status: On } : Nil))), (Tuple { chan: 3, name: Nothing, tag: Gain'' } (NonEmpty { au: (Gain' 0.5), chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 0) ]), prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1), (Tuple 3 2), (Tuple 4 2), (Tuple 5 2), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 1, status: On } Nil)), (Tuple { chan: 3, name: Nothing, tag: Merger'' } (NonEmpty { au: Merger', chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 1), (Tuple 1 0) ]), prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 2, status: On } ({ au: Merger', chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 3), (Tuple 2 4), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 1) ]), prev: (fromFoldable [ (Tuple 7 0), (Tuple 8 1), (Tuple 9 2), (Tuple 10 3), (Tuple 11 3), (Tuple 12 3), (Tuple 13 2), (Tuple 14 3), (Tuple 15 3), (Tuple 16 3), (Tuple 17 1), (Tuple 18 2), (Tuple 19 3), (Tuple 20 3), (Tuple 21 3), (Tuple 22 2), (Tuple 23 3), (Tuple 24 3), (Tuple 25 3), (Tuple 26 1), (Tuple 27 2), (Tuple 28 3), (Tuple 29 3), (Tuple 30 3), (Tuple 31 2), (Tuple 32 3), (Tuple 33 3), (Tuple 34 3) ]), ptr: 7, status: On } : Nil))), (Tuple { chan: 3, name: Nothing, tag: Speaker'' } (NonEmpty { au: Speaker', chan: 3, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 0, status: On } Nil)), (Tuple { chan: 3, name: Nothing, tag: Splitter'' } (NonEmpty { au: Splitter', chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 3), (Tuple 2 2), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 6 0), (Tuple 7 1), (Tuple 8 2), (Tuple 9 3), (Tuple 10 4), (Tuple 11 4), (Tuple 12 4), (Tuple 13 3), (Tuple 14 4), (Tuple 15 4), (Tuple 16 4), (Tuple 17 2), (Tuple 18 3), (Tuple 19 4), (Tuple 20 4), (Tuple 21 4), (Tuple 22 3), (Tuple 23 4), (Tuple 24 4), (Tuple 25 4), (Tuple 26 2), (Tuple 27 3), (Tuple 28 4), (Tuple 29 4), (Tuple 30 4), (Tuple 31 3), (Tuple 32 4), (Tuple 33 4), (Tuple 34 4) ]), ptr: 6, status: On } Nil)) ])
    describe "Linear program generation" do
      it "Should generate a correct linear program for two audio graphs" do
        let
          tree0 =
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
        let
          tree1 =
            audioToPtr
              $ speaker'
                  ( ( gain 1.0
                        ( (sinOsc 440.0)
                            :| (DL.fromFoldable (replicate 1 (sinOsc 441.0)))
                        )
                    )
                      + ( gain 0.7
                            ( (sinOsc 442.0)
                                :| (DL.fromFoldable (replicate 1 (sinOsc 443.0)))
                            )
                        )
                  )
        let
          prog =
            makeProgram
              { flat: tree0.flat, grouped: audioGrouper (DL.fromFoldable tree0.flat)
              }
              { flat: tree1.flat, grouped: audioGrouper (DL.fromFoldable tree1.flat) }
        prog.prog
          `shouldEqual`
            { binaries: [ "n@Add''_1_1", "n@Gain''_2_2", "n@Gain''_2_5", "n@Gain''_6_2", "n@Gain''_6_5", "n@SinOsc''_3_3", "n@SinOsc''_3_4", "n@SinOsc''_3_6", "n@SinOsc''_3_7", "n@SinOsc''_3_8", "n@SinOsc''_3_9", "n@SinOsc''_4_3", "n@SinOsc''_4_4", "n@SinOsc''_4_6", "n@SinOsc''_4_7", "n@SinOsc''_4_8", "n@SinOsc''_4_9", "n@SinOsc''_5_3", "n@SinOsc''_5_4", "n@SinOsc''_5_6", "n@SinOsc''_5_7", "n@SinOsc''_5_8", "n@SinOsc''_5_9", "n@SinOsc''_7_3", "n@SinOsc''_7_4", "n@SinOsc''_7_6", "n@SinOsc''_7_7", "n@SinOsc''_7_8", "n@SinOsc''_7_9", "n@SinOsc''_8_3", "n@SinOsc''_8_4", "n@SinOsc''_8_6", "n@SinOsc''_8_7", "n@SinOsc''_8_8", "n@SinOsc''_8_9", "n@SinOsc''_9_3", "n@SinOsc''_9_4", "n@SinOsc''_9_6", "n@SinOsc''_9_7", "n@SinOsc''_9_8", "n@SinOsc''_9_9", "n@Speaker''_0_0", "e@Add''_1_1__Speaker''_0_0", "e@Gain''_2_2__Add''_1_1", "e@Gain''_2_5__Add''_1_1", "e@Gain''_6_2__Add''_1_1", "e@Gain''_6_5__Add''_1_1", "e@SinOsc''_3_3__Gain''_2_2", "e@SinOsc''_3_3__Gain''_2_5", "e@SinOsc''_3_4__Gain''_2_2", "e@SinOsc''_3_4__Gain''_2_5", "e@SinOsc''_3_6__Gain''_2_2", "e@SinOsc''_3_6__Gain''_2_5", "e@SinOsc''_3_7__Gain''_2_2", "e@SinOsc''_3_7__Gain''_2_5", "e@SinOsc''_3_8__Gain''_2_2", "e@SinOsc''_3_8__Gain''_2_5", "e@SinOsc''_3_9__Gain''_2_2", "e@SinOsc''_3_9__Gain''_2_5", "e@SinOsc''_4_3__Gain''_2_2", "e@SinOsc''_4_3__Gain''_2_5", "e@SinOsc''_4_4__Gain''_2_2", "e@SinOsc''_4_4__Gain''_2_5", "e@SinOsc''_4_6__Gain''_2_2", "e@SinOsc''_4_6__Gain''_2_5", "e@SinOsc''_4_7__Gain''_2_2", "e@SinOsc''_4_7__Gain''_2_5", "e@SinOsc''_4_8__Gain''_2_2", "e@SinOsc''_4_8__Gain''_2_5", "e@SinOsc''_4_9__Gain''_2_2", "e@SinOsc''_4_9__Gain''_2_5", "e@SinOsc''_5_3__Gain''_2_2", "e@SinOsc''_5_3__Gain''_2_5", "e@SinOsc''_5_4__Gain''_2_2", "e@SinOsc''_5_4__Gain''_2_5", "e@SinOsc''_5_6__Gain''_2_2", "e@SinOsc''_5_6__Gain''_2_5", "e@SinOsc''_5_7__Gain''_2_2", "e@SinOsc''_5_7__Gain''_2_5", "e@SinOsc''_5_8__Gain''_2_2", "e@SinOsc''_5_8__Gain''_2_5", "e@SinOsc''_5_9__Gain''_2_2", "e@SinOsc''_5_9__Gain''_2_5", "e@SinOsc''_7_3__Gain''_6_2", "e@SinOsc''_7_3__Gain''_6_5", "e@SinOsc''_7_4__Gain''_6_2", "e@SinOsc''_7_4__Gain''_6_5", "e@SinOsc''_7_6__Gain''_6_2", "e@SinOsc''_7_6__Gain''_6_5", "e@SinOsc''_7_7__Gain''_6_2", "e@SinOsc''_7_7__Gain''_6_5", "e@SinOsc''_7_8__Gain''_6_2", "e@SinOsc''_7_8__Gain''_6_5", "e@SinOsc''_7_9__Gain''_6_2", "e@SinOsc''_7_9__Gain''_6_5", "e@SinOsc''_8_3__Gain''_6_2", "e@SinOsc''_8_3__Gain''_6_5", "e@SinOsc''_8_4__Gain''_6_2", "e@SinOsc''_8_4__Gain''_6_5", "e@SinOsc''_8_6__Gain''_6_2", "e@SinOsc''_8_6__Gain''_6_5", "e@SinOsc''_8_7__Gain''_6_2", "e@SinOsc''_8_7__Gain''_6_5", "e@SinOsc''_8_8__Gain''_6_2", "e@SinOsc''_8_8__Gain''_6_5", "e@SinOsc''_8_9__Gain''_6_2", "e@SinOsc''_8_9__Gain''_6_5", "e@SinOsc''_9_3__Gain''_6_2", "e@SinOsc''_9_3__Gain''_6_5", "e@SinOsc''_9_4__Gain''_6_2", "e@SinOsc''_9_4__Gain''_6_5", "e@SinOsc''_9_6__Gain''_6_2", "e@SinOsc''_9_6__Gain''_6_5", "e@SinOsc''_9_7__Gain''_6_2", "e@SinOsc''_9_7__Gain''_6_5", "e@SinOsc''_9_8__Gain''_6_2", "e@SinOsc''_9_8__Gain''_6_5", "e@SinOsc''_9_9__Gain''_6_2", "e@SinOsc''_9_9__Gain''_6_5" ], name: "LP", objective: { direction: 1, name: "obj", vars: [ { coef: 0.30000000000000004, name: "n@Gain''_2_5" }, { coef: 0.09999999999999998, name: "n@Gain''_6_2" }, { coef: 0.20000000000000007, name: "n@Gain''_6_5" }, { coef: 0.00004524886877828054, name: "n@SinOsc''_3_4" }, { coef: 0.00009049773755656108, name: "n@SinOsc''_3_6" }, { coef: 0.0001357466063348416, name: "n@SinOsc''_3_7" }, { coef: 2.2425339366515837, name: "n@SinOsc''_3_8" }, { coef: 2.2425339366515837, name: "n@SinOsc''_3_9" }, { coef: 0.00004524886877828054, name: "n@SinOsc''_4_3" }, { coef: 0.00004524886877828054, name: "n@SinOsc''_4_6" }, { coef: 0.00009049773755656108, name: "n@SinOsc''_4_7" }, { coef: 2.242488687782805, name: "n@SinOsc''_4_8" }, { coef: 2.242488687782805, name: "n@SinOsc''_4_9" }, { coef: 0.00004524886877828054, name: "n@SinOsc''_5_3" }, { coef: 0.00004524886877828054, name: "n@SinOsc''_5_6" }, { coef: 0.00009049773755656108, name: "n@SinOsc''_5_7" }, { coef: 2.242488687782805, name: "n@SinOsc''_5_8" }, { coef: 2.242488687782805, name: "n@SinOsc''_5_9" }, { coef: 0.00009049773755656108, name: "n@SinOsc''_7_3" }, { coef: 0.00004524886877828054, name: "n@SinOsc''_7_4" }, { coef: 0.00004524886877828054, name: "n@SinOsc''_7_7" }, { coef: 2.242443438914027, name: "n@SinOsc''_7_8" }, { coef: 2.242443438914027, name: "n@SinOsc''_7_9" }, { coef: 0.0001357466063348416, name: "n@SinOsc''_8_3" }, { coef: 0.00009049773755656108, name: "n@SinOsc''_8_4" }, { coef: 0.00004524886877828054, name: "n@SinOsc''_8_6" }, { coef: 2.242398190045249, name: "n@SinOsc''_8_8" }, { coef: 2.242398190045249, name: "n@SinOsc''_8_9" }, { coef: 0.0001357466063348416, name: "n@SinOsc''_9_3" }, { coef: 0.00009049773755656108, name: "n@SinOsc''_9_4" }, { coef: 0.00004524886877828054, name: "n@SinOsc''_9_6" }, { coef: 2.242398190045249, name: "n@SinOsc''_9_8" }, { coef: 2.242398190045249, name: "n@SinOsc''_9_9" }, { coef: 1000.0, name: "e@SinOsc''_3_3__Gain''_2_5" }, { coef: 1000.0, name: "e@SinOsc''_3_4__Gain''_2_5" }, { coef: 1000.0, name: "e@SinOsc''_3_6__Gain''_2_2" }, { coef: 1000.0, name: "e@SinOsc''_3_7__Gain''_2_2" }, { coef: 1000.0, name: "e@SinOsc''_3_8__Gain''_2_2" }, { coef: 1000.0, name: "e@SinOsc''_3_8__Gain''_2_5" }, { coef: 1000.0, name: "e@SinOsc''_3_9__Gain''_2_2" }, { coef: 1000.0, name: "e@SinOsc''_3_9__Gain''_2_5" }, { coef: 1000.0, name: "e@SinOsc''_4_3__Gain''_2_5" }, { coef: 1000.0, name: "e@SinOsc''_4_4__Gain''_2_5" }, { coef: 1000.0, name: "e@SinOsc''_4_6__Gain''_2_2" }, { coef: 1000.0, name: "e@SinOsc''_4_7__Gain''_2_2" }, { coef: 1000.0, name: "e@SinOsc''_4_8__Gain''_2_2" }, { coef: 1000.0, name: "e@SinOsc''_4_8__Gain''_2_5" }, { coef: 1000.0, name: "e@SinOsc''_4_9__Gain''_2_2" }, { coef: 1000.0, name: "e@SinOsc''_4_9__Gain''_2_5" }, { coef: 1000.0, name: "e@SinOsc''_5_3__Gain''_2_5" }, { coef: 1000.0, name: "e@SinOsc''_5_4__Gain''_2_5" }, { coef: 1000.0, name: "e@SinOsc''_5_6__Gain''_2_2" }, { coef: 1000.0, name: "e@SinOsc''_5_7__Gain''_2_2" }, { coef: 1000.0, name: "e@SinOsc''_5_8__Gain''_2_2" }, { coef: 1000.0, name: "e@SinOsc''_5_8__Gain''_2_5" }, { coef: 1000.0, name: "e@SinOsc''_5_9__Gain''_2_2" }, { coef: 1000.0, name: "e@SinOsc''_5_9__Gain''_2_5" }, { coef: 1000.0, name: "e@SinOsc''_7_3__Gain''_6_5" }, { coef: 1000.0, name: "e@SinOsc''_7_4__Gain''_6_5" }, { coef: 1000.0, name: "e@SinOsc''_7_6__Gain''_6_2" }, { coef: 1000.0, name: "e@SinOsc''_7_7__Gain''_6_2" }, { coef: 1000.0, name: "e@SinOsc''_7_8__Gain''_6_2" }, { coef: 1000.0, name: "e@SinOsc''_7_8__Gain''_6_5" }, { coef: 1000.0, name: "e@SinOsc''_7_9__Gain''_6_2" }, { coef: 1000.0, name: "e@SinOsc''_7_9__Gain''_6_5" }, { coef: 1000.0, name: "e@SinOsc''_8_3__Gain''_6_5" }, { coef: 1000.0, name: "e@SinOsc''_8_4__Gain''_6_5" }, { coef: 1000.0, name: "e@SinOsc''_8_6__Gain''_6_2" }, { coef: 1000.0, name: "e@SinOsc''_8_7__Gain''_6_2" }, { coef: 1000.0, name: "e@SinOsc''_8_8__Gain''_6_2" }, { coef: 1000.0, name: "e@SinOsc''_8_8__Gain''_6_5" }, { coef: 1000.0, name: "e@SinOsc''_8_9__Gain''_6_2" }, { coef: 1000.0, name: "e@SinOsc''_8_9__Gain''_6_5" }, { coef: 1000.0, name: "e@SinOsc''_9_3__Gain''_6_5" }, { coef: 1000.0, name: "e@SinOsc''_9_4__Gain''_6_5" }, { coef: 1000.0, name: "e@SinOsc''_9_6__Gain''_6_2" }, { coef: 1000.0, name: "e@SinOsc''_9_7__Gain''_6_2" }, { coef: 1000.0, name: "e@SinOsc''_9_8__Gain''_6_2" }, { coef: 1000.0, name: "e@SinOsc''_9_8__Gain''_6_5" }, { coef: 1000.0, name: "e@SinOsc''_9_9__Gain''_6_2" }, { coef: 1000.0, name: "e@SinOsc''_9_9__Gain''_6_5" } ] }, subjectTo: [ { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_0", vars: [ { coef: 1.0, name: "n@Speaker''_0_0" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_1", vars: [ { coef: 1.0, name: "n@Add''_1_1" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_2", vars: [ { coef: 1.0, name: "n@Gain''_2_2" }, { coef: 1.0, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_3", vars: [ { coef: 1.0, name: "n@SinOsc''_3_3" }, { coef: 1.0, name: "n@SinOsc''_3_4" }, { coef: 1.0, name: "n@SinOsc''_3_6" }, { coef: 1.0, name: "n@SinOsc''_3_7" }, { coef: 1.0, name: "n@SinOsc''_3_8" }, { coef: 1.0, name: "n@SinOsc''_3_9" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_4", vars: [ { coef: 1.0, name: "n@SinOsc''_4_3" }, { coef: 1.0, name: "n@SinOsc''_4_4" }, { coef: 1.0, name: "n@SinOsc''_4_6" }, { coef: 1.0, name: "n@SinOsc''_4_7" }, { coef: 1.0, name: "n@SinOsc''_4_8" }, { coef: 1.0, name: "n@SinOsc''_4_9" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_5", vars: [ { coef: 1.0, name: "n@SinOsc''_5_3" }, { coef: 1.0, name: "n@SinOsc''_5_4" }, { coef: 1.0, name: "n@SinOsc''_5_6" }, { coef: 1.0, name: "n@SinOsc''_5_7" }, { coef: 1.0, name: "n@SinOsc''_5_8" }, { coef: 1.0, name: "n@SinOsc''_5_9" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_6", vars: [ { coef: 1.0, name: "n@Gain''_6_2" }, { coef: 1.0, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_7", vars: [ { coef: 1.0, name: "n@SinOsc''_7_3" }, { coef: 1.0, name: "n@SinOsc''_7_4" }, { coef: 1.0, name: "n@SinOsc''_7_6" }, { coef: 1.0, name: "n@SinOsc''_7_7" }, { coef: 1.0, name: "n@SinOsc''_7_8" }, { coef: 1.0, name: "n@SinOsc''_7_9" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_8", vars: [ { coef: 1.0, name: "n@SinOsc''_8_3" }, { coef: 1.0, name: "n@SinOsc''_8_4" }, { coef: 1.0, name: "n@SinOsc''_8_6" }, { coef: 1.0, name: "n@SinOsc''_8_7" }, { coef: 1.0, name: "n@SinOsc''_8_8" }, { coef: 1.0, name: "n@SinOsc''_8_9" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_9", vars: [ { coef: 1.0, name: "n@SinOsc''_9_3" }, { coef: 1.0, name: "n@SinOsc''_9_4" }, { coef: 1.0, name: "n@SinOsc''_9_6" }, { coef: 1.0, name: "n@SinOsc''_9_7" }, { coef: 1.0, name: "n@SinOsc''_9_8" }, { coef: 1.0, name: "n@SinOsc''_9_9" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_0", vars: [ { coef: 1.0, name: "n@Speaker''_0_0" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_1", vars: [ { coef: 1.0, name: "n@Add''_1_1" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_2", vars: [ { coef: 1.0, name: "n@Gain''_2_2" }, { coef: 1.0, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_3", vars: [ { coef: 1.0, name: "n@SinOsc''_3_3" }, { coef: 1.0, name: "n@SinOsc''_4_3" }, { coef: 1.0, name: "n@SinOsc''_5_3" }, { coef: 1.0, name: "n@SinOsc''_7_3" }, { coef: 1.0, name: "n@SinOsc''_8_3" }, { coef: 1.0, name: "n@SinOsc''_9_3" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_4", vars: [ { coef: 1.0, name: "n@SinOsc''_3_4" }, { coef: 1.0, name: "n@SinOsc''_4_4" }, { coef: 1.0, name: "n@SinOsc''_5_4" }, { coef: 1.0, name: "n@SinOsc''_7_4" }, { coef: 1.0, name: "n@SinOsc''_8_4" }, { coef: 1.0, name: "n@SinOsc''_9_4" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_5", vars: [ { coef: 1.0, name: "n@Gain''_2_5" }, { coef: 1.0, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_6", vars: [ { coef: 1.0, name: "n@SinOsc''_3_6" }, { coef: 1.0, name: "n@SinOsc''_4_6" }, { coef: 1.0, name: "n@SinOsc''_5_6" }, { coef: 1.0, name: "n@SinOsc''_7_6" }, { coef: 1.0, name: "n@SinOsc''_8_6" }, { coef: 1.0, name: "n@SinOsc''_9_6" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_7", vars: [ { coef: 1.0, name: "n@SinOsc''_3_7" }, { coef: 1.0, name: "n@SinOsc''_4_7" }, { coef: 1.0, name: "n@SinOsc''_5_7" }, { coef: 1.0, name: "n@SinOsc''_7_7" }, { coef: 1.0, name: "n@SinOsc''_8_7" }, { coef: 1.0, name: "n@SinOsc''_9_7" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_8", vars: [ { coef: 1.0, name: "n@SinOsc''_3_8" }, { coef: 1.0, name: "n@SinOsc''_4_8" }, { coef: 1.0, name: "n@SinOsc''_5_8" }, { coef: 1.0, name: "n@SinOsc''_7_8" }, { coef: 1.0, name: "n@SinOsc''_8_8" }, { coef: 1.0, name: "n@SinOsc''_9_8" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "nodec_9", vars: [ { coef: 1.0, name: "n@SinOsc''_3_9" }, { coef: 1.0, name: "n@SinOsc''_4_9" }, { coef: 1.0, name: "n@SinOsc''_5_9" }, { coef: 1.0, name: "n@SinOsc''_7_9" }, { coef: 1.0, name: "n@SinOsc''_8_9" }, { coef: 1.0, name: "n@SinOsc''_9_9" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "edgec_0", vars: [ { coef: 1.0, name: "e@Add''_1_1__Speaker''_0_0" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "edgec_1", vars: [ { coef: 1.0, name: "e@Gain''_2_2__Add''_1_1" }, { coef: 1.0, name: "e@Gain''_2_5__Add''_1_1" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "edgec_2", vars: [ { coef: 1.0, name: "e@SinOsc''_3_3__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_3_3__Gain''_2_5" }, { coef: 1.0, name: "e@SinOsc''_3_4__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_3_4__Gain''_2_5" }, { coef: 1.0, name: "e@SinOsc''_3_6__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_3_6__Gain''_2_5" }, { coef: 1.0, name: "e@SinOsc''_3_7__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_3_7__Gain''_2_5" }, { coef: 1.0, name: "e@SinOsc''_3_8__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_3_8__Gain''_2_5" }, { coef: 1.0, name: "e@SinOsc''_3_9__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_3_9__Gain''_2_5" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "edgec_3", vars: [ { coef: 1.0, name: "e@SinOsc''_4_3__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_4_3__Gain''_2_5" }, { coef: 1.0, name: "e@SinOsc''_4_4__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_4_4__Gain''_2_5" }, { coef: 1.0, name: "e@SinOsc''_4_6__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_4_6__Gain''_2_5" }, { coef: 1.0, name: "e@SinOsc''_4_7__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_4_7__Gain''_2_5" }, { coef: 1.0, name: "e@SinOsc''_4_8__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_4_8__Gain''_2_5" }, { coef: 1.0, name: "e@SinOsc''_4_9__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_4_9__Gain''_2_5" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "edgec_4", vars: [ { coef: 1.0, name: "e@SinOsc''_5_3__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_5_3__Gain''_2_5" }, { coef: 1.0, name: "e@SinOsc''_5_4__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_5_4__Gain''_2_5" }, { coef: 1.0, name: "e@SinOsc''_5_6__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_5_6__Gain''_2_5" }, { coef: 1.0, name: "e@SinOsc''_5_7__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_5_7__Gain''_2_5" }, { coef: 1.0, name: "e@SinOsc''_5_8__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_5_8__Gain''_2_5" }, { coef: 1.0, name: "e@SinOsc''_5_9__Gain''_2_2" }, { coef: 1.0, name: "e@SinOsc''_5_9__Gain''_2_5" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "edgec_5", vars: [ { coef: 1.0, name: "e@Gain''_6_2__Add''_1_1" }, { coef: 1.0, name: "e@Gain''_6_5__Add''_1_1" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "edgec_6", vars: [ { coef: 1.0, name: "e@SinOsc''_7_3__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_7_3__Gain''_6_5" }, { coef: 1.0, name: "e@SinOsc''_7_4__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_7_4__Gain''_6_5" }, { coef: 1.0, name: "e@SinOsc''_7_6__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_7_6__Gain''_6_5" }, { coef: 1.0, name: "e@SinOsc''_7_7__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_7_7__Gain''_6_5" }, { coef: 1.0, name: "e@SinOsc''_7_8__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_7_8__Gain''_6_5" }, { coef: 1.0, name: "e@SinOsc''_7_9__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_7_9__Gain''_6_5" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "edgec_7", vars: [ { coef: 1.0, name: "e@SinOsc''_8_3__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_8_3__Gain''_6_5" }, { coef: 1.0, name: "e@SinOsc''_8_4__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_8_4__Gain''_6_5" }, { coef: 1.0, name: "e@SinOsc''_8_6__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_8_6__Gain''_6_5" }, { coef: 1.0, name: "e@SinOsc''_8_7__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_8_7__Gain''_6_5" }, { coef: 1.0, name: "e@SinOsc''_8_8__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_8_8__Gain''_6_5" }, { coef: 1.0, name: "e@SinOsc''_8_9__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_8_9__Gain''_6_5" } ] }, { bnds: { lb: 1.0, type: 5, ub: 1.0 }, name: "edgec_8", vars: [ { coef: 1.0, name: "e@SinOsc''_9_3__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_9_3__Gain''_6_5" }, { coef: 1.0, name: "e@SinOsc''_9_4__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_9_4__Gain''_6_5" }, { coef: 1.0, name: "e@SinOsc''_9_6__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_9_6__Gain''_6_5" }, { coef: 1.0, name: "e@SinOsc''_9_7__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_9_7__Gain''_6_5" }, { coef: 1.0, name: "e@SinOsc''_9_8__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_9_8__Gain''_6_5" }, { coef: 1.0, name: "e@SinOsc''_9_9__Gain''_6_2" }, { coef: 1.0, name: "e@SinOsc''_9_9__Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_0", vars: [ { coef: 1.0, name: "e@Add''_1_1__Speaker''_0_0" }, { coef: -0.75, name: "n@Add''_1_1" }, { coef: -0.75, name: "n@Speaker''_0_0" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_1", vars: [ { coef: 1.0, name: "e@Gain''_2_2__Add''_1_1" }, { coef: -0.75, name: "n@Gain''_2_2" }, { coef: -0.75, name: "n@Add''_1_1" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_2", vars: [ { coef: 1.0, name: "e@Gain''_2_5__Add''_1_1" }, { coef: -0.75, name: "n@Gain''_2_5" }, { coef: -0.75, name: "n@Add''_1_1" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_3", vars: [ { coef: 1.0, name: "e@Gain''_6_2__Add''_1_1" }, { coef: -0.75, name: "n@Gain''_6_2" }, { coef: -0.75, name: "n@Add''_1_1" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_4", vars: [ { coef: 1.0, name: "e@Gain''_6_5__Add''_1_1" }, { coef: -0.75, name: "n@Gain''_6_5" }, { coef: -0.75, name: "n@Add''_1_1" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_5", vars: [ { coef: 1.0, name: "e@SinOsc''_3_3__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_3_3" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_6", vars: [ { coef: 1.0, name: "e@SinOsc''_3_3__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_3_3" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_7", vars: [ { coef: 1.0, name: "e@SinOsc''_3_4__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_3_4" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_8", vars: [ { coef: 1.0, name: "e@SinOsc''_3_4__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_3_4" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_9", vars: [ { coef: 1.0, name: "e@SinOsc''_3_6__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_3_6" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_10", vars: [ { coef: 1.0, name: "e@SinOsc''_3_6__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_3_6" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_11", vars: [ { coef: 1.0, name: "e@SinOsc''_3_7__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_3_7" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_12", vars: [ { coef: 1.0, name: "e@SinOsc''_3_7__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_3_7" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_13", vars: [ { coef: 1.0, name: "e@SinOsc''_3_8__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_3_8" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_14", vars: [ { coef: 1.0, name: "e@SinOsc''_3_8__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_3_8" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_15", vars: [ { coef: 1.0, name: "e@SinOsc''_3_9__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_3_9" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_16", vars: [ { coef: 1.0, name: "e@SinOsc''_3_9__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_3_9" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_17", vars: [ { coef: 1.0, name: "e@SinOsc''_4_3__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_4_3" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_18", vars: [ { coef: 1.0, name: "e@SinOsc''_4_3__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_4_3" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_19", vars: [ { coef: 1.0, name: "e@SinOsc''_4_4__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_4_4" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_20", vars: [ { coef: 1.0, name: "e@SinOsc''_4_4__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_4_4" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_21", vars: [ { coef: 1.0, name: "e@SinOsc''_4_6__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_4_6" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_22", vars: [ { coef: 1.0, name: "e@SinOsc''_4_6__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_4_6" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_23", vars: [ { coef: 1.0, name: "e@SinOsc''_4_7__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_4_7" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_24", vars: [ { coef: 1.0, name: "e@SinOsc''_4_7__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_4_7" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_25", vars: [ { coef: 1.0, name: "e@SinOsc''_4_8__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_4_8" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_26", vars: [ { coef: 1.0, name: "e@SinOsc''_4_8__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_4_8" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_27", vars: [ { coef: 1.0, name: "e@SinOsc''_4_9__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_4_9" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_28", vars: [ { coef: 1.0, name: "e@SinOsc''_4_9__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_4_9" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_29", vars: [ { coef: 1.0, name: "e@SinOsc''_5_3__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_5_3" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_30", vars: [ { coef: 1.0, name: "e@SinOsc''_5_3__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_5_3" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_31", vars: [ { coef: 1.0, name: "e@SinOsc''_5_4__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_5_4" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_32", vars: [ { coef: 1.0, name: "e@SinOsc''_5_4__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_5_4" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_33", vars: [ { coef: 1.0, name: "e@SinOsc''_5_6__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_5_6" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_34", vars: [ { coef: 1.0, name: "e@SinOsc''_5_6__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_5_6" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_35", vars: [ { coef: 1.0, name: "e@SinOsc''_5_7__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_5_7" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_36", vars: [ { coef: 1.0, name: "e@SinOsc''_5_7__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_5_7" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_37", vars: [ { coef: 1.0, name: "e@SinOsc''_5_8__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_5_8" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_38", vars: [ { coef: 1.0, name: "e@SinOsc''_5_8__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_5_8" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_39", vars: [ { coef: 1.0, name: "e@SinOsc''_5_9__Gain''_2_2" }, { coef: -0.75, name: "n@SinOsc''_5_9" }, { coef: -0.75, name: "n@Gain''_2_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_40", vars: [ { coef: 1.0, name: "e@SinOsc''_5_9__Gain''_2_5" }, { coef: -0.75, name: "n@SinOsc''_5_9" }, { coef: -0.75, name: "n@Gain''_2_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_41", vars: [ { coef: 1.0, name: "e@SinOsc''_7_3__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_7_3" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_42", vars: [ { coef: 1.0, name: "e@SinOsc''_7_3__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_7_3" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_43", vars: [ { coef: 1.0, name: "e@SinOsc''_7_4__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_7_4" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_44", vars: [ { coef: 1.0, name: "e@SinOsc''_7_4__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_7_4" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_45", vars: [ { coef: 1.0, name: "e@SinOsc''_7_6__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_7_6" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_46", vars: [ { coef: 1.0, name: "e@SinOsc''_7_6__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_7_6" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_47", vars: [ { coef: 1.0, name: "e@SinOsc''_7_7__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_7_7" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_48", vars: [ { coef: 1.0, name: "e@SinOsc''_7_7__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_7_7" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_49", vars: [ { coef: 1.0, name: "e@SinOsc''_7_8__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_7_8" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_50", vars: [ { coef: 1.0, name: "e@SinOsc''_7_8__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_7_8" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_51", vars: [ { coef: 1.0, name: "e@SinOsc''_7_9__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_7_9" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_52", vars: [ { coef: 1.0, name: "e@SinOsc''_7_9__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_7_9" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_53", vars: [ { coef: 1.0, name: "e@SinOsc''_8_3__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_8_3" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_54", vars: [ { coef: 1.0, name: "e@SinOsc''_8_3__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_8_3" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_55", vars: [ { coef: 1.0, name: "e@SinOsc''_8_4__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_8_4" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_56", vars: [ { coef: 1.0, name: "e@SinOsc''_8_4__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_8_4" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_57", vars: [ { coef: 1.0, name: "e@SinOsc''_8_6__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_8_6" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_58", vars: [ { coef: 1.0, name: "e@SinOsc''_8_6__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_8_6" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_59", vars: [ { coef: 1.0, name: "e@SinOsc''_8_7__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_8_7" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_60", vars: [ { coef: 1.0, name: "e@SinOsc''_8_7__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_8_7" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_61", vars: [ { coef: 1.0, name: "e@SinOsc''_8_8__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_8_8" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_62", vars: [ { coef: 1.0, name: "e@SinOsc''_8_8__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_8_8" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_63", vars: [ { coef: 1.0, name: "e@SinOsc''_8_9__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_8_9" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_64", vars: [ { coef: 1.0, name: "e@SinOsc''_8_9__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_8_9" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_65", vars: [ { coef: 1.0, name: "e@SinOsc''_9_3__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_9_3" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_66", vars: [ { coef: 1.0, name: "e@SinOsc''_9_3__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_9_3" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_67", vars: [ { coef: 1.0, name: "e@SinOsc''_9_4__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_9_4" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_68", vars: [ { coef: 1.0, name: "e@SinOsc''_9_4__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_9_4" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_69", vars: [ { coef: 1.0, name: "e@SinOsc''_9_6__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_9_6" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_70", vars: [ { coef: 1.0, name: "e@SinOsc''_9_6__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_9_6" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_71", vars: [ { coef: 1.0, name: "e@SinOsc''_9_7__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_9_7" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_72", vars: [ { coef: 1.0, name: "e@SinOsc''_9_7__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_9_7" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_73", vars: [ { coef: 1.0, name: "e@SinOsc''_9_8__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_9_8" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_74", vars: [ { coef: 1.0, name: "e@SinOsc''_9_8__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_9_8" }, { coef: -0.75, name: "n@Gain''_6_5" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_75", vars: [ { coef: 1.0, name: "e@SinOsc''_9_9__Gain''_6_2" }, { coef: -0.75, name: "n@SinOsc''_9_9" }, { coef: -0.75, name: "n@Gain''_6_2" } ] }, { bnds: { lb: 0.0, type: 3, ub: 0.0 }, name: "edgec_76", vars: [ { coef: 1.0, name: "e@SinOsc''_9_9__Gain''_6_5" }, { coef: -0.75, name: "n@SinOsc''_9_9" }, { coef: -0.75, name: "n@Gain''_6_5" } ] } ] }
