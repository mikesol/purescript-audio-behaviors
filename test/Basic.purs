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
import FRP.Behavior.Audio (AudioProcessor, AudioUnit'(..), AudioParameter(..), AudioUnit''(..), SampleFrame, Status(..), audioGrouper, audioIO, audioIOInterleaved, audioToPtr, gain, gain', merger, sinOsc, speaker, speaker', split1, split3)
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
                    , (Tuple 1 { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 1 0) ]), ptr: 1, status: On })
                    , (Tuple 2 { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 2 0) ]), ptr: 2, status: On })
                    , (Tuple 3 { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 3 0) ]), ptr: 3, status: On })
                    , (Tuple 4 { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 4 0) ]), ptr: 4, status: On })
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
                    , (Tuple 2 { au: (Gain' (AudioParameter { param: 1.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1) ]), ptr: 2, status: On })
                    , (Tuple 3 { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 3 0) ]), ptr: 3, status: On })
                    , (Tuple 4 { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 4 0) ]), ptr: 4, status: On })
                    , (Tuple 5 { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 5 0) ]), ptr: 5, status: On })
                    , (Tuple 6 { au: (Gain' (AudioParameter { param: 0.9, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 6 0), (Tuple 7 1), (Tuple 8 1), (Tuple 9 1) ]), ptr: 6, status: On })
                    , (Tuple 7 { au: (SinOsc' (AudioParameter { param: 442.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 6 1) ]), prev: (fromFoldable [ (Tuple 7 0) ]), ptr: 7, status: On })
                    , (Tuple 8 { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 6 1) ]), prev: (fromFoldable [ (Tuple 8 0) ]), ptr: 8, status: On })
                    , (Tuple 9 { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 6 1) ]), prev: (fromFoldable [ (Tuple 9 0) ]), ptr: 9, status: On })
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
              $ split1
                  (sinOsc 440.0)
                  ( \v0 ->
                      speaker
                        ((V.head v0) :| ((V.head v0) : (V.head v0) : Nil))
                  )
        sp
          `shouldEqual`
            { flat:
                ( fromFoldable
                    [ (Tuple 0 { au: Speaker', chan: 1, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 1), (Tuple 3 1), (Tuple 4 2), (Tuple 5 3) ]), ptr: 0, status: On })
                    , (Tuple 1 { au: (SplitRes' 0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 1 0), (Tuple 4 2), (Tuple 5 3) ]), ptr: 1, status: On })
                    , (Tuple 2 { au: (SplitRes' 0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 2 0), (Tuple 4 2), (Tuple 5 3) ]), ptr: 2, status: On })
                    , (Tuple 3 { au: (SplitRes' 0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 3 0), (Tuple 4 2), (Tuple 5 3) ]), ptr: 3, status: On })
                    , (Tuple 4 { au: Splitter' 1, chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 1), (Tuple 3 1) ]), prev: (fromFoldable [ (Tuple 4 0), (Tuple 5 1) ]), ptr: 4, status: On })
                    , (Tuple 5 { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 2), (Tuple 3 2), (Tuple 4 1) ]), prev: (fromFoldable [ (Tuple 5 0) ]), ptr: 5, status: On })
                    ]
                )
            , len: 6
            , p:
                { au: Splitter' 1
                , chan: 1
                , name: Nothing
                , next:
                    ( fromFoldable
                        [ (Tuple 0 2)
                        , (Tuple 1 1)
                        , (Tuple 2 1)
                        , (Tuple 3 1)
                        ]
                    )
                , prev: (fromFoldable [ (Tuple 4 0), (Tuple 5 1) ])
                , ptr: 4
                , status: On
                }
            }
      it "should ignore split given to split" do
        let
          sp =
            audioToPtr
              $ split1
                  (sinOsc 440.0)
                  ( \v0 ->
                      split1
                        (V.head v0)
                        ( \v1 ->
                            split1
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
                    [ (Tuple 0 { au: Speaker', chan: 1, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 1), (Tuple 3 1), (Tuple 4 2), (Tuple 5 3) ]), ptr: 0, status: On })
                    , (Tuple 1 { au: (SplitRes' 0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 1 0), (Tuple 4 2), (Tuple 5 3) ]), ptr: 1, status: On })
                    , (Tuple 2 { au: (SplitRes' 0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 2 0), (Tuple 4 2), (Tuple 5 3) ]), ptr: 2, status: On })
                    , (Tuple 3 { au: (SplitRes' 0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 3 0), (Tuple 4 2), (Tuple 5 3) ]), ptr: 3, status: On })
                    , (Tuple 4 { au: Splitter' 1, chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 1), (Tuple 3 1) ]), prev: (fromFoldable [ (Tuple 4 0), (Tuple 5 1) ]), ptr: 4, status: On })
                    , (Tuple 5 { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 2), (Tuple 3 2), (Tuple 4 1) ]), prev: (fromFoldable [ (Tuple 5 0) ]), ptr: 5, status: On })
                    ]
                )
            , len: 6
            , p:
                { au: Splitter' 1
                , chan: 1
                , name: Nothing
                , next:
                    ( fromFoldable
                        [ (Tuple 0 2)
                        , (Tuple 1 1)
                        , (Tuple 2 1)
                        , (Tuple 3 1)
                        ]
                    )
                , prev: (fromFoldable [ (Tuple 4 0), (Tuple 5 1) ])
                , ptr: 4
                , status: On
                }
            }
      it "should correctly split" do
        let
          tree =
            audioToPtr
              $ split3
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
                    [ (Tuple 0 { au: Speaker', chan: 3, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 4), (Tuple 5 3), (Tuple 6 4), (Tuple 7 3), (Tuple 8 4), (Tuple 9 5), (Tuple 10 6), (Tuple 11 7), (Tuple 12 8), (Tuple 13 9), (Tuple 14 9), (Tuple 15 9), (Tuple 16 8), (Tuple 17 9), (Tuple 18 9), (Tuple 19 9), (Tuple 20 7), (Tuple 21 8), (Tuple 22 9), (Tuple 23 9), (Tuple 24 9), (Tuple 25 8), (Tuple 26 9), (Tuple 27 9), (Tuple 28 9), (Tuple 29 7), (Tuple 30 8), (Tuple 31 9), (Tuple 32 9), (Tuple 33 9), (Tuple 34 8), (Tuple 35 9), (Tuple 36 9), (Tuple 37 9) ]), ptr: 0, status: On })
                    , (Tuple 1 { au: (Gain' (AudioParameter { param: 0.5, timeOffset: 0.0 })), chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1), (Tuple 3 2), (Tuple 4 3), (Tuple 5 2), (Tuple 6 3), (Tuple 7 2), (Tuple 8 3), (Tuple 9 5), (Tuple 10 6), (Tuple 11 7), (Tuple 12 8), (Tuple 13 9), (Tuple 14 9), (Tuple 15 9), (Tuple 16 8), (Tuple 17 9), (Tuple 18 9), (Tuple 19 9), (Tuple 20 7), (Tuple 21 8), (Tuple 22 9), (Tuple 23 9), (Tuple 24 9), (Tuple 25 8), (Tuple 26 9), (Tuple 27 9), (Tuple 28 9), (Tuple 29 7), (Tuple 30 8), (Tuple 31 9), (Tuple 32 9), (Tuple 33 9), (Tuple 34 8), (Tuple 35 9), (Tuple 36 9), (Tuple 37 9) ]), ptr: 1, status: On })
                    , (Tuple 2 { au: (Merger' (7 : 5 : 3 : Nil)), chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 2), (Tuple 5 1), (Tuple 6 2), (Tuple 7 1), (Tuple 8 2), (Tuple 9 5), (Tuple 10 6), (Tuple 11 7), (Tuple 12 8), (Tuple 13 9), (Tuple 14 9), (Tuple 15 9), (Tuple 16 8), (Tuple 17 9), (Tuple 18 9), (Tuple 19 9), (Tuple 20 7), (Tuple 21 8), (Tuple 22 9), (Tuple 23 9), (Tuple 24 9), (Tuple 25 8), (Tuple 26 9), (Tuple 27 9), (Tuple 28 9), (Tuple 29 7), (Tuple 30 8), (Tuple 31 9), (Tuple 32 9), (Tuple 33 9), (Tuple 34 8), (Tuple 35 9), (Tuple 36 9), (Tuple 37 9) ]), ptr: 2, status: On })
                    , (Tuple 3 { au: (Gain' (AudioParameter { param: 0.3, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 3 0), (Tuple 4 1), (Tuple 9 5), (Tuple 10 6), (Tuple 11 7), (Tuple 12 8), (Tuple 13 9), (Tuple 14 9), (Tuple 15 9), (Tuple 16 8), (Tuple 17 9), (Tuple 18 9), (Tuple 19 9), (Tuple 20 7), (Tuple 21 8), (Tuple 22 9), (Tuple 23 9), (Tuple 24 9), (Tuple 25 8), (Tuple 26 9), (Tuple 27 9), (Tuple 28 9), (Tuple 29 7), (Tuple 30 8), (Tuple 31 9), (Tuple 32 9), (Tuple 33 9), (Tuple 34 8), (Tuple 35 9), (Tuple 36 9), (Tuple 37 9) ]), ptr: 3, status: On })
                    , (Tuple 4 { au: (SplitRes' 0), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 3), (Tuple 2 2), (Tuple 3 1) ]), prev: (fromFoldable [ (Tuple 4 0), (Tuple 9 5), (Tuple 10 6), (Tuple 11 7), (Tuple 12 8), (Tuple 13 9), (Tuple 14 9), (Tuple 15 9), (Tuple 16 8), (Tuple 17 9), (Tuple 18 9), (Tuple 19 9), (Tuple 20 7), (Tuple 21 8), (Tuple 22 9), (Tuple 23 9), (Tuple 24 9), (Tuple 25 8), (Tuple 26 9), (Tuple 27 9), (Tuple 28 9), (Tuple 29 7), (Tuple 30 8), (Tuple 31 9), (Tuple 32 9), (Tuple 33 9), (Tuple 34 8), (Tuple 35 9), (Tuple 36 9), (Tuple 37 9) ]), ptr: 4, status: On })
                    , (Tuple 5 { au: (Gain' (AudioParameter { param: 0.3, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 5 0), (Tuple 6 1), (Tuple 9 5), (Tuple 10 6), (Tuple 11 7), (Tuple 12 8), (Tuple 13 9), (Tuple 14 9), (Tuple 15 9), (Tuple 16 8), (Tuple 17 9), (Tuple 18 9), (Tuple 19 9), (Tuple 20 7), (Tuple 21 8), (Tuple 22 9), (Tuple 23 9), (Tuple 24 9), (Tuple 25 8), (Tuple 26 9), (Tuple 27 9), (Tuple 28 9), (Tuple 29 7), (Tuple 30 8), (Tuple 31 9), (Tuple 32 9), (Tuple 33 9), (Tuple 34 8), (Tuple 35 9), (Tuple 36 9), (Tuple 37 9) ]), ptr: 5, status: On })
                    , (Tuple 6 { au: (SplitRes' 1), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 3), (Tuple 2 2), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 6 0), (Tuple 9 5), (Tuple 10 6), (Tuple 11 7), (Tuple 12 8), (Tuple 13 9), (Tuple 14 9), (Tuple 15 9), (Tuple 16 8), (Tuple 17 9), (Tuple 18 9), (Tuple 19 9), (Tuple 20 7), (Tuple 21 8), (Tuple 22 9), (Tuple 23 9), (Tuple 24 9), (Tuple 25 8), (Tuple 26 9), (Tuple 27 9), (Tuple 28 9), (Tuple 29 7), (Tuple 30 8), (Tuple 31 9), (Tuple 32 9), (Tuple 33 9), (Tuple 34 8), (Tuple 35 9), (Tuple 36 9), (Tuple 37 9) ]), ptr: 6, status: On })
                    , (Tuple 7 { au: (Gain' (AudioParameter { param: 0.3, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 7 0), (Tuple 8 1), (Tuple 9 5), (Tuple 10 6), (Tuple 11 7), (Tuple 12 8), (Tuple 13 9), (Tuple 14 9), (Tuple 15 9), (Tuple 16 8), (Tuple 17 9), (Tuple 18 9), (Tuple 19 9), (Tuple 20 7), (Tuple 21 8), (Tuple 22 9), (Tuple 23 9), (Tuple 24 9), (Tuple 25 8), (Tuple 26 9), (Tuple 27 9), (Tuple 28 9), (Tuple 29 7), (Tuple 30 8), (Tuple 31 9), (Tuple 32 9), (Tuple 33 9), (Tuple 34 8), (Tuple 35 9), (Tuple 36 9), (Tuple 37 9) ]), ptr: 7, status: On })
                    , (Tuple 8 { au: (SplitRes' 2), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 3), (Tuple 2 2), (Tuple 7 1) ]), prev: (fromFoldable [ (Tuple 8 0), (Tuple 9 5), (Tuple 10 6), (Tuple 11 7), (Tuple 12 8), (Tuple 13 9), (Tuple 14 9), (Tuple 15 9), (Tuple 16 8), (Tuple 17 9), (Tuple 18 9), (Tuple 19 9), (Tuple 20 7), (Tuple 21 8), (Tuple 22 9), (Tuple 23 9), (Tuple 24 9), (Tuple 25 8), (Tuple 26 9), (Tuple 27 9), (Tuple 28 9), (Tuple 29 7), (Tuple 30 8), (Tuple 31 9), (Tuple 32 9), (Tuple 33 9), (Tuple 34 8), (Tuple 35 9), (Tuple 36 9), (Tuple 37 9) ]), ptr: 8, status: On })
                    , (Tuple 9 { au: Splitter' 3, chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 5), (Tuple 1 4), (Tuple 2 3), (Tuple 3 2), (Tuple 4 1), (Tuple 5 2), (Tuple 6 1), (Tuple 7 2), (Tuple 8 1) ]), prev: (fromFoldable [ (Tuple 9 0), (Tuple 10 1), (Tuple 11 2), (Tuple 12 3), (Tuple 13 4), (Tuple 14 4), (Tuple 15 4), (Tuple 16 3), (Tuple 17 4), (Tuple 18 4), (Tuple 19 4), (Tuple 20 2), (Tuple 21 3), (Tuple 22 4), (Tuple 23 4), (Tuple 24 4), (Tuple 25 3), (Tuple 26 4), (Tuple 27 4), (Tuple 28 4), (Tuple 29 2), (Tuple 30 3), (Tuple 31 4), (Tuple 32 4), (Tuple 33 4), (Tuple 34 3), (Tuple 35 4), (Tuple 36 4), (Tuple 37 4) ]), ptr: 9, status: On })
                    , (Tuple 10 { au: (Merger' (29 : 20 : 11 : Nil)), chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 5), (Tuple 2 4), (Tuple 3 3), (Tuple 4 2), (Tuple 5 3), (Tuple 6 2), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 10 0), (Tuple 11 1), (Tuple 12 2), (Tuple 13 3), (Tuple 14 3), (Tuple 15 3), (Tuple 16 2), (Tuple 17 3), (Tuple 18 3), (Tuple 19 3), (Tuple 20 1), (Tuple 21 2), (Tuple 22 3), (Tuple 23 3), (Tuple 24 3), (Tuple 25 2), (Tuple 26 3), (Tuple 27 3), (Tuple 28 3), (Tuple 29 1), (Tuple 30 2), (Tuple 31 3), (Tuple 32 3), (Tuple 33 3), (Tuple 34 2), (Tuple 35 3), (Tuple 36 3), (Tuple 37 3) ]), ptr: 10, status: On })
                    , (Tuple 11 { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 7), (Tuple 1 6), (Tuple 2 5), (Tuple 3 4), (Tuple 4 3), (Tuple 5 4), (Tuple 6 3), (Tuple 7 4), (Tuple 8 3), (Tuple 9 2), (Tuple 10 1) ]), prev: (fromFoldable [ (Tuple 11 0), (Tuple 12 1), (Tuple 13 2), (Tuple 14 2), (Tuple 15 2), (Tuple 16 1), (Tuple 17 2), (Tuple 18 2), (Tuple 19 2) ]), ptr: 11, status: On })
                    , (Tuple 12 { au: (Gain' (AudioParameter { param: 1.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 4), (Tuple 5 5), (Tuple 6 4), (Tuple 7 5), (Tuple 8 4), (Tuple 9 3), (Tuple 10 2), (Tuple 11 1) ]), prev: (fromFoldable [ (Tuple 12 0), (Tuple 13 1), (Tuple 14 1), (Tuple 15 1) ]), ptr: 12, status: On })
                    , (Tuple 13 { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 11 2), (Tuple 12 1) ]), prev: (fromFoldable [ (Tuple 13 0) ]), ptr: 13, status: On })
                    , (Tuple 14 { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 11 2), (Tuple 12 1) ]), prev: (fromFoldable [ (Tuple 14 0) ]), ptr: 14, status: On })
                    , (Tuple 15 { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 11 2), (Tuple 12 1) ]), prev: (fromFoldable [ (Tuple 15 0) ]), ptr: 15, status: On })
                    , (Tuple 16 { au: (Gain' (AudioParameter { param: 0.9, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 4), (Tuple 5 5), (Tuple 6 4), (Tuple 7 5), (Tuple 8 4), (Tuple 9 3), (Tuple 10 2), (Tuple 11 1) ]), prev: (fromFoldable [ (Tuple 16 0), (Tuple 17 1), (Tuple 18 1), (Tuple 19 1) ]), ptr: 16, status: On })
                    , (Tuple 17 { au: (SinOsc' (AudioParameter { param: 442.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 11 2), (Tuple 16 1) ]), prev: (fromFoldable [ (Tuple 17 0) ]), ptr: 17, status: On })
                    , (Tuple 18 { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 11 2), (Tuple 16 1) ]), prev: (fromFoldable [ (Tuple 18 0) ]), ptr: 18, status: On })
                    , (Tuple 19 { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 11 2), (Tuple 16 1) ]), prev: (fromFoldable [ (Tuple 19 0) ]), ptr: 19, status: On })
                    , (Tuple 20 { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 7), (Tuple 1 6), (Tuple 2 5), (Tuple 3 4), (Tuple 4 3), (Tuple 5 4), (Tuple 6 3), (Tuple 7 4), (Tuple 8 3), (Tuple 9 2), (Tuple 10 1) ]), prev: (fromFoldable [ (Tuple 20 0), (Tuple 21 1), (Tuple 22 2), (Tuple 23 2), (Tuple 24 2), (Tuple 25 1), (Tuple 26 2), (Tuple 27 2), (Tuple 28 2) ]), ptr: 20, status: On })
                    , (Tuple 21 { au: (Gain' (AudioParameter { param: 1.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 4), (Tuple 5 5), (Tuple 6 4), (Tuple 7 5), (Tuple 8 4), (Tuple 9 3), (Tuple 10 2), (Tuple 20 1) ]), prev: (fromFoldable [ (Tuple 21 0), (Tuple 22 1), (Tuple 23 1), (Tuple 24 1) ]), ptr: 21, status: On })
                    , (Tuple 22 { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 20 2), (Tuple 21 1) ]), prev: (fromFoldable [ (Tuple 22 0) ]), ptr: 22, status: On })
                    , (Tuple 23 { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 20 2), (Tuple 21 1) ]), prev: (fromFoldable [ (Tuple 23 0) ]), ptr: 23, status: On })
                    , (Tuple 24 { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 20 2), (Tuple 21 1) ]), prev: (fromFoldable [ (Tuple 24 0) ]), ptr: 24, status: On })
                    , (Tuple 25 { au: (Gain' (AudioParameter { param: 0.9, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 4), (Tuple 5 5), (Tuple 6 4), (Tuple 7 5), (Tuple 8 4), (Tuple 9 3), (Tuple 10 2), (Tuple 20 1) ]), prev: (fromFoldable [ (Tuple 25 0), (Tuple 26 1), (Tuple 27 1), (Tuple 28 1) ]), ptr: 25, status: On })
                    , (Tuple 26 { au: (SinOsc' (AudioParameter { param: 442.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 20 2), (Tuple 25 1) ]), prev: (fromFoldable [ (Tuple 26 0) ]), ptr: 26, status: On })
                    , (Tuple 27 { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 20 2), (Tuple 25 1) ]), prev: (fromFoldable [ (Tuple 27 0) ]), ptr: 27, status: On })
                    , (Tuple 28 { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 20 2), (Tuple 25 1) ]), prev: (fromFoldable [ (Tuple 28 0) ]), ptr: 28, status: On })
                    , (Tuple 29 { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 7), (Tuple 1 6), (Tuple 2 5), (Tuple 3 4), (Tuple 4 3), (Tuple 5 4), (Tuple 6 3), (Tuple 7 4), (Tuple 8 3), (Tuple 9 2), (Tuple 10 1) ]), prev: (fromFoldable [ (Tuple 29 0), (Tuple 30 1), (Tuple 31 2), (Tuple 32 2), (Tuple 33 2), (Tuple 34 1), (Tuple 35 2), (Tuple 36 2), (Tuple 37 2) ]), ptr: 29, status: On })
                    , (Tuple 30 { au: (Gain' (AudioParameter { param: 1.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 4), (Tuple 5 5), (Tuple 6 4), (Tuple 7 5), (Tuple 8 4), (Tuple 9 3), (Tuple 10 2), (Tuple 29 1) ]), prev: (fromFoldable [ (Tuple 30 0), (Tuple 31 1), (Tuple 32 1), (Tuple 33 1) ]), ptr: 30, status: On })
                    , (Tuple 31 { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 29 2), (Tuple 30 1) ]), prev: (fromFoldable [ (Tuple 31 0) ]), ptr: 31, status: On })
                    , (Tuple 32 { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 29 2), (Tuple 30 1) ]), prev: (fromFoldable [ (Tuple 32 0) ]), ptr: 32, status: On })
                    , (Tuple 33 { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 29 2), (Tuple 30 1) ]), prev: (fromFoldable [ (Tuple 33 0) ]), ptr: 33, status: On })
                    , (Tuple 34 { au: (Gain' (AudioParameter { param: 0.9, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 8), (Tuple 1 7), (Tuple 2 6), (Tuple 3 5), (Tuple 4 4), (Tuple 5 5), (Tuple 6 4), (Tuple 7 5), (Tuple 8 4), (Tuple 9 3), (Tuple 10 2), (Tuple 29 1) ]), prev: (fromFoldable [ (Tuple 34 0), (Tuple 35 1), (Tuple 36 1), (Tuple 37 1) ]), ptr: 34, status: On })
                    , (Tuple 35 { au: (SinOsc' (AudioParameter { param: 442.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 29 2), (Tuple 34 1) ]), prev: (fromFoldable [ (Tuple 35 0) ]), ptr: 35, status: On })
                    , (Tuple 36 { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 29 2), (Tuple 34 1) ]), prev: (fromFoldable [ (Tuple 36 0) ]), ptr: 36, status: On })
                    , (Tuple 37 { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 9), (Tuple 1 8), (Tuple 2 7), (Tuple 3 6), (Tuple 4 5), (Tuple 5 6), (Tuple 6 5), (Tuple 7 6), (Tuple 8 5), (Tuple 9 4), (Tuple 10 3), (Tuple 29 2), (Tuple 34 1) ]), prev: (fromFoldable [ (Tuple 37 0) ]), ptr: 37, status: On })
                    ]
                )
            , len: 38
            , p:
                { au: Splitter' 3
                , chan: 3
                , name: Nothing
                , next: (fromFoldable [ (Tuple 0 5), (Tuple 1 4), (Tuple 2 3), (Tuple 3 2), (Tuple 4 1), (Tuple 5 2), (Tuple 6 1), (Tuple 7 2), (Tuple 8 1) ])
                , prev:
                    ( fromFoldable
                        [ (Tuple 9 0)
                        , (Tuple 10 1)
                        , (Tuple 11 2)
                        , (Tuple 12 3)
                        , (Tuple 13 4)
                        , (Tuple 14 4)
                        , (Tuple 15 4)
                        , (Tuple 16 3)
                        , (Tuple 17 4)
                        , (Tuple 18 4)
                        , (Tuple 19 4)
                        , (Tuple 20 2)
                        , (Tuple 21 3)
                        , (Tuple 22 4)
                        , (Tuple 23 4)
                        , (Tuple 24 4)
                        , (Tuple 25 3)
                        , (Tuple 26 4)
                        , (Tuple 27 4)
                        , (Tuple 28 4)
                        , (Tuple 29 2)
                        , (Tuple 30 3)
                        , (Tuple 31 4)
                        , (Tuple 32 4)
                        , (Tuple 33 4)
                        , (Tuple 34 3)
                        , (Tuple 35 4)
                        , (Tuple 36 4)
                        , (Tuple 37 4)
                        ]
                    )
                , ptr: 9
                , status: On
                }
            }
    describe "Audio grouper" do
      it "should group correctly" do
        let
          ag =
            (audioGrouper <<< DL.fromFoldable <<< map snd)
              ( [ (Tuple 0 { au: Speaker', chan: 3, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 0, status: On })
                , (Tuple 1 { au: (Gain' (AudioParameter { param: 0.5, timeOffset: 0.0 })), chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 0) ]), prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1), (Tuple 3 2), (Tuple 4 2), (Tuple 5 2), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 1, status: On })
                , (Tuple 2 { au: Merger' Nil, chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 1), (Tuple 1 0) ]), prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 2, status: On })
                , (Tuple 3 { au: (Gain' (AudioParameter { param: 0.3, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), prev: (fromFoldable [ (Tuple 3 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 3, status: On })
                , (Tuple 4 { au: (Gain' (AudioParameter { param: 0.3, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), prev: (fromFoldable [ (Tuple 4 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 4, status: On })
                , (Tuple 5 { au: (Gain' (AudioParameter { param: 0.3, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), prev: (fromFoldable [ (Tuple 5 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 5, status: On })
                , (Tuple 6 { au: Splitter' 3, chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 3), (Tuple 2 2), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 6 0), (Tuple 7 1), (Tuple 8 2), (Tuple 9 3), (Tuple 10 4), (Tuple 11 4), (Tuple 12 4), (Tuple 13 3), (Tuple 14 4), (Tuple 15 4), (Tuple 16 4), (Tuple 17 2), (Tuple 18 3), (Tuple 19 4), (Tuple 20 4), (Tuple 21 4), (Tuple 22 3), (Tuple 23 4), (Tuple 24 4), (Tuple 25 4), (Tuple 26 2), (Tuple 27 3), (Tuple 28 4), (Tuple 29 4), (Tuple 30 4), (Tuple 31 3), (Tuple 32 4), (Tuple 33 4), (Tuple 34 4) ]), ptr: 6, status: On })
                , (Tuple 7 { au: Merger' Nil, chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 3), (Tuple 2 4), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 1) ]), prev: (fromFoldable [ (Tuple 7 0), (Tuple 8 1), (Tuple 9 2), (Tuple 10 3), (Tuple 11 3), (Tuple 12 3), (Tuple 13 2), (Tuple 14 3), (Tuple 15 3), (Tuple 16 3), (Tuple 17 1), (Tuple 18 2), (Tuple 19 3), (Tuple 20 3), (Tuple 21 3), (Tuple 22 2), (Tuple 23 3), (Tuple 24 3), (Tuple 25 3), (Tuple 26 1), (Tuple 27 2), (Tuple 28 3), (Tuple 29 3), (Tuple 30 3), (Tuple 31 2), (Tuple 32 3), (Tuple 33 3), (Tuple 34 3) ]), ptr: 7, status: On })
                , (Tuple 8 { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2), (Tuple 7 0) ]), prev: (fromFoldable [ (Tuple 8 0), (Tuple 9 1), (Tuple 10 2), (Tuple 11 2), (Tuple 12 2), (Tuple 13 1), (Tuple 14 2), (Tuple 15 2), (Tuple 16 2) ]), ptr: 8, status: On })
                , (Tuple 9 { au: (Gain' (AudioParameter { param: 1.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 8 0) ]), prev: (fromFoldable [ (Tuple 9 0), (Tuple 10 1), (Tuple 11 1), (Tuple 12 1) ]), ptr: 9, status: On })
                , (Tuple 10 { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 10 0) ]), ptr: 10, status: On })
                , (Tuple 11 { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 11 0) ]), ptr: 11, status: On })
                , (Tuple 12 { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 12 0) ]), ptr: 12, status: On })
                , (Tuple 13 { au: (Gain' (AudioParameter { param: 0.9, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 8 0) ]), prev: (fromFoldable [ (Tuple 13 0), (Tuple 14 1), (Tuple 15 1), (Tuple 16 1) ]), ptr: 13, status: On })
                , (Tuple 14 { au: (SinOsc' (AudioParameter { param: 442.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), prev: (fromFoldable [ (Tuple 14 0) ]), ptr: 14, status: On })
                , (Tuple 15 { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), prev: (fromFoldable [ (Tuple 15 0) ]), ptr: 15, status: On })
                , (Tuple 16 { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), prev: (fromFoldable [ (Tuple 16 0) ]), ptr: 16, status: On })
                , (Tuple 17 { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2), (Tuple 7 0) ]), prev: (fromFoldable [ (Tuple 17 0), (Tuple 18 1), (Tuple 19 2), (Tuple 20 2), (Tuple 21 2), (Tuple 22 1), (Tuple 23 2), (Tuple 24 2), (Tuple 25 2) ]), ptr: 17, status: On })
                , (Tuple 18 { au: (Gain' (AudioParameter { param: 1.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 17 0) ]), prev: (fromFoldable [ (Tuple 18 0), (Tuple 19 1), (Tuple 20 1), (Tuple 21 1) ]), ptr: 18, status: On })
                , (Tuple 19 { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 18 1) ]), prev: (fromFoldable [ (Tuple 19 0) ]), ptr: 19, status: On })
                , (Tuple 20 { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 18 1) ]), prev: (fromFoldable [ (Tuple 20 0) ]), ptr: 20, status: On })
                , (Tuple 21 { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 18 1) ]), prev: (fromFoldable [ (Tuple 21 0) ]), ptr: 21, status: On })
                , (Tuple 22 { au: (Gain' (AudioParameter { param: 0.9, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 17 0) ]), prev: (fromFoldable [ (Tuple 22 0), (Tuple 23 1), (Tuple 24 1), (Tuple 25 1) ]), ptr: 22, status: On })
                , (Tuple 23 { au: (SinOsc' (AudioParameter { param: 442.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 22 1) ]), prev: (fromFoldable [ (Tuple 23 0) ]), ptr: 23, status: On })
                , (Tuple 24 { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 22 1) ]), prev: (fromFoldable [ (Tuple 24 0) ]), ptr: 24, status: On })
                , (Tuple 25 { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 22 1) ]), prev: (fromFoldable [ (Tuple 25 0) ]), ptr: 25, status: On })
                , (Tuple 26 { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2), (Tuple 7 0) ]), prev: (fromFoldable [ (Tuple 26 0), (Tuple 27 1), (Tuple 28 2), (Tuple 29 2), (Tuple 30 2), (Tuple 31 1), (Tuple 32 2), (Tuple 33 2), (Tuple 34 2) ]), ptr: 26, status: On })
                , (Tuple 27 { au: (Gain' (AudioParameter { param: 1.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 26 0) ]), prev: (fromFoldable [ (Tuple 27 0), (Tuple 28 1), (Tuple 29 1), (Tuple 30 1) ]), ptr: 27, status: On })
                , (Tuple 28 { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 27 1) ]), prev: (fromFoldable [ (Tuple 28 0) ]), ptr: 28, status: On })
                , (Tuple 29 { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 27 1) ]), prev: (fromFoldable [ (Tuple 29 0) ]), ptr: 29, status: On })
                , (Tuple 30 { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 27 1) ]), prev: (fromFoldable [ (Tuple 30 0) ]), ptr: 30, status: On })
                , (Tuple 31 { au: (Gain' (AudioParameter { param: 0.9, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 26 0) ]), prev: (fromFoldable [ (Tuple 31 0), (Tuple 32 1), (Tuple 33 1), (Tuple 34 1) ]), ptr: 31, status: On })
                , (Tuple 32 { au: (SinOsc' (AudioParameter { param: 442.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 31 1) ]), prev: (fromFoldable [ (Tuple 32 0) ]), ptr: 32, status: On })
                , (Tuple 33 { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 31 1) ]), prev: (fromFoldable [ (Tuple 33 0) ]), ptr: 33, status: On })
                , (Tuple 34 { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 31 1) ]), prev: (fromFoldable [ (Tuple 34 0) ]), ptr: 34, status: On })
                ]
              )
        ag `shouldEqual` (fromFoldable [ (Tuple { chan: 1, name: Nothing, tag: Add'' } (NonEmpty { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2), (Tuple 7 0) ]), prev: (fromFoldable [ (Tuple 8 0), (Tuple 9 1), (Tuple 10 2), (Tuple 11 2), (Tuple 12 2), (Tuple 13 1), (Tuple 14 2), (Tuple 15 2), (Tuple 16 2) ]), ptr: 8, status: On } ({ au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2), (Tuple 7 0) ]), prev: (fromFoldable [ (Tuple 17 0), (Tuple 18 1), (Tuple 19 2), (Tuple 20 2), (Tuple 21 2), (Tuple 22 1), (Tuple 23 2), (Tuple 24 2), (Tuple 25 2) ]), ptr: 17, status: On } : { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 4), (Tuple 2 5), (Tuple 3 6), (Tuple 4 6), (Tuple 5 6), (Tuple 6 2), (Tuple 7 0) ]), prev: (fromFoldable [ (Tuple 26 0), (Tuple 27 1), (Tuple 28 2), (Tuple 29 2), (Tuple 30 2), (Tuple 31 1), (Tuple 32 2), (Tuple 33 2), (Tuple 34 2) ]), ptr: 26, status: On } : Nil))), (Tuple { chan: 1, name: Nothing, tag: Gain'' } (NonEmpty { au: (Gain' (AudioParameter { param: 0.3, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), prev: (fromFoldable [ (Tuple 3 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 3, status: On } ({ au: (Gain' (AudioParameter { param: 0.3, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), prev: (fromFoldable [ (Tuple 4 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 4, status: On } : { au: (Gain' (AudioParameter { param: 0.3, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1), (Tuple 2 0) ]), prev: (fromFoldable [ (Tuple 5 0), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 5, status: On } : { au: (Gain' (AudioParameter { param: 1.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 8 0) ]), prev: (fromFoldable [ (Tuple 9 0), (Tuple 10 1), (Tuple 11 1), (Tuple 12 1) ]), ptr: 9, status: On } : { au: (Gain' (AudioParameter { param: 0.9, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 8 0) ]), prev: (fromFoldable [ (Tuple 13 0), (Tuple 14 1), (Tuple 15 1), (Tuple 16 1) ]), ptr: 13, status: On } : { au: (Gain' (AudioParameter { param: 1.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 17 0) ]), prev: (fromFoldable [ (Tuple 18 0), (Tuple 19 1), (Tuple 20 1), (Tuple 21 1) ]), ptr: 18, status: On } : { au: (Gain' (AudioParameter { param: 0.9, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 17 0) ]), prev: (fromFoldable [ (Tuple 22 0), (Tuple 23 1), (Tuple 24 1), (Tuple 25 1) ]), ptr: 22, status: On } : { au: (Gain' (AudioParameter { param: 1.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 26 0) ]), prev: (fromFoldable [ (Tuple 27 0), (Tuple 28 1), (Tuple 29 1), (Tuple 30 1) ]), ptr: 27, status: On } : { au: (Gain' (AudioParameter { param: 0.9, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 5), (Tuple 2 6), (Tuple 3 7), (Tuple 4 7), (Tuple 5 7), (Tuple 6 3), (Tuple 7 1), (Tuple 26 0) ]), prev: (fromFoldable [ (Tuple 31 0), (Tuple 32 1), (Tuple 33 1), (Tuple 34 1) ]), ptr: 31, status: On } : Nil))), (Tuple { chan: 1, name: Nothing, tag: SinOsc'' } (NonEmpty { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 10 0) ]), ptr: 10, status: On } ({ au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 11 0) ]), ptr: 11, status: On } : { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 9 1) ]), prev: (fromFoldable [ (Tuple 12 0) ]), ptr: 12, status: On } : { au: (SinOsc' (AudioParameter { param: 442.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), prev: (fromFoldable [ (Tuple 14 0) ]), ptr: 14, status: On } : { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), prev: (fromFoldable [ (Tuple 15 0) ]), ptr: 15, status: On } : { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 8 2), (Tuple 13 1) ]), prev: (fromFoldable [ (Tuple 16 0) ]), ptr: 16, status: On } : { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 18 1) ]), prev: (fromFoldable [ (Tuple 19 0) ]), ptr: 19, status: On } : { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 18 1) ]), prev: (fromFoldable [ (Tuple 20 0) ]), ptr: 20, status: On } : { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 18 1) ]), prev: (fromFoldable [ (Tuple 21 0) ]), ptr: 21, status: On } : { au: (SinOsc' (AudioParameter { param: 442.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 22 1) ]), prev: (fromFoldable [ (Tuple 23 0) ]), ptr: 23, status: On } : { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 22 1) ]), prev: (fromFoldable [ (Tuple 24 0) ]), ptr: 24, status: On } : { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 17 2), (Tuple 22 1) ]), prev: (fromFoldable [ (Tuple 25 0) ]), ptr: 25, status: On } : { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 27 1) ]), prev: (fromFoldable [ (Tuple 28 0) ]), ptr: 28, status: On } : { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 27 1) ]), prev: (fromFoldable [ (Tuple 29 0) ]), ptr: 29, status: On } : { au: (SinOsc' (AudioParameter { param: 441.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 27 1) ]), prev: (fromFoldable [ (Tuple 30 0) ]), ptr: 30, status: On } : { au: (SinOsc' (AudioParameter { param: 442.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 31 1) ]), prev: (fromFoldable [ (Tuple 32 0) ]), ptr: 32, status: On } : { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 31 1) ]), prev: (fromFoldable [ (Tuple 33 0) ]), ptr: 33, status: On } : { au: (SinOsc' (AudioParameter { param: 443.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 6), (Tuple 1 7), (Tuple 2 8), (Tuple 3 9), (Tuple 4 9), (Tuple 5 9), (Tuple 6 5), (Tuple 7 3), (Tuple 26 2), (Tuple 31 1) ]), prev: (fromFoldable [ (Tuple 34 0) ]), ptr: 34, status: On } : Nil))), (Tuple { chan: 3, name: Nothing, tag: Gain'' } (NonEmpty { au: (Gain' (AudioParameter { param: 0.5, timeOffset: 0.0 })), chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 0) ]), prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1), (Tuple 3 2), (Tuple 4 2), (Tuple 5 2), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 1, status: On } Nil)), (Tuple { chan: 3, name: Nothing, tag: Merger'' } (NonEmpty { au: Merger' Nil, chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 1), (Tuple 1 0) ]), prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 2, status: On } ({ au: Merger' Nil, chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 3), (Tuple 2 4), (Tuple 3 5), (Tuple 4 5), (Tuple 5 5), (Tuple 6 1) ]), prev: (fromFoldable [ (Tuple 7 0), (Tuple 8 1), (Tuple 9 2), (Tuple 10 3), (Tuple 11 3), (Tuple 12 3), (Tuple 13 2), (Tuple 14 3), (Tuple 15 3), (Tuple 16 3), (Tuple 17 1), (Tuple 18 2), (Tuple 19 3), (Tuple 20 3), (Tuple 21 3), (Tuple 22 2), (Tuple 23 3), (Tuple 24 3), (Tuple 25 3), (Tuple 26 1), (Tuple 27 2), (Tuple 28 3), (Tuple 29 3), (Tuple 30 3), (Tuple 31 2), (Tuple 32 3), (Tuple 33 3), (Tuple 34 3) ]), ptr: 7, status: On } : Nil))), (Tuple { chan: 3, name: Nothing, tag: Speaker'' } (NonEmpty { au: Speaker', chan: 3, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 4), (Tuple 7 5), (Tuple 8 6), (Tuple 9 7), (Tuple 10 8), (Tuple 11 8), (Tuple 12 8), (Tuple 13 7), (Tuple 14 8), (Tuple 15 8), (Tuple 16 8), (Tuple 17 6), (Tuple 18 7), (Tuple 19 8), (Tuple 20 8), (Tuple 21 8), (Tuple 22 7), (Tuple 23 8), (Tuple 24 8), (Tuple 25 8), (Tuple 26 6), (Tuple 27 7), (Tuple 28 8), (Tuple 29 8), (Tuple 30 8), (Tuple 31 7), (Tuple 32 8), (Tuple 33 8), (Tuple 34 8) ]), ptr: 0, status: On } Nil)), (Tuple { chan: 3, name: Nothing, tag: Splitter'' } (NonEmpty { au: Splitter' 3, chan: 3, name: Nothing, next: (fromFoldable [ (Tuple 0 4), (Tuple 1 3), (Tuple 2 2), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 6 0), (Tuple 7 1), (Tuple 8 2), (Tuple 9 3), (Tuple 10 4), (Tuple 11 4), (Tuple 12 4), (Tuple 13 3), (Tuple 14 4), (Tuple 15 4), (Tuple 16 4), (Tuple 17 2), (Tuple 18 3), (Tuple 19 4), (Tuple 20 4), (Tuple 21 4), (Tuple 22 3), (Tuple 23 4), (Tuple 24 4), (Tuple 25 4), (Tuple 26 2), (Tuple 27 3), (Tuple 28 4), (Tuple 29 4), (Tuple 30 4), (Tuple 31 3), (Tuple 32 4), (Tuple 33 4), (Tuple 34 4) ]), ptr: 6, status: On } Nil)) ])
