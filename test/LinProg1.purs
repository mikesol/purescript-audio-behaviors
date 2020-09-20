module Test.LinProg1 where

import Prelude
import Control.Promise (toAffE)
import Data.Int (toNumber)
import Data.List (range, List(..), (:))
import Data.List as DL
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|), NonEmpty(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import FRP.Behavior.Audio (AudioUnit'(..), AudioUnit''(..), Instruction(..), Status(..), audioGrouper, audioReconciliation'', audioToPtr, gain, getGlpkImpl, sinOsc, speaker', AudioParameter(..))
import Foreign (Foreign)
import Test.Spec (SpecT, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

getGLPK :: Aff Foreign
getGLPK = toAffE getGlpkImpl

linprogTestSuite :: forall m. Monad m => SpecT Aff Unit m Unit
linprogTestSuite =
  describe "linprog suite 1" do
    describe "Linear program generation" do
      before getGLPK do
        it "Should generate a correct linear program for two audio graphs" \glpk -> do
          let
            tree0 =
              audioToPtr
                $ speaker'
                    ( ( gain 1.0
                          ( (sinOsc 440.0)
                              :| (map (sinOsc <<< (440.0 * _) <<< toNumber) $ range 2 4)
                          )
                      )
                        + ( gain 0.9
                              ( (sinOsc 442.0)
                                  :| (map (sinOsc <<< (442.0 * _) <<< toNumber) $ range 2 3)
                              )
                          )
                    )
          let
            tree1 =
              audioToPtr
                $ speaker'
                    ( ( gain 0.4
                          ( (sinOsc 481.0)
                              :| (map (sinOsc <<< (481.0 * _) <<< toNumber) $ range 2 2)
                          )
                      )
                        + ( gain 0.3
                              ( (sinOsc 413.0)
                                  :| (map (sinOsc <<< (413.0 * _) <<< toNumber) $ range 2 5)
                              )
                          )
                    )
          let
            rekon =
              audioReconciliation'' glpk
                { flat: tree0.flat, grouped: audioGrouper (DL.fromFoldable tree0.flat)
                }
                { flat: tree1.flat, grouped: audioGrouper (DL.fromFoldable tree1.flat) }
          rekon
            `shouldEqual`
              { cur:
                  { flat:
                      ( fromFoldable
                          [ (Tuple 0 { au: Speaker', chan: 1, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 3), (Tuple 5 2), (Tuple 6 3), (Tuple 7 3), (Tuple 8 3), (Tuple 9 3), (Tuple 10 3) ]), ptr: 0, status: On })
                          , (Tuple 1 { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1), (Tuple 3 2), (Tuple 4 2), (Tuple 5 1), (Tuple 6 2), (Tuple 7 2), (Tuple 8 2), (Tuple 9 2), (Tuple 10 2) ]), ptr: 1, status: On })
                          , (Tuple 2 { au: (Gain' (AudioParameter { param: 0.4, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 1) ]), ptr: 2, status: On })
                          , (Tuple 3 { au: (SinOsc' (AudioParameter { param: 481.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 3 0) ]), ptr: 3, status: On })
                          , (Tuple 4 { au: (SinOsc' (AudioParameter { param: 962.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 4 0) ]), ptr: 4, status: On })
                          , (Tuple 5 { au: (Gain' (AudioParameter { param: 0.3, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 5 0), (Tuple 6 1), (Tuple 7 1), (Tuple 8 1), (Tuple 9 1), (Tuple 10 1) ]), ptr: 5, status: On })
                          , (Tuple 6 { au: (SinOsc' (AudioParameter { param: 413.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 6 0) ]), ptr: 6, status: On })
                          , (Tuple 7 { au: (SinOsc' (AudioParameter { param: 826.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 7 0) ]), ptr: 7, status: On })
                          , (Tuple 8 { au: (SinOsc' (AudioParameter { param: 1239.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 8 0) ]), ptr: 8, status: On })
                          , (Tuple 9 { au: (SinOsc' (AudioParameter { param: 1652.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 9 0) ]), ptr: 9, status: On })
                          , (Tuple 10 { au: (SinOsc' (AudioParameter { param: 2065.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 10 0) ]), ptr: 10, status: On })
                          ]
                      )
                  , grouped: (fromFoldable [ (Tuple { chan: 1, name: Nothing, tag: Add'' } (NonEmpty { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1), (Tuple 3 2), (Tuple 4 2), (Tuple 5 1), (Tuple 6 2), (Tuple 7 2), (Tuple 8 2), (Tuple 9 2), (Tuple 10 2) ]), ptr: 1, status: On } Nil)), (Tuple { chan: 1, name: Nothing, tag: Gain'' } (NonEmpty { au: (Gain' (AudioParameter { param: 0.4, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 1) ]), ptr: 2, status: On } ({ au: (Gain' (AudioParameter { param: 0.3, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 5 0), (Tuple 6 1), (Tuple 7 1), (Tuple 8 1), (Tuple 9 1), (Tuple 10 1) ]), ptr: 5, status: On } : Nil))), (Tuple { chan: 1, name: Nothing, tag: SinOsc'' } (NonEmpty { au: (SinOsc' (AudioParameter { param: 481.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 3 0) ]), ptr: 3, status: On } ({ au: (SinOsc' (AudioParameter { param: 962.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 4 0) ]), ptr: 4, status: On } : { au: (SinOsc' (AudioParameter { param: 413.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 6 0) ]), ptr: 6, status: On } : { au: (SinOsc' (AudioParameter { param: 826.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 7 0) ]), ptr: 7, status: On } : { au: (SinOsc' (AudioParameter { param: 1239.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 8 0) ]), ptr: 8, status: On } : { au: (SinOsc' (AudioParameter { param: 1652.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 9 0) ]), ptr: 9, status: On } : { au: (SinOsc' (AudioParameter { param: 2065.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 5 1) ]), prev: (fromFoldable [ (Tuple 10 0) ]), ptr: 10, status: On } : Nil))), (Tuple { chan: 1, name: Nothing, tag: Speaker'' } (NonEmpty { au: Speaker', chan: 1, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 3), (Tuple 5 2), (Tuple 6 3), (Tuple 7 3), (Tuple 8 3), (Tuple 9 3), (Tuple 10 3) ]), ptr: 0, status: On } Nil)) ])
                  }
              , instructionSet:
                  ( (DisconnectFrom 10 7)
                      : ( Shuffle
                            [ (Tuple 0 0)
                            , (Tuple 1 1)
                            , (Tuple 2 5)
                            , (Tuple 3 6)
                            , (Tuple 4 7)
                            , (Tuple 5 8)
                            , (Tuple 6 10)
                            , (Tuple 7 2)
                            , (Tuple 8 3)
                            , (Tuple 9 4)
                            , (Tuple 10 9)
                            ]
                        )
                      : (ConnectTo 9 5 Nothing)
                      : (SetGain 2 0.4 0.0)
                      : (SetFrequency 3 481.0 0.0)
                      : (SetFrequency 4 962.0 0.0)
                      : (SetGain 5 0.3 0.0)
                      : (SetFrequency 6 413.0 0.0)
                      : (SetFrequency 7 826.0 0.0)
                      : (SetFrequency 8 1239.0 0.0)
                      : (SetFrequency 9 1652.0 0.0)
                      : (SetFrequency 10 2065.0 0.0)
                      : Nil
                  )
              , prev: { flat: (fromFoldable [ (Tuple 0 { au: Speaker', chan: 1, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 3), (Tuple 7 2), (Tuple 8 3), (Tuple 9 3), (Tuple 10 3) ]), ptr: 0, status: On }), (Tuple 1 { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1), (Tuple 3 2), (Tuple 4 2), (Tuple 5 2), (Tuple 6 2), (Tuple 7 1), (Tuple 8 2), (Tuple 9 2), (Tuple 10 2) ]), ptr: 1, status: On }), (Tuple 2 { au: (Gain' (AudioParameter { param: 1.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1), (Tuple 6 1) ]), ptr: 2, status: On }), (Tuple 3 { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 3 0) ]), ptr: 3, status: On }), (Tuple 4 { au: (SinOsc' (AudioParameter { param: 880.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 4 0) ]), ptr: 4, status: On }), (Tuple 5 { au: (SinOsc' (AudioParameter { param: 1320.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 5 0) ]), ptr: 5, status: On }), (Tuple 6 { au: (SinOsc' (AudioParameter { param: 1760.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 6 0) ]), ptr: 6, status: On }), (Tuple 7 { au: (Gain' (AudioParameter { param: 0.9, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 7 0), (Tuple 8 1), (Tuple 9 1), (Tuple 10 1) ]), ptr: 7, status: On }), (Tuple 8 { au: (SinOsc' (AudioParameter { param: 442.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 7 1) ]), prev: (fromFoldable [ (Tuple 8 0) ]), ptr: 8, status: On }), (Tuple 9 { au: (SinOsc' (AudioParameter { param: 884.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 7 1) ]), prev: (fromFoldable [ (Tuple 9 0) ]), ptr: 9, status: On }), (Tuple 10 { au: (SinOsc' (AudioParameter { param: 1326.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 7 1) ]), prev: (fromFoldable [ (Tuple 10 0) ]), ptr: 10, status: On }) ]), grouped: (fromFoldable [ (Tuple { chan: 1, name: Nothing, tag: Add'' } (NonEmpty { au: Add', chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 1) ]), prev: (fromFoldable [ (Tuple 1 0), (Tuple 2 1), (Tuple 3 2), (Tuple 4 2), (Tuple 5 2), (Tuple 6 2), (Tuple 7 1), (Tuple 8 2), (Tuple 9 2), (Tuple 10 2) ]), ptr: 1, status: On } Nil)), (Tuple { chan: 1, name: Nothing, tag: Gain'' } (NonEmpty { au: (Gain' (AudioParameter { param: 1.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 2 0), (Tuple 3 1), (Tuple 4 1), (Tuple 5 1), (Tuple 6 1) ]), ptr: 2, status: On } ({ au: (Gain' (AudioParameter { param: 0.9, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 2), (Tuple 1 1) ]), prev: (fromFoldable [ (Tuple 7 0), (Tuple 8 1), (Tuple 9 1), (Tuple 10 1) ]), ptr: 7, status: On } : Nil))), (Tuple { chan: 1, name: Nothing, tag: SinOsc'' } (NonEmpty { au: (SinOsc' (AudioParameter { param: 440.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 3 0) ]), ptr: 3, status: On } ({ au: (SinOsc' (AudioParameter { param: 880.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 4 0) ]), ptr: 4, status: On } : { au: (SinOsc' (AudioParameter { param: 1320.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 5 0) ]), ptr: 5, status: On } : { au: (SinOsc' (AudioParameter { param: 1760.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 2 1) ]), prev: (fromFoldable [ (Tuple 6 0) ]), ptr: 6, status: On } : { au: (SinOsc' (AudioParameter { param: 442.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 7 1) ]), prev: (fromFoldable [ (Tuple 8 0) ]), ptr: 8, status: On } : { au: (SinOsc' (AudioParameter { param: 884.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 7 1) ]), prev: (fromFoldable [ (Tuple 9 0) ]), ptr: 9, status: On } : { au: (SinOsc' (AudioParameter { param: 1326.0, timeOffset: 0.0 })), chan: 1, name: Nothing, next: (fromFoldable [ (Tuple 0 3), (Tuple 1 2), (Tuple 7 1) ]), prev: (fromFoldable [ (Tuple 10 0) ]), ptr: 10, status: On } : Nil))), (Tuple { chan: 1, name: Nothing, tag: Speaker'' } (NonEmpty { au: Speaker', chan: 1, name: Nothing, next: (fromFoldable []), prev: (fromFoldable [ (Tuple 0 0), (Tuple 1 1), (Tuple 2 2), (Tuple 3 3), (Tuple 4 3), (Tuple 5 3), (Tuple 6 3), (Tuple 7 2), (Tuple 8 3), (Tuple 9 3), (Tuple 10 3) ]), ptr: 0, status: On } Nil)) ]) }
              , reconciliation:
                  ( fromFoldable
                      [ (Tuple 0 0) -- keep speaker
                      , (Tuple 1 1) -- keep add
                      , (Tuple 2 5) -- flip gain to side with more inputs
                      , (Tuple 3 6) -- 440L -> 413R
                      , (Tuple 4 7) -- 880L -> 826R
                      , (Tuple 5 8) -- 1320L -> 1239R
                      , (Tuple 6 10) -- 1760L -> 2065R
                      , (Tuple 7 2) -- flip gain to side with less inputs
                      , (Tuple 8 3) -- 442R -> 481L
                      , (Tuple 9 4) -- 884R -> 962L
                      , (Tuple 10 9) -- 1326R -> 1652R
                      ]
                  )
              }
