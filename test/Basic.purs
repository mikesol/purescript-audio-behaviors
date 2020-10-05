module Test.Basic where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (replicate, zipWith)
import Data.List (List(..), (:))
import Data.List as DL
import Data.Map as DM
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Set (fromFoldable)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (d3)
import Data.Vec as V
import Effect.Aff (Error)
import Effect.Class (class MonadEffect)
import FRP.Behavior.Audio (AudioParameter(..), AudioProcessor, AudioUnit'(..), SampleFrame, Status(..), audioToPtr, dup1, gain, gain', merger, microphone, sinOsc, speaker', split3)
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
basicTestSuite = do
  describe "complex split with merge" do
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
              ( DM.fromFoldable
                  [ ( Tuple 0
                        { au: Speaker'
                        , chan: 3
                        , head: 0
                        , name: Nothing
                        , next: (fromFoldable Nil)
                        , prev:
                            ( fromFoldable
                                (1 : Nil)
                            )
                        , ptr: 0
                        , status: On
                        }
                    )
                  , ( Tuple 1
                        { au: (Gain' $ AudioParameter { param: 0.5, timeOffset: 0.0 })
                        , chan: 3
                        , head: 1
                        , name: Nothing
                        , next:
                            ( fromFoldable
                                (0 : Nil)
                            )
                        , prev: (fromFoldable (2 : Nil))
                        , ptr: 1
                        , status: On
                        }
                    )
                  , ( Tuple 2
                        { au:
                            (Merger' (7 : 5 : 3 : Nil))
                        , chan: 3
                        , head: 2
                        , name: Nothing
                        , next: (fromFoldable (1 : Nil))
                        , prev: (fromFoldable (3 : 5 : 7 : Nil))
                        , ptr: 2
                        , status: On
                        }
                    )
                  , ( Tuple 3
                        { au: (Gain' $ AudioParameter { param: 0.3, timeOffset: 0.0 })
                        , chan: 1
                        , head: 3
                        , name: Nothing
                        , next:
                            ( fromFoldable
                                (2 : Nil)
                            )
                        , prev: (fromFoldable (4 : Nil))
                        , ptr: 3
                        , status: On
                        }
                    )
                  , ( Tuple 4
                        { au: (SplitRes' 0)
                        , chan: 1
                        , head: 4
                        , name: Nothing
                        , next:
                            ( fromFoldable
                                (3 : Nil)
                            )
                        , prev: (fromFoldable Nil)
                        , ptr: 4
                        , status: On
                        }
                    )
                  , ( Tuple 5
                        { au: (Gain' $ AudioParameter { param: 0.3, timeOffset: 0.0 })
                        , chan: 1
                        , head: 5
                        , name: Nothing
                        , next:
                            ( fromFoldable
                                (2 : Nil)
                            )
                        , prev: (fromFoldable (6 : Nil))
                        , ptr: 5
                        , status: On
                        }
                    )
                  , (Tuple 6 { au: (SplitRes' 1), chan: 1, head: 6, name: Nothing, next: (fromFoldable (5 : Nil)), prev: (fromFoldable Nil), ptr: 6, status: On })
                  , (Tuple 7 { au: (Gain' $ AudioParameter { param: 0.3, timeOffset: 0.0 }), chan: 1, head: 7, name: Nothing, next: (fromFoldable (2 : Nil)), prev: (fromFoldable (8 : Nil)), ptr: 7, status: On })
                  , ( Tuple 8
                        { au: (SplitRes' 2)
                        , chan: 1
                        , head: 8
                        , name: Nothing
                        , next: (fromFoldable (7 : Nil))
                        , prev:
                            (fromFoldable Nil)
                        , ptr: 8
                        , status: On
                        }
                    )
                  , ( Tuple 9
                        { au: (Splitter' 3)
                        , chan: 3
                        , head: 0
                        , name: Nothing
                        , next: (fromFoldable (4 : 6 : 8 : Nil))
                        , prev:
                            (fromFoldable (10 : Nil))
                        , ptr: 9
                        , status: On
                        }
                    )
                  , (Tuple 10 { au: (Merger' (29 : 20 : 11 : Nil)), chan: 3, head: 10, name: Nothing, next: (fromFoldable (9 : Nil)), prev: (fromFoldable (11 : 20 : 29 : Nil)), ptr: 10, status: On })
                  , (Tuple 11 { au: Add', chan: 1, head: 11, name: Nothing, next: (fromFoldable (10 : Nil)), prev: (fromFoldable (12 : 16 : Nil)), ptr: 11, status: On })
                  , (Tuple 12 { au: (Gain' $ AudioParameter { param: 1.0, timeOffset: 0.0 }), chan: 1, head: 12, name: Nothing, next: (fromFoldable (11 : Nil)), prev: (fromFoldable (13 : 14 : 15 : Nil)), ptr: 12, status: On })
                  , (Tuple 13 { au: (SinOsc' $ AudioParameter { param: 440.0, timeOffset: 0.0 }), chan: 1, head: 13, name: Nothing, next: (fromFoldable (12 : Nil)), prev: (fromFoldable Nil), ptr: 13, status: On })
                  , (Tuple 14 { au: (SinOsc' $ AudioParameter { param: 441.0, timeOffset: 0.0 }), chan: 1, head: 14, name: Nothing, next: (fromFoldable (12 : Nil)), prev: (fromFoldable Nil), ptr: 14, status: On })
                  , (Tuple 15 { au: (SinOsc' $ AudioParameter { param: 441.0, timeOffset: 0.0 }), chan: 1, head: 15, name: Nothing, next: (fromFoldable (12 : Nil)), prev: (fromFoldable Nil), ptr: 15, status: On })
                  , ( Tuple 16
                        { au:
                            ( Gain'
                                $ AudioParameter
                                    { param: 0.9
                                    , timeOffset: 0.0
                                    }
                            )
                        , chan: 1
                        , head: 16
                        , name: Nothing
                        , next: (fromFoldable (11 : Nil))
                        , prev: (fromFoldable (17 : 18 : 19 : Nil))
                        , ptr: 16
                        , status: On
                        }
                    )
                  , (Tuple 17 { au: (SinOsc' $ AudioParameter { param: 442.0, timeOffset: 0.0 }), chan: 1, head: 17, name: Nothing, next: (fromFoldable (16 : Nil)), prev: (fromFoldable Nil), ptr: 17, status: On })
                  , (Tuple 18 { au: (SinOsc' $ AudioParameter { param: 443.0, timeOffset: 0.0 }), chan: 1, head: 18, name: Nothing, next: (fromFoldable (16 : Nil)), prev: (fromFoldable Nil), ptr: 18, status: On })
                  , (Tuple 19 { au: (SinOsc' $ AudioParameter { param: 443.0, timeOffset: 0.0 }), chan: 1, head: 19, name: Nothing, next: (fromFoldable (16 : Nil)), prev: (fromFoldable Nil), ptr: 19, status: On })
                  , (Tuple 20 { au: Add', chan: 1, head: 20, name: Nothing, next: (fromFoldable (10 : Nil)), prev: (fromFoldable (21 : 25 : Nil)), ptr: 20, status: On })
                  , ( Tuple 21
                        { au: (Gain' $ AudioParameter { param: 1.0, timeOffset: 0.0 })
                        , chan: 1
                        , head: 21
                        , name: Nothing
                        , next: (fromFoldable (20 : Nil))
                        , prev: (fromFoldable (22 : 23 : 24 : Nil))
                        , ptr: 21
                        , status: On
                        }
                    )
                  , (Tuple 22 { au: (SinOsc' $ AudioParameter { param: 440.0, timeOffset: 0.0 }), chan: 1, head: 22, name: Nothing, next: (fromFoldable (21 : Nil)), prev: (fromFoldable Nil), ptr: 22, status: On })
                  , (Tuple 23 { au: (SinOsc' $ AudioParameter { param: 441.0, timeOffset: 0.0 }), chan: 1, head: 23, name: Nothing, next: (fromFoldable (21 : Nil)), prev: (fromFoldable Nil), ptr: 23, status: On })
                  , (Tuple 24 { au: (SinOsc' $ AudioParameter { param: 441.0, timeOffset: 0.0 }), chan: 1, head: 24, name: Nothing, next: (fromFoldable (21 : Nil)), prev: (fromFoldable Nil), ptr: 24, status: On })
                  , (Tuple 25 { au: (Gain' $ AudioParameter { param: 0.9, timeOffset: 0.0 }), chan: 1, head: 25, name: Nothing, next: (fromFoldable (20 : Nil)), prev: (fromFoldable (26 : 27 : 28 : Nil)), ptr: 25, status: On })
                  , (Tuple 26 { au: (SinOsc' $ AudioParameter { param: 442.0, timeOffset: 0.0 }), chan: 1, head: 26, name: Nothing, next: (fromFoldable (25 : Nil)), prev: (fromFoldable Nil), ptr: 26, status: On })
                  , ( Tuple 27
                        { au: (SinOsc' $ AudioParameter { param: 443.0, timeOffset: 0.0 })
                        , chan: 1
                        , head: 27
                        , name: Nothing
                        , next:
                            ( fromFoldable
                                (25 : Nil)
                            )
                        , prev: (fromFoldable Nil)
                        , ptr: 27
                        , status: On
                        }
                    )
                  , (Tuple 28 { au: (SinOsc' $ AudioParameter { param: 443.0, timeOffset: 0.0 }), chan: 1, head: 28, name: Nothing, next: (fromFoldable (25 : Nil)), prev: (fromFoldable Nil), ptr: 28, status: On })
                  , (Tuple 29 { au: Add', chan: 1, head: 29, name: Nothing, next: (fromFoldable (10 : Nil)), prev: (fromFoldable (30 : 34 : Nil)), ptr: 29, status: On })
                  , (Tuple 30 { au: (Gain' $ AudioParameter { param: 1.0, timeOffset: 0.0 }), chan: 1, head: 30, name: Nothing, next: (fromFoldable (29 : Nil)), prev: (fromFoldable (31 : 32 : 33 : Nil)), ptr: 30, status: On })
                  , ( Tuple 31
                        { au: (SinOsc' $ AudioParameter { param: 440.0, timeOffset: 0.0 })
                        , chan: 1
                        , head: 31
                        , name: Nothing
                        , next: (fromFoldable (30 : Nil))
                        , prev: (fromFoldable Nil)
                        , ptr: 31
                        , status: On
                        }
                    )
                  , (Tuple 32 { au: (SinOsc' $ AudioParameter { param: 441.0, timeOffset: 0.0 }), chan: 1, head: 32, name: Nothing, next: (fromFoldable (30 : Nil)), prev: (fromFoldable Nil), ptr: 32, status: On })
                  , (Tuple 33 { au: (SinOsc' $ AudioParameter { param: 441.0, timeOffset: 0.0 }), chan: 1, head: 33, name: Nothing, next: (fromFoldable (30 : Nil)), prev: (fromFoldable Nil), ptr: 33, status: On })
                  , (Tuple 34 { au: (Gain' $ AudioParameter { param: 0.9, timeOffset: 0.0 }), chan: 1, head: 34, name: Nothing, next: (fromFoldable (29 : Nil)), prev: (fromFoldable (35 : 36 : 37 : Nil)), ptr: 34, status: On })
                  , (Tuple 35 { au: (SinOsc' $ AudioParameter { param: 442.0, timeOffset: 0.0 }), chan: 1, head: 35, name: Nothing, next: (fromFoldable (34 : Nil)), prev: (fromFoldable Nil), ptr: 35, status: On })
                  , ( Tuple 36
                        { au: (SinOsc' $ AudioParameter { param: 443.0, timeOffset: 0.0 })
                        , chan: 1
                        , head: 36
                        , name: Nothing
                        , next: (fromFoldable (34 : Nil))
                        , prev: (fromFoldable Nil)
                        , ptr: 36
                        , status: On
                        }
                    )
                  , (Tuple 37 { au: (SinOsc' $ AudioParameter { param: 443.0, timeOffset: 0.0 }), chan: 1, head: 37, name: Nothing, next: (fromFoldable (34 : Nil)), prev: (fromFoldable Nil), ptr: 37, status: On })
                  ]
              )
          , len: 38
          , p: { au: (Splitter' 3), chan: 3, head: 0, name: Nothing, next: (fromFoldable (4 : 6 : 8 : Nil)), prev: (fromFoldable (10 : Nil)), ptr: 9, status: On }
          }
  describe "Audio tree" do
    it "should correctly split" do
      let
        tree =
          audioToPtr
            (speaker' $ dup1 microphone (\u -> (sinOsc 42.0 * u)))
      tree
        `shouldEqual`
          { flat:
              ( DM.fromFoldable
                  [ ( Tuple 0
                        { au: Speaker'
                        , head: 0
                        , chan: 1
                        , name: Nothing
                        , next: (fromFoldable Nil)
                        , prev:
                            ( fromFoldable
                                (1 : Nil)
                            )
                        , ptr: 0
                        , status: On
                        }
                    )
                  , ( Tuple 1
                        { au: Mul'
                        , head: 1
                        , chan: 1
                        , name: Nothing
                        , next: (fromFoldable (0 : Nil))
                        , prev:
                            ( fromFoldable
                                (2 : 3 : Nil)
                            )
                        , ptr: 1
                        , status: On
                        }
                    )
                  , ( Tuple 2
                        { au:
                            (SinOsc' $ AudioParameter { param: 42.0, timeOffset: 0.0 })
                        , head: 2
                        , chan: 1
                        , name: Nothing
                        , next: (fromFoldable (1 : Nil))
                        , prev: (fromFoldable Nil)
                        , ptr: 2
                        , status: On
                        }
                    )
                  , ( Tuple 3
                        { au: DupRes'
                        , head: 3
                        , chan: 1
                        , name: Nothing
                        , next: (fromFoldable (1 : Nil))
                        , prev:
                            (fromFoldable Nil)
                        , ptr: 3
                        , status: On
                        }
                    )
                  , ( Tuple 4
                        { au: Dup'
                        , head: 1
                        , chan: 1
                        , name: Nothing
                        , next: (fromFoldable (3 : Nil))
                        , prev: (fromFoldable (5 : Nil))
                        , ptr: 4
                        , status: On
                        }
                    )
                  , ( Tuple 5
                        { au: Microphone'
                        , head: 5
                        , chan: 1
                        , name: Nothing
                        , next: (fromFoldable (4 : Nil))
                        , prev: (fromFoldable Nil)
                        , ptr: 5
                        , status: On
                        }
                    )
                  ]
              )
          , len: 6
          , p: { head: 0, au: Speaker', chan: 1, name: Nothing, next: (fromFoldable Nil), prev: (fromFoldable (1 : Nil)), ptr: 0, status: On }
          }
