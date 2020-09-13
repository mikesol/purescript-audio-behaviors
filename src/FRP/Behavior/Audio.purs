module FRP.Behavior.Audio
  ( SampleFrame
  , AudioProcessor
  , soundify
  , AudioUnit
  , makeAudioWorkletProcessor
  , audioIO
  , audioIOInterleaved
  , IdxContext
  ) where

import Prelude
import Data.Array (foldl, head, index, length, mapWithIndex, range, replicate, snoc, zipWith, (!!))
import Data.Int (floor, toNumber)
import Data.Map as M
import Data.List (List(..), (:), singleton)
import Data.Maybe (fromMaybe)
import Data.NonEmpty as NE
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (sequence)
import Data.Typelevel.Num (class Nat, D1, D2, toInt')
import Data.Vec (Vec, fill)
import Effect (Effect, whileE)
import Effect.Ref (Ref, modify_, new, read, write)
import FRP.Behavior (Behavior, behavior, sample_)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Time (interval)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Record (merge)
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)

newtype IdxContext i
  = IdxContext i

instance deArr ::
  Mapping (IdxContext Int) (Array Number) Number where
  mapping (IdxContext i) = \a -> fromMaybe 0.0 $ (a !! if length a == 1 then 0 else i)

paramGetter ::
  forall (a :: # Type) (b :: # Type).
  Homogeneous a (Array Number) =>
  Homogeneous b Number =>
  HMap (IdxContext Int) (Record a) (Record b) =>
  Int ->
  { | a } ->
  { | b }
paramGetter i a = hmap (IdxContext i) a

type SampleFrame
  = Array (Array Number) -- inputs [ channels [ ] ]

type Audio
  = Array (Array (Array Number)) -- inputs [ channels [ samples [] ] ]

type AudioProcessor (r :: # Type)
  = Behavior Number -> -- current time
    ( Number -> -- offset from current time - a positive float. 0.0 = now, 0.58 = 0.58 seconds ago, etc
      Behavior SampleFrame -- a sample frame at that time
    ) ->
    Behavior (Record r) -> -- custom parameters at the current time
    Behavior SampleFrame -- a sample frame at the current time

type AudioSink
  = SampleFrame -> Effect Unit

writeToOutput ::
  Event Unit ->
  Behavior SampleFrame ->
  (SampleFrame -> Effect Unit) ->
  Effect (Effect Unit)
writeToOutput e scene render = subscribe (sample_ scene e) render

-- | Create an event which fires for every audio frame.
-- | Note that this does not have a canceler.  Cancellation from audio
-- | processing should be handled in the `process()` function of the worklet.
audioDriver :: Effect Boolean -> Event Unit
audioDriver incr =
  makeEvent \k -> do
    whileE incr (k unit)
    pure (pure unit)

withParam ::
  forall a r.
  Effect r ->
  Event a ->
  Event { value :: a, param :: r }
withParam incr e =
  makeEvent \k ->
    e
      `subscribe`
        \value -> do
          param <- incr
          k { value, param }

-- | A generic behavior from a parameter
_behavior :: forall r. Effect r -> Behavior r
_behavior eff = behavior \e -> map (\{ value, param } -> value param) (withParam eff e)

currentTime = _behavior :: Effect Number -> Behavior Number

sampleFrame = _behavior :: Effect SampleFrame -> Behavior SampleFrame

controlParams =
  _behavior ::
    forall (r :: # Type).
    Homogeneous r Number =>
    Effect (Record r) ->
    Behavior (Record r)

foreign import _makeAudioWorkletProcessor ::
  forall (r :: # Type).
  String -> -- name
  Number -> -- retention
  Record r -> -- defaults
  (Effect Boolean -> Event Unit) -> -- driver
  (Effect Number -> Behavior Number) -> -- currentTime
  (Effect SampleFrame -> Behavior SampleFrame) -> -- sampleFrame
  (Effect (Record r) -> Behavior (Record r)) -> -- control params
  AudioProcessor r -> -- audio processor
  ( Event Unit ->
    Behavior SampleFrame ->
    (SampleFrame -> Effect Unit) ->
    Effect (Effect Unit)
  ) -> -- animator
  Effect Unit

-- | Make an audio worklet processor
-- | - the name of the processor
-- | - the number of seconds worth of audio to retain
-- | - default parameters
-- | - a function accepting
-- |   - a `Behavior` that reports the current time
-- |   - a function that takes a positive number of seconds and returns a `Behavior` that reports audio that many seconds into the past. Reaching back beyond the retention window causes undefined behavior.
-- |   - a `Behavior` that reports all control parameters at the current time
-- |   and returns a behavior reporting on a single sample
makeAudioWorkletProcessor ::
  forall (param :: # Type).
  Homogeneous param Number =>
  String -> -- name
  Number -> -- seconds of audio to retain
  Record param -> -- default values
  AudioProcessor param -> -- the audio processor
  Effect Unit
makeAudioWorkletProcessor name retention defaults proc =
  _makeAudioWorkletProcessor
    name
    retention
    defaults
    audioDriver
    currentTime
    sampleFrame
    controlParams
    proc
    writeToOutput

audiol :: forall a. Array (Array (Array a)) -> Int
audiol a = length $ fromMaybe [] ((head a) >>= head)

-- | The inner audio loop
-- | Useful when an audio plugin is an external module and you need to
-- | import a function to do processing.  Accepts:
-- | - an audio processor
-- | - the sample rate
-- | - the length of the current input
-- | - the current parameters
-- | - the current inputs
-- | - the current sample rate
-- | - the current buffer
-- | returns the processed audio in an effect
audioIO ::
  forall (params :: # Type) (param :: # Type).
  Homogeneous params (Array Number) =>
  Homogeneous param Number =>
  HMap (IdxContext Int) (Record params) (Record param) =>
  AudioProcessor param ->
  Int ->
  Int ->
  Record params ->
  Int ->
  Audio ->
  Effect Audio
audioIO processor sampleRate inputl params currentSample sink = do
  curpos <- new 0
  output <- new [ [ [] ] ]
  let
    _driver =
      audioDriver do
        cp <- read curpos
        write (cp + 1) curpos
        pure $ cp < inputl
  let
    _behaviorCurrentTime =
      currentTime do
        cp <- read curpos
        let
          ct = cp + currentSample
        pure $ (toNumber ct) / (toNumber sampleRate)
  let
    _secondsToBehaviorSampleFrame = \lb ->
      sampleFrame do
        cp0 <- read curpos
        let
          lookback = if lb < 0.0 then 0.0 else lb
        let
          lookbackInSamples = floor (lookback * toNumber sampleRate)
        let
          cp = (audiol sink) - inputl - lookbackInSamples
        pure
          $ if (cp < 0) then
              replicate
                (length sink)
                (replicate (length (fromMaybe [] $ head sink)) 0.0)
            else
              map (map (fromMaybe 0.0 <<< flip index cp)) sink
  let
    _behaviorControlParams =
      controlParams
        $ do
            cp <- read curpos
            pure $ paramGetter cp params
  let
    chain =
      processor
        _behaviorCurrentTime
        _secondsToBehaviorSampleFrame
        _behaviorControlParams
  bam <-
    writeToOutput _driver chain \frame -> do
      modify_ (flip (zipWith (zipWith snoc)) frame) output
      pure unit
  bam
  read output

-- | The inner audio loop
-- | The same as audioIO, but it keeps the sink interleaved.
-- | For web audio, this avoids additional memory copying.
-- | Note that the outer array enclosing the samples is not used here.
audioIOInterleaved ::
  forall (params :: # Type) (param :: # Type).
  Homogeneous params (Array Number) =>
  Homogeneous param Number =>
  HMap (IdxContext Int) (Record params) (Record param) =>
  AudioProcessor param ->
  Int ->
  Record params ->
  Int ->
  Int ->
  Int ->
  Array Number ->
  Effect (Array Number)
audioIOInterleaved processor sampleRate params currentSample nChannels bufferLength sink = do
  curpos <- new 0
  output <- sequence (replicate (nChannels * bufferLength) $ new 0.0)
  let
    _driver =
      audioDriver do
        cp <- read curpos
        write (cp + 1) curpos
        pure $ cp < bufferLength
  let
    _behaviorCurrentTime =
      currentTime do
        cp <- read curpos
        let
          ct = cp + currentSample
        pure $ (toNumber ct) / (toNumber sampleRate)
  let
    _secondsToBehaviorSampleFrame = \lb ->
      sampleFrame do
        cp0 <- read curpos
        let -- how far we're looking back in seconds
          lookback = if lb < 0.0 then 0.0 else lb
        let -- how many samples we're looking back
          lookbackInSamples = (bufferLength - cp0) + floor (lookback * toNumber sampleRate)
        let -- number of blocs to rewind
          numberOfBlocsToSkip = lookbackInSamples / bufferLength
        let -- the offset from the end of the bloc we are reading
          offsetFromEndOfBloc = lookbackInSamples - (numberOfBlocsToSkip * bufferLength)
        let -- number of floats to read back to get to bloc
          floatsToSkipUntilDesiredBloc = numberOfBlocsToSkip * bufferLength * nChannels
        let -- number of floats to read back to get to channel (in stereo, L goes to blocLength, R to 0)
          floatsToSkipUntilDesiredChannel i = ((nChannels - 1 - i) * bufferLength)
        let
          sinkLen = length sink
        let
          frame =
            [ map
                ( \i ->
                    fromMaybe 0.0
                      ( sink
                          !! ( sinkLen - floatsToSkipUntilDesiredBloc
                                - (floatsToSkipUntilDesiredChannel i)
                                - offsetFromEndOfBloc
                            )
                      )
                )
                (range 0 (nChannels - 1))
            ]
        -- useful for debugging
        --        log
        --          $ "cp0 "
        --          <> show cp0
        --          <> " lb "
        --          <> show lb
        --          <> " lookback "
        --          <> show lookback
        --          <> " lookbackInSamples "
        --          <> show lookbackInSamples
        --          <> " numberOfBlocsToSkip "
        --          <> show numberOfBlocsToSkip
        --          <> " offsetFromEndOfBloc "
        --          <> show offsetFromEndOfBloc
        --          <> " floatsToSkipUntilDesiredBloc "
        --          <> show floatsToSkipUntilDesiredBloc
        --          <> " floatsToSkipUntilDesiredChannel0 "
        --          <> show (floatsToSkipUntilDesiredChannel 0)
        --          <> " floatsToSkipUntilDesiredChannel1 "
        --          <> show (floatsToSkipUntilDesiredChannel 1)
        --          <> " frame "
        --          <> show frame
        pure frame
  let
    _behaviorControlParams =
      controlParams
        $ do
            cp <- read curpos
            pure $ paramGetter cp params
  let
    chain =
      processor
        _behaviorCurrentTime
        _secondsToBehaviorSampleFrame
        _behaviorControlParams
  bam <-
    writeToOutput _driver chain \frame -> do
      cp <- read curpos
      void
        $ sequence
            ( head frame
                >>= \channels ->
                    pure
                      $ sequence
                          ( mapWithIndex
                              ( \i sample ->
                                  sequence
                                    ( output !! ((i * bufferLength) + cp)
                                        >>= \ref -> pure $ (write sample ref)
                                    )
                              )
                              channels
                          )
            )
      pure unit
  bam
  sequence $ map read output

type AudioInfo  -- change this
  = Array Number

type PtrInfo'
  = { ptr :: Int
    , iChan :: Int
    , oChan :: Int
    , next :: List Int
    }

type PtrInfo
  = { ptr :: Int
    , iChan :: Int
    , oChan :: Int
    , prev :: List Int
    , next :: List Int
    , au :: AudioUnit'
    }

data AudioUnit i o
  = Microphone
  | SinOsc Number
  | SquareOsc Number
  | Splitter (AudioUnit i i) (Vec i (AudioUnit D1 D1) -> AudioUnit o o)
  | Panner (AudioUnit i i)
  | Tagged String (AudioUnit i o)
  | Mul (NonEmpty List (AudioUnit i i))
  | Add (NonEmpty List (AudioUnit i i))
  | Merger (Vec o Int) (AudioUnit i i)
  | Constant Number
  | Delay Number (AudioUnit i i)
  | Gain Number (NonEmpty List (AudioUnit i i))
  | Speaker (NonEmpty List (AudioUnit i i))
  | NoSound
  | SplitRes

data AudioUnit'
  = Microphone'
  | SinOsc' Number
  | SquareOsc' Number
  | Splitter'
  | Panner'
  | Tagged' String
  | Mul'
  | Add'
  | Merger'
  | Constant' Number
  | Delay' Number
  | Gain' Number
  | Speaker'
  | NoSound'
  | SplitRes'

au' :: forall i o. AudioUnit i o -> AudioUnit'
au' Microphone = Microphone'

au' (SinOsc n) = (SinOsc' n)

au' (SquareOsc n) = (SquareOsc' n)

au' (Splitter _ _) = Splitter'

au' (Panner _) = Panner'

au' (Tagged s _) = Tagged' s

au' (Mul _) = Mul'

au' (Add _) = Add'

au' (Merger _ _) = Merger'

au' (Constant n) = Constant' n

au' (Delay n _) = Delay' n

au' (Gain n _) = Gain' n

au' (Speaker n) = Speaker'

au' NoSound = NoSound'

au' SplitRes = SplitRes'

type AlgStep
  = { len :: Int
    , flat :: M.Map Int PtrInfo
    , init :: List Int
    , p :: PtrInfo
    }

-------------------------------------------- au          ------    len ptr
a2i :: forall i o. Nat i => Nat o => AudioUnit i o -> Int
a2i a = toInt' (Proxy :: Proxy i)

a2o :: forall i o. Nat i => Nat o => AudioUnit i o -> Int
a2o a = toInt' (Proxy :: Proxy o)

audioToPtr :: forall i o. Nat i => Nat o => AudioUnit i o -> PtrInfo
audioToPtr = _.p <<< go 0 Nil
  where
  go :: forall a b. Nat a => Nat b => Int -> List Int -> AudioUnit a b -> AlgStep
  go i next au =
    go'
      { ptr: i + 1
      , next
      , iChan: a2i au
      , oChan: a2o au
      }
      au

  terminus :: forall a b. Nat a => Nat b => PtrInfo' -> AudioUnit a b -> AlgStep
  terminus ptr v =
    let
      p = merge ptr { prev: Nil, au: au' v }
    in
      { len: 1
      , init: singleton ptr.ptr
      , flat: M.singleton ptr.ptr p
      , p
      }

  passthrough ::
    forall a b c d.
    Nat a =>
    Nat b =>
    Nat c =>
    Nat d =>
    PtrInfo' ->
    AudioUnit a b ->
    AudioUnit c d ->
    AlgStep
  passthrough ptr v a =
    let
      r = go ptr.ptr (singleton ptr.ptr) a
    in
      let
        p =
          merge ptr
            { prev: singleton r.p.ptr
            , au: au' v
            }
      in
        { len: r.len + 1
        , init: r.init
        , p
        , flat: r.flat <> (M.singleton ptr.ptr p)
        }

  listthrough ::
    forall a b c d.
    Nat a =>
    Nat b =>
    Nat c =>
    Nat d =>
    PtrInfo' ->
    AudioUnit a b ->
    NonEmpty List (AudioUnit c d) ->
    AlgStep
  listthrough ptr v l =
    let
      r =
        foldl
          ( \b@(h :| tl) a ->
              ( go (h.p.ptr + h.len) (singleton ptr.ptr) a
              )
                :| (h : tl)
          )
          ( NE.singleton
              (go ptr.ptr (singleton ptr.ptr) $ NE.head l)
          )
          l
    in
      let
        p =
          merge ptr
            { prev: let (hd :| tl) = map _.p.ptr r in (hd : tl)
            , au: au' v
            }
      in
        { len: (foldl (+) 0 (map _.len r)) + 1
        , init: foldl (<>) Nil (map _.init r)
        , flat: (foldl (<>) M.empty (map _.flat r)) <> M.singleton ptr.ptr p
        , p
        }

  go' :: forall a b. Nat a => Nat b => PtrInfo' -> AudioUnit a b -> AlgStep
  go' ptr v@Microphone = terminus ptr v

  go' ptr v@(SinOsc n) = terminus ptr v

  go' ptr v@(SquareOsc n) = terminus ptr v

  go' ptr v@(Constant n) = terminus ptr v

  go' ptr v@NoSound = terminus ptr v

  go' ptr v@(Panner a) = passthrough ptr v a

  go' ptr v@(Tagged s a) = passthrough ptr v a

  go' ptr v@(Delay n a) = passthrough ptr v a

  go' ptr v@(Merger l a) = passthrough ptr v a

  go' ptr v@(Mul l) = listthrough ptr v l

  go' ptr v@(Add l) = listthrough ptr v l

  go' ptr v@(Gain n l) = listthrough ptr v l

  go' ptr v@(Speaker l) = listthrough ptr v l

  go' ptr v@(Splitter a f) =
    let
      -- determine the inner chain
      ic = f (fill (const SplitRes))
    in
      let
        -- run alg on the inner chain
        -- to get the inner result
        ir = go ptr.ptr ptr.next ic
      in
        let
          -- continuing down, we offset the pointer
          -- by the number of nodes in the graph
          -- and point to this one
          fr = go (ptr.ptr + ir.len) (singleton ptr.ptr) a
        in
          let
            p =
              merge ptr
                { prev: singleton fr.p.ptr
                -- the next nodes are the initial nodes from the inner
                -- result
                , next: ir.init
                , au: au' v
                }
          in
            { len: fr.len + ir.len + 1
            -- the initial nodes come from downstream
            , init: fr.init
            , flat: ir.flat <> fr.flat <> (M.singleton ptr.ptr p)
            , p
            }

  go' ptr SplitRes =
    { len: 0 -- not actually a node
    , init: ptr.next -- these are the true initial objects
    , p: merge ptr { prev: Nil, au: au' SplitRes }
    , flat: M.empty
    }

pt :: forall i o. Nat i => Nat o => AudioUnit i o -> AudioUnit o o
pt = unsafeCoerce

microphone :: AudioUnit D1 D1
microphone = Microphone

sinOsc :: Number -> AudioUnit D1 D1
sinOsc = SinOsc

squareOsc :: Number -> AudioUnit D1 D1
squareOsc = SquareOsc

splitter :: forall i o. Nat i => Nat o => AudioUnit i i -> (Vec i (AudioUnit D1 D1) -> AudioUnit o o) -> AudioUnit i o
splitter = Splitter

panner :: AudioUnit D2 D2 -> AudioUnit D2 D2
panner = Panner

tagged :: forall i o. Nat i => Nat o => String -> AudioUnit i o -> AudioUnit i o
tagged = Tagged

speaker :: forall i. Nat i => NonEmpty List (AudioUnit i i) -> AudioUnit i i
speaker = Speaker

mul :: forall i. Nat i => NonEmpty List (AudioUnit i i) -> AudioUnit i i
mul = Mul

add :: forall i. Nat i => NonEmpty List (AudioUnit i i) -> AudioUnit i i
add = Add

merger :: forall i o. Nat i => Nat o => Vec o Int -> AudioUnit i i -> AudioUnit o o
merger v = pt <<< (Merger v)

constant :: forall i. Nat i => Number -> AudioUnit i i
constant = Constant

delay :: forall i. Nat i => Number -> AudioUnit i i -> AudioUnit i i
delay = Delay

gain :: forall i. Nat i => Number -> NonEmpty List (AudioUnit i i) -> AudioUnit i i
gain = Gain

instance semiringAudioUnit :: Semiring (AudioUnit o o) where
  zero = Constant 0.0
  one = Constant 1.0
  add a b = Add (a :| (b : Nil))
  mul a b = Mul (a :| (b : Nil))

type AudioBehavior i o
  = Behavior (AudioUnit i o)

-- reconciles the previous graph with the current one
audioReconciliation :: forall i o i' o'. Ref (AudioUnit i o) -> (AudioUnit i' o') -> Effect Unit
audioReconciliation prev cur = pure unit

soundify ::
  forall i o.
  Int ->
  (AudioBehavior i o) ->
  Effect (Effect Unit)
soundify e scene = do
  u <- new NoSound
  subscribe (sample_ scene (interval e)) (audioReconciliation u)
