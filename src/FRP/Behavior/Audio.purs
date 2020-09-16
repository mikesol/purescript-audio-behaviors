module FRP.Behavior.Audio
  ( SampleFrame
  , AudioProcessor
  , soundify
  , AudioUnit
  , makeAudioWorkletProcessor
  , audioIO
  , audioIOInterleaved
  , Status(..)
  , AudioUnit''(..)
  , IdxContext
  , audioToPtr
  , AudioUnit'(..)
  , microphone
  , sinOsc
  , squareOsc
  , splitter
  , panner
  , mul
  , add
  , merger
  , constant
  , delay
  , gain
  , speaker
  , speaker'
  , gain'
  , audioGrouper
  , makeProgram
  ) where

import Prelude
import Control.Bind (bindFlipped)
import Data.Array (catMaybes, filter, foldl, groupBy, head, index, length, mapWithIndex, range, replicate, snoc, sortWith, takeEnd, zipWith, (!!))
import Data.Array as A
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (floor, toNumber)
import Data.Int.Parse (parseInt, toRadix)
import Data.List (List(..), fromFoldable, partition, (:))
import Data.List as DL
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NE
import Data.String (Pattern(..), split, take)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (class Pos, D1, D2, toInt')
import Data.Unfoldable1 as DU
import Data.Vec (Vec, fill)
import Data.Vec as V
import Effect (Effect, whileE)
import Effect.Exception (throw)
import Effect.Ref (Ref, modify_, new, read, write)
import FRP.Behavior (Behavior, behavior, sample_)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Time (interval)
import Foreign (Foreign)
import Foreign.Object (Object, filterWithKey)
import Foreign.Object as O
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Math as Math
import Record (merge)
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)

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
    , chan :: Int
    , status :: Status
    , next :: Map Int Int
    }

type PtrInfo
  = { ptr :: Int
    , chan :: Int
    , prev :: Map Int Int
    , next :: Map Int Int
    , au :: AudioUnit'
    , status :: Status
    , name :: MString
    }

type MString
  = Maybe String

data Status
  = On
  | Off

chopHack :: List ~> List
chopHack Nil = Nil

chopHack (h : t) = t

derive instance genericStatus :: Generic Status _

instance showStatus :: Show Status where
  show s = genericShow s

derive instance eqStatus :: Eq Status

data AudioUnit ch
  = Microphone MString
  | SinOsc MString Number
  | SquareOsc MString Number
  | Splitter MString (AudioUnit ch) (Vec ch (AudioUnit D1) -> AudioUnit ch)
  | Panner MString (AudioUnit ch)
  | Mul MString (NonEmpty List (AudioUnit ch))
  | Add MString (NonEmpty List (AudioUnit ch))
  | Merger MString (Vec ch (AudioUnit D1))
  | Constant MString Number
  | Delay MString Number (AudioUnit ch)
  | Gain MString Number (NonEmpty List (AudioUnit ch))
  | Speaker MString (NonEmpty List (AudioUnit ch))
  | NoSound MString
  | SplitRes

data AudioUnit'
  = Microphone'
  | SinOsc' Number
  | SquareOsc' Number
  | Splitter'
  | Panner'
  | Mul'
  | Add'
  | Swap'
  | Merger'
  | Constant' Number
  | Delay' Number
  | Gain' Number
  | Speaker'
  | NoSound'
  | SplitRes'

derive instance genericAudioUnit' :: Generic AudioUnit' _

instance showAudioUnit' :: Show AudioUnit' where
  show s = genericShow s

derive instance eqAudioUnit' :: Eq AudioUnit'

data AudioUnit''
  = Microphone''
  | SinOsc''
  | SquareOsc''
  | Splitter''
  | Panner''
  | Mul''
  | Add''
  | Swap''
  | Merger''
  | Constant''
  | Delay''
  | Gain''
  | Speaker''
  | NoSound''
  | SplitRes''

derive instance genericAudioUnit'' :: Generic AudioUnit'' _

instance showAudioUnit'' :: Show AudioUnit'' where
  show s = genericShow s

derive instance eqAudioUnit'' :: Eq AudioUnit''

instance ordAudioUnit'' :: Ord AudioUnit'' where
  compare a b = compare (show a) (show b)

au' :: forall ch. AudioUnit ch -> { au :: AudioUnit', name :: MString }
au' (Microphone name) = { au: Microphone', name }

au' (SinOsc name n) = { au: (SinOsc' n), name }

au' (SquareOsc name n) = { au: (SquareOsc' n), name }

au' (Splitter name _ _) = { au: Splitter', name }

au' (Panner name _) = { au: Panner', name }

au' (Mul name _) = { au: Mul', name }

au' (Add name _) = { au: Add', name }

au' (Merger name _) = { au: Merger', name }

au' (Constant name n) = { au: (Constant' n), name }

au' (Delay name n _) = { au: (Delay' n), name }

au' (Gain name n _) = { au: (Gain' n), name }

au' (Speaker name _) = { au: Speaker', name }

au' (NoSound name) = { au: NoSound', name }

au' SplitRes = { au: SplitRes', name: Nothing }

au'' :: AudioUnit' -> AudioUnit''
au'' Microphone' = Microphone''

au'' (SinOsc' _) = SinOsc''

au'' (SquareOsc' _) = SquareOsc''

au'' Splitter' = Splitter''

au'' Panner' = Panner''

au'' Mul' = Mul''

au'' Add' = Add''

au'' Swap' = Swap''

au'' Merger' = Merger''

au'' (Constant' _) = Constant''

au'' (Delay' _) = Delay''

au'' (Gain' _) = Gain''

au'' Speaker' = Speaker''

au'' NoSound' = NoSound''

au'' SplitRes' = SplitRes''

tagToAU :: AudioUnit'' -> AudioUnit'
tagToAU Microphone'' = Microphone'

tagToAU SinOsc'' = SinOsc' 50000.0

tagToAU SquareOsc'' = SquareOsc' 50000.0

tagToAU Splitter'' = Splitter'

tagToAU Panner'' = Panner'

tagToAU Mul'' = Mul'

tagToAU Add'' = Add'

tagToAU Swap'' = Swap'

tagToAU Merger'' = Merger'

tagToAU Constant'' = Constant' 1000.0

tagToAU Delay'' = Delay' 1000.0

tagToAU Gain'' = Gain' 1000.0

tagToAU Speaker'' = Speaker'

tagToAU NoSound'' = NoSound'

tagToAU SplitRes'' = SplitRes'

trivialConstraint :: AudioUnit'' -> Boolean
trivialConstraint Microphone'' = true

trivialConstraint SinOsc'' = false

trivialConstraint SquareOsc'' = false

trivialConstraint Splitter'' = true

trivialConstraint Panner'' = false

trivialConstraint Mul'' = true

trivialConstraint Add'' = true

trivialConstraint Swap'' = true

trivialConstraint Merger'' = true

trivialConstraint Constant'' = false

trivialConstraint Delay'' = false

trivialConstraint Gain'' = false

trivialConstraint Speaker'' = true

trivialConstraint NoSound'' = true

trivialConstraint SplitRes'' = true

type UnfoldedFlatAudio
  = Tuple Int PtrInfo

type FlatAudio
  = M.Map Int PtrInfo

type AlgStep
  = { len :: Int
    , flat :: FlatAudio
    , p :: PtrInfo
    }

a2c :: forall ch. Pos ch => AudioUnit ch -> Int
a2c a = toInt' (Proxy :: Proxy ch)

audioToPtr ::
  forall channels.
  Pos channels =>
  AudioUnit channels -> AlgStep
audioToPtr = go (-1) M.empty
  where
  go :: forall ch. Pos ch => Int -> Map Int Int -> AudioUnit ch -> AlgStep
  go i next au =
    go'
      { ptr: i + 1
      , next
      , status: On
      , chan: a2c au
      }
      au

  terminus :: forall ch. Pos ch => PtrInfo' -> AudioUnit ch -> AlgStep
  terminus ptr v =
    let
      au = au' v
    in
      let
        p = merge { next: map (_ + 1) ptr.next, prev: M.singleton ptr.ptr 0, au: au.au, name: au.name } ptr
      in
        { len: 1
        , flat: M.singleton ptr.ptr p
        , p
        }

  passthrough ::
    forall ch.
    Pos ch =>
    PtrInfo' ->
    AudioUnit ch ->
    AudioUnit ch ->
    AlgStep
  passthrough ptr v a =
    let
      nxt = map (_ + 1) ptr.next
    in
      let
        r = go ptr.ptr (nxt <> (M.singleton ptr.ptr 0)) a
      in
        let
          au = au' v
        in
          let
            p =
              merge
                { next: nxt
                , prev: M.singleton ptr.ptr 0 <> map (_ + 1) r.p.prev
                , au: au.au
                , name: au.name
                }
                ptr
          in
            { len: r.len + 1
            , p
            , flat: r.flat <> (M.singleton ptr.ptr p)
            }

  listthrough ::
    forall ch ich.
    Pos ch =>
    Pos ich =>
    PtrInfo' ->
    AudioUnit ch ->
    NonEmpty List (AudioUnit ich) ->
    AlgStep
  listthrough ptr v l =
    let
      nxt = map (_ + 1) ptr.next
    in
      let
        nxtM = (nxt <> (M.singleton ptr.ptr 0))
      in
        let
          r =
            foldl
              ( \b@(h :| tl) a ->
                  ( go (h.p.ptr + h.len - 1) nxtM a
                  )
                    :| (h : tl)
              )
              ( NE.singleton
                  (go ptr.ptr nxtM $ NE.head l)
              )
              (NE.tail l)
        in
          let
            au = au' v
          in
            let
              p =
                merge
                  { next: nxt
                  , prev:
                      ( let
                          (hd :| tl) =
                            map
                              (map (_ + 1) <<< _.p.prev)
                              r
                        in
                          (foldl M.union hd tl)
                      )
                        <> M.singleton ptr.ptr 0
                  , au: au.au
                  , name: au.name
                  }
                  ptr
            in
              { len: (foldl (+) 0 (map _.len r)) + 1
              , flat: (foldl (<>) M.empty (map _.flat r)) <> M.singleton ptr.ptr p
              , p
              }

  go' :: forall ch. Pos ch => PtrInfo' -> AudioUnit ch -> AlgStep
  go' ptr v@(Microphone name) = terminus ptr v

  go' ptr v@(SinOsc name n) = terminus ptr v

  go' ptr v@(SquareOsc name n) = terminus ptr v

  go' ptr v@(Constant name n) = terminus ptr v

  go' ptr v@(NoSound name) = terminus ptr v

  go' ptr v@(Panner name a) = passthrough ptr v a

  go' ptr v@(Delay name n a) = passthrough ptr v a

  go' ptr v@(Mul name l) = listthrough ptr v l

  go' ptr v@(Merger name l) = listthrough ptr v (V.head l :| ((chopHack <<< fromFoldable <<< V.toArray) l))

  go' ptr v@(Add name l) = listthrough ptr v l

  go' ptr v@(Gain name n l) = listthrough ptr v l

  go' ptr v@(Speaker name l) = listthrough ptr v l

  go' ptr v@(Splitter name a f) =
    let
      -- determine the inner chain
      ic = f (fill (const SplitRes))
    in
      let
        -- run alg on the inner chain
        -- to get the inner result
        ----------------------------------------------------------
        ------------------------------------------------
        ------------------------------
        -----------
        -----
        -- go needs to have the result of this appended
        ir = go (ptr.ptr - 1) ptr.next ic
      in
        let
          maxPrev = foldl max 0 ir.p.prev
        in
          let
            -- continuing down, we offset the pointer
            -- by the number of nodes in the graph
            -- and point to this one
            -----------------------------------------------------------------
            ---------------------------------------------------------
            ----------------------------------------------
            ------------------------------------
            -------------------------
            -------------
            fr =
              go
                (ptr.ptr + ir.len)
                ((map (\i -> maxPrev + 1 - i) ir.p.prev) <> (M.singleton (ptr.ptr + ir.len) 0))
                a
          in
            let
              au = au' v
            in
              let
                -- problem: if fr is SplitRes
                -- then prev is nothing
                -- which leads to subPrev being underfull
                -- as it just points to itself
                -- for now, don't deal with this
                subPrev = M.singleton (ptr.ptr + ir.len) 0 <> map (_ + 1) fr.p.prev
              in
                let
                  p =
                    merge
                      { ptr: ptr.ptr + ir.len
                      , prev: subPrev
                      ----- need to pivot this to work backwards
                      , next: map (\i -> maxPrev - i + 1) ir.p.prev
                      , au: au.au
                      , name: au.name
                      }
                      ptr
                in
                  { len: fr.len + ir.len + 1
                  -- correct flat structure
                  , flat:
                      map
                        (\rec -> rec { prev = M.union (map (_ + maxPrev + 1) subPrev) rec.prev })
                        ir.flat
                        <> fr.flat
                        <> (M.singleton (ptr.ptr + ir.len) p)
                  , p
                  }

  go' ptr SplitRes =
    { len: 0 -- not actually a node
    , p: merge ptr { prev: M.empty :: Map Int Int, au: SplitRes', name: Nothing }
    , flat: M.empty
    }

microphone :: AudioUnit D1
microphone = Microphone Nothing

sinOsc :: Number -> AudioUnit D1
sinOsc = SinOsc Nothing

squareOsc :: Number -> AudioUnit D1
squareOsc = SquareOsc Nothing

splitter :: forall ch. Pos ch => AudioUnit ch -> (Vec ch (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
splitter SplitRes f = f (fill (const SplitRes))

splitter x f = Splitter Nothing x f

panner :: AudioUnit D2 -> AudioUnit D2
panner = Panner Nothing

speaker :: forall ch. Pos ch => NonEmpty List (AudioUnit ch) -> AudioUnit ch
speaker = Speaker Nothing

speaker' :: forall ch. Pos ch => AudioUnit ch -> AudioUnit ch
speaker' = Speaker Nothing <<< NE.singleton

merger ::
  forall ch.
  Pos ch =>
  Vec ch (AudioUnit D1) ->
  AudioUnit ch
merger = Merger Nothing

mul :: forall ch. Pos ch => NonEmpty List (AudioUnit ch) -> AudioUnit ch
mul = Mul Nothing

add :: forall ch. Pos ch => NonEmpty List (AudioUnit ch) -> AudioUnit ch
add = Add Nothing

constant :: forall ch. Pos ch => Number -> AudioUnit ch
constant = Constant Nothing

delay :: forall ch. Pos ch => Number -> AudioUnit ch -> AudioUnit ch
delay = Delay Nothing

gain :: forall ch. Pos ch => Number -> NonEmpty List (AudioUnit ch) -> AudioUnit ch
gain = Gain Nothing

gain' :: forall ch. Pos ch => Number -> AudioUnit ch -> AudioUnit ch
gain' n = Gain Nothing n <<< NE.singleton

instance semiringAudioUnit :: Semiring (AudioUnit ch) where
  zero = Constant Nothing 0.0
  one = Constant Nothing 1.0
  add a b = Add Nothing (a :| (b : Nil))
  mul a b = Mul Nothing (a :| (b : Nil))

type AudioBehavior ch
  = Behavior (AudioUnit ch)

type Reconcilable
  = { grouped :: GroupedAudio
    , flat :: FlatAudio
    }

ucomp :: AudioUnit' -> AudioUnit' -> Boolean
ucomp Microphone' Microphone' = true

ucomp (SinOsc' _) (SinOsc' _) = true

ucomp (SquareOsc' _) (SquareOsc' _) = true

ucomp Splitter' Splitter' = true

ucomp Panner' Panner' = true

ucomp Mul' Mul' = true

ucomp Add' Add' = true

ucomp Swap' Swap' = true

ucomp Merger' Merger' = true

ucomp (Constant' _) (Constant' _) = true

ucomp (Delay' _) (Delay' _) = true

ucomp (Gain' _) (Gain' _) = true

ucomp Speaker' Speaker' = true

ucomp NoSound' NoSound' = true

ucomp SplitRes' SplitRes' = true

ucomp _ _ = false

oscMULT = 1.0 / 22100.0 :: Number

gainMULT = 1.0 :: Number

constMULT = 1.0 :: Number

delayMULT = 0.1 :: Number

toCoef :: AudioUnit' -> AudioUnit' -> Number
toCoef Microphone' Microphone' = 0.0

toCoef (SinOsc' f0) (SinOsc' f1) = (Math.abs $ f0 - f1) * oscMULT

toCoef (SquareOsc' f0) (SquareOsc' f1) = (Math.abs $ f0 - f1) * oscMULT

toCoef Splitter' Splitter' = 0.0

toCoef Panner' Panner' = 0.0

toCoef Mul' Mul' = 0.0

toCoef Add' Add' = 0.0

toCoef Swap' Swap' = 0.0

toCoef Merger' Merger' = 0.0

toCoef (Constant' c0) (Constant' c1) = (Math.abs $ c0 - c1) * constMULT

toCoef (Delay' d0) (Delay' d1) = (Math.abs $ d0 - d1) * delayMULT

toCoef (Gain' g0) (Gain' g1) = (Math.abs $ g0 - g1) * gainMULT

toCoef Speaker' Speaker' = 0.0

toCoef NoSound' NoSound' = 0.0

toCoef SplitRes' SplitRes' = 0.0

toCoef _ _ = 0.0

acomp :: PtrInfo -> PtrInfo -> Boolean
acomp a b = ucomp a.au b.au && a.name == b.name && a.chan == b.chan

type AudioTag
  = { tag :: AudioUnit'', chan :: Int, name :: MString }

type UnfoldedGroupedAudio
  = Tuple AudioTag (NonEmpty List PtrInfo)

type GroupedAudio
  = Map AudioTag (NonEmpty List PtrInfo)

audioGrouper ::
  List PtrInfo ->
  GroupedAudio
audioGrouper Nil = M.empty

audioGrouper (h : t) =
  let
    pt = partition (acomp h) t
  in
    ( ( M.singleton
          { tag: au'' h.au, chan: h.chan, name: h.name
          }
          (h :| pt.yes)
      )
        <> audioGrouper pt.no
    )

makeContiguousUnits :: AudioTag -> Int -> Int -> List PtrInfo
makeContiguousUnits _ _ 0 = Nil

makeContiguousUnits t start n =
  map
    ( \i ->
        { au: tagToAU t.tag
        -- name should always be nothing
        , name: t.name
        , status: Off
        , next: M.empty
        , chan: t.chan
        , prev: M.empty
        , ptr: i
        }
    )
    (DU.range start (start + n - 1))

maybeNel :: forall a. List a -> Maybe (NonEmpty List a)
maybeNel Nil = Nothing

maybeNel (h : t) = Just $ h :| t

nonEmptyListLength :: forall a. NonEmpty List a -> Int
nonEmptyListLength (_ :| t) = DL.length t + 1

addContiguousNewUnits :: UnfoldedGroupedAudio -> Reconcilable -> Reconcilable
addContiguousNewUnits ug toModify =
  let
    nu =
      makeContiguousUnits
        (fst ug)
        (M.size toModify.flat)
        (max 0 (nonEmptyListLength (snd ug) - (maybe 0 nonEmptyListLength $ M.lookup (fst ug) toModify.grouped)))
  in
    ( { grouped:
          maybe
            toModify.grouped
            (\nel -> M.insertWith (\(h0 :| t0) (h1 :| t1) -> h0 :| (t0 <> (h1 : t1))) (fst ug) nel toModify.grouped)
            $ maybeNel nu
      , flat:
          toModify.flat <> (M.fromFoldable (map (\i -> Tuple i.ptr i) nu))
      }
    )

normalizeReconcilable :: Reconcilable -> Reconcilable -> Reconcilable
normalizeReconcilable target tom = go (M.toUnfoldable target.grouped) tom
  where
  go :: List UnfoldedGroupedAudio -> Reconcilable -> Reconcilable
  go Nil toModify = toModify

  go (h : t) toModify = let tm = addContiguousNewUnits h toModify in go t tm

data LPNode
  = LPNode AudioUnit'' Int Int

data LPEdge
  = LPEdge AudioUnit'' Int Int AudioUnit'' Int Int

type LPVar
  = { name :: String, coef :: Number }

type LPNodeVar
  = { node :: LPNode, coef :: Number }

type LPEdgeVar
  = { edge :: LPEdge, coef :: Number }

type LPObjective
  = { direction :: Int
    , name :: String
    , vars :: Array LPVar
    }

getNodeIdx :: Boolean -> LPNode -> Int
getNodeIdx true (LPNode _ i _) = i

getNodeIdx false (LPNode _ _ i) = i

getEdgeIdx :: Boolean -> LPEdge -> Tuple Int Int
getEdgeIdx true (LPEdge _ i _ _ i' _) = Tuple i i'

getEdgeIdx false (LPEdge _ _ i _ _ i') = Tuple i i'

makeLPNodeConstraints :: Boolean -> Array LPNodeVar -> Array LPConstraint
makeLPNodeConstraints cptr a =
  mapWithIndex
    ( \i x ->
        { name: "nodec_" <> show i
        , vars: toArray $ map (\{ node } -> { name: nodeToString node, coef: 1.0 }) x
        , bnds: { type: 5, ub: 1.0, lb: 1.0 }
        }
    )
    $ groupBy
        (\{ node: node0 } { node: node1 } -> getNodeIdx cptr node0 == getNodeIdx cptr node1)
        (sortWith (\{ node } -> getNodeIdx cptr node) a)

makeLPEdgeConstraints :: Boolean -> Array LPEdgeVar -> Array LPConstraint
makeLPEdgeConstraints cptr a =
  mapWithIndex
    ( \i x ->
        { name: "edgec_" <> show i
        , vars:
            toArray
              $ map
                  ( \{ edge } ->
                      { name: edgeToString edge
                      , coef: 1.0
                      }
                  )
                  x
        , bnds: { type: 5, ub: 1.0, lb: 1.0 }
        }
    )
    $ groupBy
        ( \{ edge: edge1 } { edge: edge2 } ->
            getEdgeIdx cptr edge1 == getEdgeIdx cptr edge2
        )
        (sortWith (\{ edge } -> getEdgeIdx cptr edge) a)

edgeToString :: LPEdge -> String
edgeToString (LPEdge tag b c d e f) =
  "e@" <> show tag <> "_"
    <> show b
    <> "_"
    <> show c
    <> "__"
    <> show d
    <> "_"
    <> show e
    <> "_"
    <> show f

nodeToString :: LPNode -> String
nodeToString (LPNode tag b c) =
  "n@" <> show tag <> "_"
    <> show b
    <> "_"
    <> show c

makeLPNodeEdgeConstraints :: Array LPEdgeVar -> Array LPConstraint
makeLPNodeEdgeConstraints a =
  mapWithIndex
    ( \i { edge: edge@(LPEdge tag0 a0 b0 tag1 a1 b1) } ->
        { name: "edgec_" <> show i
        , vars:
            [ { name: edgeToString edge
              , coef: 1.0
              }
            , { name: nodeToString (LPNode tag0 a0 b0)
              , coef: -0.75
              }
            , { name: nodeToString (LPNode tag1 a1 b1)
              , coef: -0.75
              }
            ]
        , bnds: { type: 3, ub: 0.0, lb: 0.0 }
        }
    )
    a

type LPBound
  = { type :: Int, ub :: Number, lb :: Number }

type LPConstraint
  = { name :: String
    , vars :: Array LPVar
    , bnds :: { type :: Int, ub :: Number, lb :: Number }
    }

type LinearProgram
  = { name :: String
    , objective :: LPObjective
    , subjectTo :: Array LPConstraint
    , binaries :: Array String
    }

nextDegree :: Map Int Int -> Array Int
nextDegree m = (A.fromFoldable <<< M.keys) $ M.filter (_ == 1) m

-- penalize atemporal relationships
nextCoef :: Map Int Int -> Int -> Number
nextCoef m i = maybe 1000.0 ((_ * 1.0) <<< (_ - 1.0) <<< toNumber) (M.lookup i m)

glpMIN = 1 :: Int

nodeVars :: Reconcilable -> Reconcilable -> Array LPNodeVar
nodeVars r0 r1 =
  join
    ( map
        ( \(Tuple k v0) ->
            ( maybe []
                ( \v1 ->
                    join
                      ( map
                          ( \i ->
                              map
                                ( \o ->
                                    { node: LPNode k.tag i.ptr o.ptr
                                    , coef: toCoef i.au o.au
                                    }
                                )
                                $ A.fromFoldable v1
                          )
                          $ A.fromFoldable v0
                      )
                )
                $ M.lookup k r1.grouped
            )
        )
        $ M.toUnfoldable r0.grouped
    )

edgeVars :: Reconcilable -> Reconcilable -> Array LPEdgeVar
edgeVars r0 r1 =
  join
    ( map
        ( \(Tuple k v0) ->
            ( maybe []
                ( \v1 ->
                    (join <<< join <<< join)
                      ( map
                          ( \i ->
                              map
                                ( \o ->
                                    map
                                      ( \n ->
                                          map
                                            ( \m ->
                                                { edge:
                                                    LPEdge k.tag
                                                      i.ptr
                                                      o.ptr
                                                      (au'' m.au)
                                                      n
                                                      m.ptr
                                                , coef: nextCoef o.next m.ptr
                                                }
                                            )
                                            ( maybe
                                                []
                                                (\res -> A.fromFoldable res)
                                                $ ( M.lookup n r0.flat
                                                      >>= \lk ->
                                                          M.lookup
                                                            { tag: au'' lk.au
                                                            , chan: lk.chan
                                                            , name: lk.name
                                                            }
                                                            r1.grouped
                                                  )
                                            )
                                      )
                                      ( maybe
                                          []
                                          (\aud -> nextDegree aud.next)
                                          $ M.lookup i.ptr r0.flat
                                      )
                                )
                                $ A.fromFoldable v1
                          )
                          $ A.fromFoldable v0
                      )
                )
                $ M.lookup k r1.grouped
            )
        )
        $ M.toUnfoldable r0.grouped
    )

-- for now, there are no edge constraints
-- finding edges requires numerous graph traversals in the target,
-- and the complexity gets out of hand quickly
-- we can add as an experiment later
audioToLP :: Reconcilable -> Reconcilable -> LinearProgram
audioToLP r0 r1 =
  let
    nv = nodeVars r0 r1
  in
    let
      ev = edgeVars r0 r1
    in
      let
        vars =
          (map (\{ node, coef } -> { name: nodeToString node, coef }) nv)
            <> (map (\{ edge, coef } -> { name: edgeToString edge, coef }) ev)
      in
        { name: "LP"
        , objective:
            { direction: glpMIN
            , name: "obj"
            , vars: filter (\{ coef } -> coef /= 0.0) vars
            }
        , binaries: map (\{ name } -> name) vars
        -- we do not construct an edge constraint based on the target edges
        -- not quite sure why, but doing so leads to infeasibility
        -- whereas levaing it open leads to a solution... 🤷‍♂️
        , subjectTo:
            makeLPNodeConstraints true nv
              <> makeLPNodeConstraints false nv
              <> makeLPEdgeConstraints true ev
              <> makeLPNodeEdgeConstraints ev
        }

makeProgram :: Reconcilable -> Reconcilable -> LinearProgram
makeProgram prev cur =
  let
    cur_ = normalizeReconcilable prev cur
  in
    let
      prev_ = normalizeReconcilable cur prev
    in
      audioToLP prev_ cur_

foreign import _glpk ::
  Foreign ->
  LinearProgram ->
  Either String (Object Int) ->
  (Object Int -> Either String (Object Int)) ->
  Either String (Object Int)

glpk :: Foreign -> LinearProgram -> Either String (Object Int)
glpk g lp = _glpk g lp (Left "Could not complete linear program") Right

toTuple :: Array Int -> Maybe (Tuple Int Int)
toTuple a = do
  l <- a !! 0
  r <- a !! 1
  pure $ Tuple l r

objectToMapping :: Object Int -> Map Int Int
objectToMapping o =
  M.fromFoldable
    $ catMaybes
        ( map
            ( bindFlipped toTuple
                <<< sequence
                <<< map (flip parseInt (toRadix 10))
                <<< takeEnd 2
                <<< split (Pattern "_")
            )
            $ O.keys (filterWithKey (\k v -> take 2 k == "n@" && v == 1) o)
        )

touchAudioUnits :: Foreign -> Ref (Map Int Foreign) -> Reconcilable -> Reconcilable -> Map Int Int -> Effect Unit
touchAudioUnits gtx raw prev cur objMap = pure unit

-- reconciles the previous graph with the current one
audioReconciliation :: Foreign -> Foreign -> Ref (Map Int Foreign) -> Ref Reconcilable -> Reconcilable -> Effect Unit
audioReconciliation g ctx raw prev' cur = do
  prev <- read prev'
  let
    prog = makeProgram prev cur
  either throw (touchAudioUnits ctx raw prev cur <<< objectToMapping) $ glpk g prog

-- | The main sound loop
-- | - glpk: The linear solver
-- | - ctx: The audio context
-- | - milli: The number of milliseconds between render cycles. Try 150...
-- | - scene: The sound
soundify ::
  forall ch.
  Pos ch =>
  Foreign ->
  Foreign ->
  Int ->
  AudioBehavior ch ->
  Effect (Effect Unit)
soundify g ctx e scene = do
  raw <- new (M.empty :: Map Int Foreign)
  u <-
    new
      { grouped: M.empty
      , flat: M.empty
      }
  subscribe (sample_ scene (interval e))
    ( audioReconciliation g ctx raw u
        <<< (\i -> { flat: i.flat, grouped: audioGrouper (fromFoldable i.flat) })
        <<< audioToPtr
    )
