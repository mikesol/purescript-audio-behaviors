module FRP.Behavior.Audio
  ( SampleFrame
  , AudioProcessor
  , AudioParameter(..)
  , AudioBuffer
  , AudioUnit
  , AudioContext
  , CanvasInfo
  , AudioInfo
  , VisualInfo
  , BrowserAudioTrack
  , BrowserAudioBuffer
  , BrowserFloatArray
  , AV(..)
  , Animation(..)
  , IAudioUnit(..)
  , IAnimation(..)
  , class RunnableMedia
  , reconciliationToInstructionSet
  , touchAudio
  , objectToMapping
  , LinearProgram
  , runInBrowser
  , runInBrowser_
  , audioBuffer
  , Oversample
  , LPObjective
  , LPConstraint
  , LPVar
  , makeAudioContext
  , makeFloatArray
  , makeAudioBuffer
  , makeAudioTrack
  , makeAudioWorkletProcessor
  , audioIO
  , Instruction(..)
  , Reconcilable
  , GroupedAudio
  , FlatAudio
  , AudioTag
  , PtrInfo
  , MString
  , Reconciled
  , audioIOInterleaved
  , Status(..)
  , AudioUnit''(..)
  , IdxContext
  , audioToPtr
  , AudioUnit'(..)
  , microphone
  , play
  , playBuf
  , loopBuf
  , playDynamicBuf
  , loopDynamicBuf
  , lowpass
  , highpass
  , bandpass
  , lowshelf
  , highshelf
  , peaking
  , notch
  , allpass
  , convolver
  , dynamicConvolver
  , dynamicsCompressor
  , dup1
  , dup2
  , dup3
  , dup4
  , dup5
  , waveShaper
  , dynamicWaveShaper
  , periodicOsc
  , dynamicPeriodicOsc
  , sinOsc
  , sawtoothOsc
  , traingleOsc
  , squareOsc
  , split1
  , split2
  , split3
  , split4
  , split5
  , panner
  , mul
  , add
  , merger
  , constant
  , delay
  , gain
  , speaker
  , speaker_
  , microphone_
  , play_
  , playBuf_
  , loopBuf_
  , playDynamicBuf_
  , loopDynamicBuf_
  , lowpass_
  , highpass_
  , bandpass_
  , lowshelf_
  , highshelf_
  , peaking_
  , notch_
  , allpass_
  , convolver_
  , dynamicConvolver_
  , dynamicsCompressor_
  , dup1_
  , dup2_
  , dup3_
  , dup4_
  , dup5_
  , waveShaper_
  , dynamicWaveShaper_
  , periodicOsc_
  , dynamicPeriodicOsc_
  , sinOsc_
  , sawtoothOsc_
  , traingleOsc_
  , squareOsc_
  , split1_
  , split2_
  , split3_
  , split4_
  , split5_
  , panner_
  , mul_
  , add_
  , merger_
  , constant_
  , delay_
  , gain_
  , playBufT
  , loopBufT
  , playDynamicBufT
  , loopDynamicBufT
  , lowpassT
  , highpassT
  , bandpassT
  , lowshelfT
  , highshelfT
  , peakingT
  , notchT
  , allpassT
  , dynamicsCompressorT
  , periodicOscT
  , dynamicPeriodicOscT
  , sinOscT
  , sawtoothOscT
  , traingleOscT
  , squareOscT
  , pannerT
  , constantT
  , delayT
  , gainT
  , playBufT_
  , loopBufT_
  , playDynamicBufT_
  , loopDynamicBufT_
  , lowpassT_
  , highpassT_
  , bandpassT_
  , lowshelfT_
  , highshelfT_
  , peakingT_
  , notchT_
  , allpassT_
  , dynamicsCompressorT_
  , periodicOscT_
  , dynamicPeriodicOscT_
  , sinOscT_
  , sawtoothOscT_
  , traingleOscT_
  , squareOscT_
  , pannerT_
  , constantT_
  , delayT_
  , gainT_
  , speaker'
  , gain'
  , gainT'
  , gain_'
  , gainT_'
  , audioGrouper
  , makeProgram
  ) where

import Prelude
import Control.Bind (bindFlipped)
import Data.Array (catMaybes, filter, foldl, groupBy, head, index, length, mapWithIndex, range, replicate, snoc, sortWith, takeEnd, zipWith, (!!))
import Data.Array as A
import Data.Array.NonEmpty (toArray)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (floor, toNumber)
import Data.Int.Parse (parseInt, toRadix)
import Data.JSDate (getTime, now)
import Data.List (List(..), fromFoldable, partition, (:))
import Data.List as DL
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.NonEmpty as NE
import Data.Set (member)
import Data.String (Pattern(..), split, take)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd, swap, uncurry)
import Data.Typelevel.Num (class Pos, D1, D2, D3, D4, D5, toInt')
import Data.Unfoldable (class Unfoldable)
import Data.Unfoldable1 as DU
import Data.Vec (Vec, fill)
import Data.Vec as V
import Effect (Effect, whileE)
import Effect.Class.Console (log)
import Effect.Ref (modify_, new, read, write)
import FRP.Behavior (Behavior, behavior, sample_)
import FRP.Event (Event, EventIO, create, makeEvent, subscribe)
import FRP.Event.Time (interval)
import Foreign (Foreign)
import Foreign.Object (Object, filterWithKey)
import Foreign.Object as O
import Graphics.Canvas (CanvasElement, clearRect, getCanvasHeight, getCanvasWidth, getContext2D)
import Graphics.Drawing (Drawing, render)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Math as Math
import Record (merge)
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)

foreign import data BrowserAudioBuffer :: Type

foreign import data BrowserFloatArray :: Type

foreign import data BrowserAudioTrack :: Type

foreign import data AudioContext :: Type

foreign import makeAudioContext :: Effect AudioContext

foreign import makeAudioTrack :: String -> Effect BrowserAudioTrack

foreign import makeAudioBuffer :: AudioContext -> AudioBuffer -> Effect BrowserAudioBuffer

foreign import makeFloatArray :: Array Number -> Effect BrowserFloatArray

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

data AudioBuffer
  = AudioBuffer Int (Array (Array Number))

derive instance genericAudioBuffer :: Generic AudioBuffer _

instance showAudioBuffer :: Show AudioBuffer where
  show s = genericShow s

derive instance eqAudioBuffer :: Eq AudioBuffer

data Oversample
  = None
  | TwoX
  | FourX

derive instance genericOversample :: Generic Oversample _

instance showOversampler :: Show Oversample where
  show s = genericShow s

derive instance eqOversample :: Eq Oversample

newtype AudioParameter a
  = AudioParameter
  { param :: a
  , timeOffset :: Number
  }

instance audioParameterFunctor :: Functor AudioParameter where
  map f (AudioParameter { param, timeOffset }) = AudioParameter { param: f param, timeOffset }

data AudioUnit ch
  = Microphone MString
  | Play MString String Number
  | PlayBuf MString String (AudioParameter Number)
  | LoopBuf MString String (AudioParameter Number) Number Number
  | PlayDynamicBuf MString AudioBuffer (AudioParameter Number)
  | LoopDynamicBuf MString AudioBuffer (AudioParameter Number) Number Number
  | Lowpass MString (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Highpass MString (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Bandpass MString (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Lowshelf MString (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Highshelf MString (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Peaking MString (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Notch MString (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Allpass MString (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Convolver MString String (AudioUnit ch)
  | DynamicConvolver MString AudioBuffer (AudioUnit ch)
  | DynamicsCompressor MString (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | SawtoothOsc MString (AudioParameter Number)
  | TriangleOsc MString (AudioParameter Number)
  | PeriodicOsc MString (AudioParameter Number) String
  | DynamicPeriodicOsc MString (AudioParameter Number) (Array Number) (Array Number)
  | WaveShaper MString String Oversample (AudioUnit ch)
  | DynamicWaveShaper MString (Array Number) Oversample (AudioUnit ch)
  | Dup1 MString (AudioUnit D1) (AudioUnit D1 -> AudioUnit ch)
  | Dup2 MString (AudioUnit D2) (AudioUnit D2 -> AudioUnit ch)
  | Dup3 MString (AudioUnit D3) (AudioUnit D3 -> AudioUnit ch)
  | Dup4 MString (AudioUnit D4) (AudioUnit D4 -> AudioUnit ch)
  | Dup5 MString (AudioUnit D5) (AudioUnit D5 -> AudioUnit ch)
  | SinOsc MString (AudioParameter Number)
  | SquareOsc MString (AudioParameter Number)
  | Split1 MString (AudioUnit D1) (Vec D1 (AudioUnit D1) -> AudioUnit ch)
  | Split2 MString (AudioUnit D2) (Vec D2 (AudioUnit D1) -> AudioUnit ch)
  | Split3 MString (AudioUnit D3) (Vec D3 (AudioUnit D1) -> AudioUnit ch)
  | Split4 MString (AudioUnit D4) (Vec D4 (AudioUnit D1) -> AudioUnit ch)
  | Split5 MString (AudioUnit D5) (Vec D5 (AudioUnit D1) -> AudioUnit ch)
  | StereoPanner MString (AudioParameter Number) (AudioUnit ch)
  | Mul MString (NonEmpty List (AudioUnit ch))
  | Add MString (NonEmpty List (AudioUnit ch))
  | Merger MString (Vec ch (AudioUnit D1))
  | Constant MString (AudioParameter Number)
  | Delay MString (AudioParameter Number) (AudioUnit ch)
  | Gain MString (AudioParameter Number) (NonEmpty List (AudioUnit ch))
  | Speaker MString (NonEmpty List (AudioUnit ch))
  | NoSound MString
  | SplitRes Int
  | DupRes

data AudioUnit'
  = Microphone'
  | Play' String Number
  | PlayBuf' String (AudioParameter Number)
  | LoopBuf' String (AudioParameter Number) Number Number
  | PlayDynamicBuf' AudioBuffer (AudioParameter Number)
  | LoopDynamicBuf' AudioBuffer (AudioParameter Number) Number Number
  | Lowpass' (AudioParameter Number) (AudioParameter Number) (AudioParameter Number)
  | Highpass' (AudioParameter Number) (AudioParameter Number) (AudioParameter Number)
  | Bandpass' (AudioParameter Number) (AudioParameter Number) (AudioParameter Number)
  | Lowshelf' (AudioParameter Number) (AudioParameter Number) (AudioParameter Number)
  | Highshelf' (AudioParameter Number) (AudioParameter Number) (AudioParameter Number)
  | Peaking' (AudioParameter Number) (AudioParameter Number) (AudioParameter Number)
  | Notch' (AudioParameter Number) (AudioParameter Number) (AudioParameter Number)
  | Allpass' (AudioParameter Number) (AudioParameter Number) (AudioParameter Number)
  | Convolver' String
  | DynamicConvolver' AudioBuffer
  | DynamicsCompressor' (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioParameter Number)
  | SawtoothOsc' (AudioParameter Number)
  | TriangleOsc' (AudioParameter Number)
  | PeriodicOsc' (AudioParameter Number) String
  | DynamicPeriodicOsc' (AudioParameter Number) (Array Number) (Array Number)
  | WaveShaper' String Oversample
  | DynamicWaveShaper' (Array Number) Oversample
  | Dup'
  | SinOsc' (AudioParameter Number)
  | SquareOsc' (AudioParameter Number)
  | Splitter' Int
  | StereoPanner' (AudioParameter Number)
  | Mul'
  | Add'
  | Swap'
  | Merger' (List Int)
  | Constant' (AudioParameter Number)
  | Delay' (AudioParameter Number)
  | Gain' (AudioParameter Number)
  | Speaker'
  | NoSound'
  | SplitRes' Int
  | DupRes'

derive instance genericAudioUnit' :: Generic AudioUnit' _

instance showAudioUnit' :: Show AudioUnit' where
  show s = genericShow s

derive instance eqAudioUnit' :: Eq AudioUnit'

data AudioUnit''
  = Microphone''
  | Play''
  | PlayBuf''
  | LoopBuf''
  | PlayDynamicBuf''
  | LoopDynamicBuf''
  | Lowpass''
  | Highpass''
  | Bandpass''
  | Lowshelf''
  | Highshelf''
  | Peaking''
  | Notch''
  | Allpass''
  | Convolver''
  | DynamicConvolver''
  | DynamicsCompressor''
  | SawtoothOsc''
  | TriangleOsc''
  | PeriodicOsc''
  | DynamicPeriodicOsc''
  | WaveShaper''
  | DynamicWaveShaper''
  | Dup''
  | SinOsc''
  | SquareOsc''
  | Splitter''
  | StereoPanner''
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
  | DupRes''

derive instance genericAudioUnit'' :: Generic AudioUnit'' _

instance showAudioUnit'' :: Show AudioUnit'' where
  show s = genericShow s

derive instance eqAudioUnit'' :: Eq AudioUnit''

instance ordAudioUnit'' :: Ord AudioUnit'' where
  compare a b = compare (show a) (show b)

au' :: forall ch. Pos ch => AudioUnit ch -> { au :: AudioUnit', name :: MString }
au' (Microphone name) = { au: Microphone', name }

au' (Play name file timingHack) = { au: Play' file timingHack, name }

au' (PlayBuf name buf speed) = { au: PlayBuf' buf speed, name }

au' (LoopBuf name buf speed start end) = { au: LoopBuf' buf speed start end, name }

au' (PlayDynamicBuf name buf speed) = { au: PlayDynamicBuf' buf speed, name }

au' (LoopDynamicBuf name buf speed start end) = { au: LoopDynamicBuf' buf speed start end, name }

au' (Lowpass name f q g _) = { au: Lowpass' f q g, name }

au' (Highpass name f q g _) = { au: Highpass' f q g, name }

au' (Bandpass name f q g _) = { au: Bandpass' f q g, name }

au' (Lowshelf name f q g _) = { au: Lowshelf' f q g, name }

au' (Highshelf name f q g _) = { au: Highshelf' f q g, name }

au' (Peaking name f q g _) = { au: Peaking' f q g, name }

au' (Notch name f q g _) = { au: Notch' f q g, name }

au' (Allpass name f q g _) = { au: Allpass' f q g, name }

au' (Convolver name buf _) = { au: Convolver' buf, name }

au' (DynamicConvolver name buf _) = { au: DynamicConvolver' buf, name }

au' (DynamicsCompressor name thresh knee ratio attack release _) =
  { au: DynamicsCompressor' thresh knee ratio attack release
  , name
  }

au' (SawtoothOsc name n) = { au: (SawtoothOsc' n), name }

au' (TriangleOsc name n) = { au: (TriangleOsc' n), name }

au' (PeriodicOsc name n s) = { au: (PeriodicOsc' n s), name }

au' (DynamicPeriodicOsc name n r i) = { au: (DynamicPeriodicOsc' n r i), name }

au' (WaveShaper name curve os _) = { au: (WaveShaper' curve os), name }

au' (DynamicWaveShaper name curve os _) = { au: (DynamicWaveShaper' curve os), name }

au' (Dup1 name _ _) = { au: Dup', name } -- hack, we set the actual value later

au' (Dup2 name _ _) = { au: Dup', name } -- hack, we set the actual value later

au' (Dup3 name _ _) = { au: Dup', name } -- hack, we set the actual value later

au' (Dup4 name _ _) = { au: Dup', name } -- hack, we set the actual value later

au' (Dup5 name _ _) = { au: Dup', name } -- hack, we set the actual value later

au' (SinOsc name n) = { au: (SinOsc' n), name }

au' (SquareOsc name n) = { au: (SquareOsc' n), name }

au' (Split1 name _ _) = { au: (Splitter' 1), name }

au' (Split2 name _ _) = { au: (Splitter' 2), name }

au' (Split3 name _ _) = { au: (Splitter' 3), name }

au' (Split4 name _ _) = { au: (Splitter' 4), name }

au' (Split5 name _ _) = { au: (Splitter' 5), name }

au' (StereoPanner name n _) = { au: (StereoPanner' n), name }

au' (Mul name _) = { au: Mul', name }

au' (Add name _) = { au: Add', name }

-------------- this is a hack and is corrected for the merger
-------------- specifically later on
au' (Merger name _) = { au: (Merger' Nil), name }

au' (Constant name n) = { au: (Constant' n), name }

au' (Delay name n _) = { au: (Delay' n), name }

au' (Gain name n _) = { au: (Gain' n), name }

au' (Speaker name _) = { au: Speaker', name }

au' (NoSound name) = { au: NoSound', name }

au' (SplitRes n) = { au: (SplitRes' n), name: Nothing }

au' DupRes = { au: DupRes', name: Nothing }

au'' :: AudioUnit' -> AudioUnit''
au'' Microphone' = Microphone''

au'' (Play' _ _) = Play''

au'' (PlayBuf' _ _) = PlayBuf''

au'' (LoopBuf' _ _ _ _) = LoopBuf''

au'' (PlayDynamicBuf' _ _) = PlayDynamicBuf''

au'' (LoopDynamicBuf' _ _ _ _) = LoopDynamicBuf''

au'' (Lowpass' _ _ _) = Lowpass''

au'' (Highpass' _ _ _) = Highpass''

au'' (Bandpass' _ _ _) = Bandpass''

au'' (Lowshelf' _ _ _) = Lowshelf''

au'' (Highshelf' _ _ _) = Highshelf''

au'' (Peaking' _ _ _) = Peaking''

au'' (Notch' _ _ _) = Notch''

au'' (Allpass' _ _ _) = Allpass''

au'' (Convolver' _) = Convolver''

au'' (DynamicConvolver' _) = DynamicConvolver''

au'' (DynamicsCompressor' _ _ _ _ _) = DynamicsCompressor''

au'' (SawtoothOsc' _) = SawtoothOsc''

au'' (TriangleOsc' _) = TriangleOsc''

au'' (PeriodicOsc' _ _) = PeriodicOsc''

au'' (DynamicPeriodicOsc' _ _ _) = DynamicPeriodicOsc''

au'' (WaveShaper' _ _) = WaveShaper''

au'' (DynamicWaveShaper' _ _) = DynamicWaveShaper''

au'' Dup' = Dup''

au'' (SinOsc' _) = SinOsc''

au'' (SquareOsc' _) = SquareOsc''

au'' (Splitter' _) = Splitter''

au'' (StereoPanner' _) = StereoPanner''

au'' Mul' = Mul''

au'' Add' = Add''

au'' Swap' = Swap''

au'' (Merger' _) = Merger''

au'' (Constant' _) = Constant''

au'' (Delay' _) = Delay''

au'' (Gain' _) = Gain''

au'' Speaker' = Speaker''

au'' NoSound' = NoSound''

au'' (SplitRes' _) = SplitRes''

au'' DupRes' = DupRes''

tagToAU :: AudioUnit'' -> AudioUnit'
tagToAU Microphone'' = Microphone'

tagToAU Play'' = Play' "" 0.0

tagToAU PlayBuf'' = PlayBuf' "" (ap_ (-1.0))

tagToAU LoopBuf'' = LoopBuf' "" (ap_ (-1.0)) (-1.0) (-1.0)

tagToAU PlayDynamicBuf'' = PlayDynamicBuf' (AudioBuffer 0 [ [] ]) (ap_ (-1.0))

tagToAU LoopDynamicBuf'' = LoopDynamicBuf' (AudioBuffer 0 [ [] ]) (ap_ (-1.0)) (-1.0) (-1.0)

tagToAU Lowpass'' = Lowpass' (ap_ (-1.0)) (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Highpass'' = Highpass' (ap_ (-1.0)) (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Bandpass'' = Bandpass' (ap_ (-1.0)) (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Lowshelf'' = Lowshelf' (ap_ (-1.0)) (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Highshelf'' = Highshelf' (ap_ (-1.0)) (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Peaking'' = Peaking' (ap_ (-1.0)) (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Notch'' = Notch' (ap_ (-1.0)) (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Allpass'' = Allpass' (ap_ (-1.0)) (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Convolver'' = Convolver' ""

tagToAU DynamicConvolver'' = DynamicConvolver' (AudioBuffer 0 [ [] ])

tagToAU DynamicsCompressor'' = DynamicsCompressor' (ap_ (-1.0)) (ap_ (-1.0)) (ap_ (-1.0)) (ap_ (-1.0)) (ap_ (-1.0))

tagToAU SawtoothOsc'' = SawtoothOsc' (ap_ 50000.0)

tagToAU TriangleOsc'' = TriangleOsc' (ap_ 50000.0)

tagToAU PeriodicOsc'' = PeriodicOsc' (ap_ 50000.0) ""

tagToAU DynamicPeriodicOsc'' = DynamicPeriodicOsc' (ap_ 50000.0) [] []

tagToAU WaveShaper'' = WaveShaper' "" None

tagToAU DynamicWaveShaper'' = DynamicWaveShaper' [] None

tagToAU Dup'' = Dup'

tagToAU SinOsc'' = SinOsc' (ap_ 50000.0)

tagToAU SquareOsc'' = SquareOsc' (ap_ 50000.0)

tagToAU Splitter'' = Splitter' (-1)

tagToAU StereoPanner'' = (StereoPanner' (ap_ 3.0))

tagToAU Mul'' = Mul'

tagToAU Add'' = Add'

tagToAU Swap'' = Swap'

tagToAU Merger'' = Merger' Nil

tagToAU Constant'' = Constant' (ap_ 1000.0)

tagToAU Delay'' = Delay' (ap_ 1000.0)

tagToAU Gain'' = Gain' (ap_ 1000.0)

tagToAU Speaker'' = Speaker'

tagToAU NoSound'' = NoSound'

tagToAU SplitRes'' = (SplitRes' (-1))

tagToAU DupRes'' = DupRes'

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

mergerHack :: AudioUnit' -> NonEmpty List Int -> AudioUnit'
mergerHack (Merger' _) (h :| t) = Merger' (h : t)

mergerHack a _ = a

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
            au =
              ( \{ au: awd, name } ->
                  { au: mergerHack awd (map _.p.ptr r), name }
              )
                (au' v)
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

  closurethrough ::
    forall ch ix.
    Pos ch =>
    Pos ix =>
    PtrInfo' ->
    AudioUnit ch ->
    AudioUnit ix ->
    AudioUnit ch ->
    AlgStep
  closurethrough ptr v a ic =
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

  go' :: forall ch. Pos ch => PtrInfo' -> AudioUnit ch -> AlgStep
  go' ptr v@(Microphone name) = terminus ptr v

  go' ptr v@(Play _ _ _) = terminus ptr v

  go' ptr v@(PlayBuf _ _ _) = terminus ptr v

  go' ptr v@(LoopBuf _ _ _ _ _) = terminus ptr v

  go' ptr v@(PlayDynamicBuf _ _ _) = terminus ptr v

  go' ptr v@(LoopDynamicBuf _ _ _ _ _) = terminus ptr v

  go' ptr v@(Lowpass _ _ _ _ a) = passthrough ptr v a

  go' ptr v@(Highpass _ _ _ _ a) = passthrough ptr v a

  go' ptr v@(Bandpass _ _ _ _ a) = passthrough ptr v a

  go' ptr v@(Lowshelf _ _ _ _ a) = passthrough ptr v a

  go' ptr v@(Highshelf _ _ _ _ a) = passthrough ptr v a

  go' ptr v@(Peaking _ _ _ _ a) = passthrough ptr v a

  go' ptr v@(Notch _ _ _ _ a) = passthrough ptr v a

  go' ptr v@(Allpass _ _ _ _ a) = passthrough ptr v a

  go' ptr v@(Convolver _ _ a) = passthrough ptr v a

  go' ptr v@(DynamicConvolver _ _ a) = passthrough ptr v a

  go' ptr v@(DynamicsCompressor _ _ _ _ _ _ a) = passthrough ptr v a

  go' ptr v@(SawtoothOsc _ _) = terminus ptr v

  go' ptr v@(TriangleOsc _ _) = terminus ptr v

  go' ptr v@(PeriodicOsc _ _ _) = terminus ptr v

  go' ptr v@(DynamicPeriodicOsc _ _ _ _) = terminus ptr v

  go' ptr v@(WaveShaper _ _ _ a) = passthrough ptr v a

  go' ptr v@(DynamicWaveShaper _ _ _ a) = passthrough ptr v a

  go' ptr v@(Dup1 name a f) = closurethrough ptr v a $ f DupRes

  go' ptr v@(Dup2 name a f) = closurethrough ptr v a $ f DupRes

  go' ptr v@(Dup3 name a f) = closurethrough ptr v a $ f DupRes

  go' ptr v@(Dup4 name a f) = closurethrough ptr v a $ f DupRes

  go' ptr v@(Dup5 name a f) = closurethrough ptr v a $ f DupRes

  go' ptr v@(SinOsc name n) = terminus ptr v

  go' ptr v@(SquareOsc name n) = terminus ptr v

  go' ptr v@(Constant name n) = terminus ptr v

  go' ptr v@(NoSound name) = terminus ptr v

  go' ptr v@(SplitRes n) = terminus ptr v

  go' ptr v@(DupRes) = terminus ptr v

  go' ptr v@(StereoPanner name n a) = passthrough ptr v a

  go' ptr v@(Delay name n a) = passthrough ptr v a

  go' ptr v@(Mul name l) = listthrough ptr v l

  go' ptr v@(Merger name l) = listthrough ptr v (V.head l :| ((chopHack <<< fromFoldable <<< V.toArray) l))

  go' ptr v@(Add name l) = listthrough ptr v l

  go' ptr v@(Gain name n l) = listthrough ptr v l

  go' ptr v@(Speaker name l) = listthrough ptr v l

  go' ptr v@(Split1 name a f) = closurethrough ptr v a $ f (fill SplitRes)

  go' ptr v@(Split2 name a f) = closurethrough ptr v a $ f (fill SplitRes)

  go' ptr v@(Split3 name a f) = closurethrough ptr v a $ f (fill SplitRes)

  go' ptr v@(Split4 name a f) = closurethrough ptr v a $ f (fill SplitRes)

  go' ptr v@(Split5 name a f) = closurethrough ptr v a $ f (fill SplitRes)

microphone :: AudioUnit D1
microphone = Microphone Nothing

microphone_ :: String -> AudioUnit D1
microphone_ = Microphone <<< Just

apP :: forall a. AudioParameter a -> a
apP (AudioParameter { param }) = param

apT :: forall a. AudioParameter a -> Number
apT (AudioParameter { timeOffset }) = timeOffset

play ::
  forall ch.
  Pos ch =>
  String ->
  AudioUnit ch
play handle = Play Nothing handle 0.0

play_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  AudioUnit ch
play_ s handle = Play (Just s) handle 0.0

playT ::
  forall ch.
  Pos ch =>
  String ->
  Number ->
  AudioUnit ch
playT handle n = Play Nothing handle n

playT_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  Number ->
  AudioUnit ch
playT_ s handle n = Play (Just s) handle n

playBuf ::
  forall ch.
  Pos ch =>
  String ->
  Number ->
  AudioUnit ch
playBuf handle n = PlayBuf Nothing handle (ap_ n)

playBuf_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  Number ->
  AudioUnit ch
playBuf_ s handle n = PlayBuf (Just s) handle (ap_ n)

playBufT ::
  forall ch.
  Pos ch =>
  String ->
  AudioParameter Number ->
  AudioUnit ch
playBufT handle n = PlayBuf Nothing handle n

playBufT_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  AudioParameter Number ->
  AudioUnit ch
playBufT_ s handle n = PlayBuf (Just s) handle n

loopBuf ::
  forall ch.
  Pos ch =>
  String ->
  Number ->
  Number ->
  Number ->
  AudioUnit ch
loopBuf handle a b c = LoopBuf Nothing handle (ap_ a) b c

loopBuf_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  Number ->
  Number ->
  Number ->
  AudioUnit ch
loopBuf_ s handle a b c = LoopBuf (Just s) handle (ap_ a) b c

loopBufT ::
  forall ch.
  Pos ch =>
  String ->
  AudioParameter Number ->
  Number ->
  Number ->
  AudioUnit ch
loopBufT handle a b c = LoopBuf Nothing handle a b c

loopBufT_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  AudioParameter Number ->
  Number ->
  Number ->
  AudioUnit ch
loopBufT_ s handle a b c = LoopBuf (Just s) handle a b c

playDynamicBuf ::
  forall ch bch blen.
  Pos ch =>
  Pos bch =>
  Pos blen =>
  Int ->
  Vec bch (Vec blen Number) ->
  Number ->
  AudioUnit ch
playDynamicBuf i v a = PlayDynamicBuf Nothing (AudioBuffer i (map V.toArray $ V.toArray v)) (ap_ a)

audioBuffer ::
  forall bch blen.
  Pos bch =>
  Pos blen =>
  Int ->
  Vec bch (Vec blen Number) ->
  AudioBuffer
audioBuffer i v = AudioBuffer i (map V.toArray $ V.toArray v)

playDynamicBuf_ ::
  forall ch bch blen.
  Pos ch =>
  Pos bch =>
  Pos blen =>
  String ->
  Int ->
  Vec bch (Vec blen Number) ->
  Number ->
  AudioUnit ch
playDynamicBuf_ s i v a = PlayDynamicBuf (Just s) (AudioBuffer i (map V.toArray $ V.toArray v)) (ap_ a)

playDynamicBufT ::
  forall ch bch blen.
  Pos ch =>
  Pos bch =>
  Pos blen =>
  Int ->
  Vec bch (Vec blen Number) ->
  AudioParameter Number ->
  AudioUnit ch
playDynamicBufT i v a = PlayDynamicBuf Nothing (AudioBuffer i (map V.toArray $ V.toArray v)) a

playDynamicBufT_ ::
  forall ch bch blen.
  Pos ch =>
  Pos bch =>
  Pos blen =>
  String ->
  Int ->
  Vec bch (Vec blen Number) ->
  AudioParameter Number ->
  AudioUnit ch
playDynamicBufT_ s i v a = PlayDynamicBuf (Just s) (AudioBuffer i (map V.toArray $ V.toArray v)) a

loopDynamicBuf ::
  forall ch bch blen.
  Pos ch =>
  Pos bch =>
  Pos blen =>
  Int ->
  Vec bch (Vec blen Number) ->
  Number ->
  Number ->
  Number ->
  AudioUnit ch
loopDynamicBuf i v a b c = LoopDynamicBuf Nothing (AudioBuffer i (map V.toArray $ V.toArray v)) (ap_ a) b c

loopDynamicBuf_ ::
  forall ch bch blen.
  Pos ch =>
  Pos bch =>
  Pos blen =>
  String ->
  Int ->
  Vec bch (Vec blen Number) ->
  Number ->
  Number ->
  Number ->
  AudioUnit ch
loopDynamicBuf_ s i v a b c = LoopDynamicBuf (Just s) (AudioBuffer i (map V.toArray $ V.toArray v)) (ap_ a) b c

loopDynamicBufT ::
  forall ch bch blen.
  Pos ch =>
  Pos bch =>
  Pos blen =>
  Int ->
  Vec bch (Vec blen Number) ->
  AudioParameter Number ->
  Number ->
  Number ->
  AudioUnit ch
loopDynamicBufT i v a b c = LoopDynamicBuf Nothing (AudioBuffer i (map V.toArray $ V.toArray v)) a b c

loopDynamicBufT_ ::
  forall ch bch blen.
  Pos ch =>
  Pos bch =>
  Pos blen =>
  String ->
  Int ->
  Vec bch (Vec blen Number) ->
  AudioParameter Number ->
  Number ->
  Number ->
  AudioUnit ch
loopDynamicBufT_ s i v a b c = LoopDynamicBuf (Just s) (AudioBuffer i (map V.toArray $ V.toArray v)) a b c

lowpass :: forall ch. Pos ch => Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
lowpass a b c = Lowpass Nothing (ap_ a) (ap_ b) (ap_ c)

lowpass_ :: forall ch. Pos ch => String -> Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
lowpass_ s a b c = Lowpass (Just s) (ap_ a) (ap_ b) (ap_ c)

lowpassT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
lowpassT a b c = Lowpass Nothing a b c

lowpassT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
lowpassT_ s a b c = Lowpass (Just s) a b c

highpass :: forall ch. Pos ch => Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
highpass a b c = Highpass Nothing (ap_ a) (ap_ b) (ap_ c)

highpass_ :: forall ch. Pos ch => String -> Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
highpass_ s a b c = Highpass (Just s) (ap_ a) (ap_ b) (ap_ c)

highpassT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
highpassT a b c = Highpass Nothing a b c

highpassT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
highpassT_ s a b c = Highpass (Just s) a b c

bandpass :: forall ch. Pos ch => Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
bandpass a b c = Bandpass Nothing (ap_ a) (ap_ b) (ap_ c)

bandpass_ :: forall ch. Pos ch => String -> Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
bandpass_ s a b c = Bandpass (Just s) (ap_ a) (ap_ b) (ap_ c)

bandpassT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
bandpassT a b c = Bandpass Nothing a b c

bandpassT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
bandpassT_ s a b c = Bandpass (Just s) a b c

lowshelf :: forall ch. Pos ch => Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
lowshelf a b c = Lowshelf Nothing (ap_ a) (ap_ b) (ap_ c)

lowshelf_ :: forall ch. Pos ch => String -> Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
lowshelf_ s a b c = Lowshelf (Just s) (ap_ a) (ap_ b) (ap_ c)

lowshelfT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
lowshelfT a b c = Lowshelf Nothing a b c

lowshelfT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
lowshelfT_ s a b c = Lowshelf (Just s) a b c

highshelf :: forall ch. Pos ch => Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
highshelf a b c = Highshelf Nothing (ap_ a) (ap_ b) (ap_ c)

highshelf_ :: forall ch. Pos ch => String -> Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
highshelf_ s a b c = Highshelf (Just s) (ap_ a) (ap_ b) (ap_ c)

highshelfT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
highshelfT a b c = Highshelf Nothing a b c

highshelfT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
highshelfT_ s a b c = Highshelf (Just s) a b c

peaking :: forall ch. Pos ch => Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
peaking a b c = Peaking Nothing (ap_ a) (ap_ b) (ap_ c)

peaking_ :: forall ch. Pos ch => String -> Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
peaking_ s a b c = Peaking (Just s) (ap_ a) (ap_ b) (ap_ c)

peakingT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
peakingT a b c = Peaking Nothing a b c

peakingT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
peakingT_ s a b c = Peaking (Just s) a b c

notch :: forall ch. Pos ch => Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
notch a b c = Notch Nothing (ap_ a) (ap_ b) (ap_ c)

notch_ :: forall ch. Pos ch => String -> Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
notch_ s a b c = Notch (Just s) (ap_ a) (ap_ b) (ap_ c)

notchT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
notchT a b c = Notch Nothing a b c

notchT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
notchT_ s a b c = Notch (Just s) a b c

allpass :: forall ch. Pos ch => Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
allpass a b c = Allpass Nothing (ap_ a) (ap_ b) (ap_ c)

allpass_ :: forall ch. Pos ch => String -> Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
allpass_ s a b c = Allpass (Just s) (ap_ a) (ap_ b) (ap_ c)

allpassT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
allpassT a b c = Allpass Nothing a b c

allpassT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
allpassT_ s a b c = Allpass (Just s) a b c

convolver ::
  forall ch.
  Pos ch =>
  String ->
  AudioUnit ch ->
  AudioUnit ch
convolver handle = Convolver Nothing handle

convolver_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  AudioUnit ch ->
  AudioUnit ch
convolver_ s handle = Convolver (Just s) handle

dynamicConvolver ::
  forall ch bch blen.
  Pos ch =>
  Pos bch =>
  Pos blen =>
  Int ->
  Vec bch (Vec blen Number) ->
  AudioUnit ch ->
  AudioUnit ch
dynamicConvolver i v = DynamicConvolver Nothing (AudioBuffer i (map V.toArray $ V.toArray v))

dynamicConvolver_ ::
  forall ch bch blen.
  Pos ch =>
  Pos bch =>
  Pos blen =>
  String ->
  Int ->
  Vec bch (Vec blen Number) ->
  AudioUnit ch ->
  AudioUnit ch
dynamicConvolver_ s i v = DynamicConvolver (Just s) (AudioBuffer i (map V.toArray $ V.toArray v))

dynamicsCompressor ::
  forall ch.
  Pos ch =>
  Number -> Number -> Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
dynamicsCompressor a b c d e = DynamicsCompressor Nothing (ap_ a) (ap_ b) (ap_ c) (ap_ d) (ap_ e)

dynamicsCompressor_ ::
  forall ch.
  Pos ch =>
  String -> Number -> Number -> Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
dynamicsCompressor_ s a b c d e = DynamicsCompressor (Just s) (ap_ a) (ap_ b) (ap_ c) (ap_ d) (ap_ e)

dynamicsCompressorT ::
  forall ch.
  Pos ch =>
  AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
dynamicsCompressorT a b c d e = DynamicsCompressor Nothing a b c d e

dynamicsCompressorT_ ::
  forall ch.
  Pos ch =>
  String ->
  AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
dynamicsCompressorT_ s a b c d e = DynamicsCompressor (Just s) a b c d e

dup1 :: forall ch. Pos ch => AudioUnit D1 -> (AudioUnit D1 -> AudioUnit ch) -> AudioUnit ch
dup1 DupRes f = f DupRes

dup1 x f = Dup1 Nothing x f

dup1_ :: forall ch. Pos ch => String -> AudioUnit D1 -> (AudioUnit D1 -> AudioUnit ch) -> AudioUnit ch
dup1_ s DupRes f = f DupRes

dup1_ s x f = Dup1 (Just s) x f

dup2 :: forall ch. Pos ch => AudioUnit D2 -> (AudioUnit D2 -> AudioUnit ch) -> AudioUnit ch
dup2 DupRes f = f DupRes

dup2 x f = Dup2 Nothing x f

dup2_ :: forall ch. Pos ch => String -> AudioUnit D2 -> (AudioUnit D2 -> AudioUnit ch) -> AudioUnit ch
dup2_ s DupRes f = f DupRes

dup2_ s x f = Dup2 (Just s) x f

dup3 :: forall ch. Pos ch => AudioUnit D3 -> (AudioUnit D3 -> AudioUnit ch) -> AudioUnit ch
dup3 DupRes f = f DupRes

dup3 x f = Dup3 Nothing x f

dup3_ :: forall ch. Pos ch => String -> AudioUnit D3 -> (AudioUnit D3 -> AudioUnit ch) -> AudioUnit ch
dup3_ s DupRes f = f DupRes

dup3_ s x f = Dup3 (Just s) x f

dup4 :: forall ch. Pos ch => AudioUnit D4 -> (AudioUnit D4 -> AudioUnit ch) -> AudioUnit ch
dup4 DupRes f = f DupRes

dup4 x f = Dup4 Nothing x f

dup4_ :: forall ch. Pos ch => String -> AudioUnit D4 -> (AudioUnit D4 -> AudioUnit ch) -> AudioUnit ch
dup4_ s DupRes f = f DupRes

dup4_ s x f = Dup4 (Just s) x f

dup5 :: forall ch. Pos ch => AudioUnit D5 -> (AudioUnit D5 -> AudioUnit ch) -> AudioUnit ch
dup5 DupRes f = f DupRes

dup5 x f = Dup5 Nothing x f

dup5_ :: forall ch. Pos ch => String -> AudioUnit D5 -> (AudioUnit D5 -> AudioUnit ch) -> AudioUnit ch
dup5_ s DupRes f = f DupRes

dup5_ s x f = Dup5 (Just s) x f

ap_ :: forall a. a -> AudioParameter a
ap_ a =
  AudioParameter
    { param: a
    , timeOffset: 0.0
    }

instance showAPNum :: Show (AudioParameter Number) where
  show (AudioParameter s) = show s

instance showAPANum :: Show (AudioParameter (Array Number)) where
  show (AudioParameter s) = show s

instance showAPB :: Show (AudioParameter AudioBuffer) where
  show (AudioParameter s) = show s

instance showAPO :: Show (AudioParameter Oversample) where
  show (AudioParameter s) = show s

instance eqAPNum :: Eq (AudioParameter Number) where
  eq (AudioParameter s) (AudioParameter r) = s == r

instance eqAPANum :: Eq (AudioParameter (Array Number)) where
  eq (AudioParameter s) (AudioParameter r) = s == r

instance eqAPB :: Eq (AudioParameter AudioBuffer) where
  eq (AudioParameter s) (AudioParameter r) = s == r

instance eqAPO :: Eq (AudioParameter Oversample) where
  eq (AudioParameter s) (AudioParameter r) = s == r

waveShaper ::
  forall ch.
  Pos ch =>
  String ->
  Oversample ->
  AudioUnit ch ->
  AudioUnit ch
waveShaper handle = WaveShaper Nothing handle

waveShaper_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  Oversample ->
  AudioUnit ch ->
  AudioUnit ch
waveShaper_ s handle = WaveShaper (Just s) handle

dynamicWaveShaper ::
  forall ch.
  Pos ch =>
  Array Number ->
  Oversample ->
  AudioUnit ch ->
  AudioUnit ch
dynamicWaveShaper = DynamicWaveShaper Nothing

dynamicWaveShaper_ ::
  forall ch.
  Pos ch =>
  String ->
  Array Number ->
  Oversample ->
  AudioUnit ch ->
  AudioUnit ch
dynamicWaveShaper_ s = DynamicWaveShaper (Just s)

periodicOsc ::
  forall ch.
  Pos ch =>
  String ->
  Number ->
  AudioUnit D1
periodicOsc handle n = PeriodicOsc Nothing (ap_ n) handle

periodicOsc_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  Number ->
  AudioUnit D1
periodicOsc_ s handle n = PeriodicOsc (Just s) (ap_ n) handle

periodicOscT ::
  forall ch.
  Pos ch =>
  String ->
  AudioParameter Number ->
  AudioUnit D1
periodicOscT handle n = PeriodicOsc Nothing n handle

periodicOscT_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  AudioParameter Number ->
  AudioUnit D1
periodicOscT_ s handle n = PeriodicOsc (Just s) n handle

dynamicPeriodicOsc ::
  forall len.
  Pos len =>
  Number ->
  Vec len Number ->
  Vec len Number ->
  AudioUnit D1
dynamicPeriodicOsc n a b = DynamicPeriodicOsc Nothing (ap_ n) (V.toArray a) (V.toArray b)

dynamicPeriodicOsc_ ::
  forall len.
  Pos len =>
  String ->
  Number ->
  Vec len Number ->
  Vec len Number ->
  AudioUnit D1
dynamicPeriodicOsc_ s n a b = DynamicPeriodicOsc (Just s) (ap_ n) (V.toArray a) (V.toArray b)

dynamicPeriodicOscT ::
  forall len.
  Pos len =>
  AudioParameter Number ->
  Vec len Number ->
  Vec len Number ->
  AudioUnit D1
dynamicPeriodicOscT n a b = DynamicPeriodicOsc Nothing n (V.toArray a) (V.toArray b)

dynamicPeriodicOscT_ ::
  forall len.
  Pos len =>
  String ->
  AudioParameter Number ->
  Vec len Number ->
  Vec len Number ->
  AudioUnit D1
dynamicPeriodicOscT_ s n a b = DynamicPeriodicOsc (Just s) n (V.toArray a) (V.toArray b)

sinOsc :: Number -> AudioUnit D1
sinOsc n = SinOsc Nothing (ap_ n)

sinOsc_ :: String -> Number -> AudioUnit D1
sinOsc_ s n = SinOsc (Just s) (ap_ n)

sinOscT :: AudioParameter Number -> AudioUnit D1
sinOscT n = SinOsc Nothing n

sinOscT_ :: String -> AudioParameter Number -> AudioUnit D1
sinOscT_ s n = SinOsc (Just s) n

sawtoothOsc :: Number -> AudioUnit D1
sawtoothOsc n = SawtoothOsc Nothing (ap_ n)

sawtoothOsc_ :: String -> Number -> AudioUnit D1
sawtoothOsc_ s n = SawtoothOsc (Just s) (ap_ n)

sawtoothOscT :: AudioParameter Number -> AudioUnit D1
sawtoothOscT n = SawtoothOsc Nothing n

sawtoothOscT_ :: String -> AudioParameter Number -> AudioUnit D1
sawtoothOscT_ s n = SawtoothOsc (Just s) n

traingleOsc :: Number -> AudioUnit D1
traingleOsc n = TriangleOsc Nothing (ap_ n)

traingleOsc_ :: String -> Number -> AudioUnit D1
traingleOsc_ s n = TriangleOsc (Just s) (ap_ n)

traingleOscT :: AudioParameter Number -> AudioUnit D1
traingleOscT n = TriangleOsc Nothing n

traingleOscT_ :: String -> AudioParameter Number -> AudioUnit D1
traingleOscT_ s n = TriangleOsc (Just s) n

squareOsc :: Number -> AudioUnit D1
squareOsc n = SquareOsc Nothing (ap_ n)

squareOsc_ :: String -> Number -> AudioUnit D1
squareOsc_ s n = SquareOsc (Just s) (ap_ n)

squareOscT :: AudioParameter Number -> AudioUnit D1
squareOscT n = SquareOsc Nothing n

squareOscT_ :: String -> AudioParameter Number -> AudioUnit D1
squareOscT_ s n = SquareOsc (Just s) n

split1 :: forall ch. Pos ch => AudioUnit D1 -> (Vec D1 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split1 (SplitRes i) f = f (fill $ const (SplitRes i))

split1 x f = Split1 Nothing x f

split1_ :: forall ch. Pos ch => String -> AudioUnit D1 -> (Vec D1 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split1_ s (SplitRes i) f = f (fill $ const (SplitRes i))

split1_ s x f = Split1 (Just s) x f

split2 :: forall ch. Pos ch => AudioUnit D2 -> (Vec D2 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split2 (SplitRes i) f = f (fill $ const (SplitRes i))

split2 x f = Split2 Nothing x f

split2_ :: forall ch. Pos ch => String -> AudioUnit D2 -> (Vec D2 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split2_ s (SplitRes i) f = f (fill $ const (SplitRes i))

split2_ s x f = Split2 (Just s) x f

split3 :: forall ch. Pos ch => AudioUnit D3 -> (Vec D3 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split3 (SplitRes i) f = f (fill $ const (SplitRes i))

split3 x f = Split3 Nothing x f

split3_ :: forall ch. Pos ch => String -> AudioUnit D3 -> (Vec D3 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split3_ s (SplitRes i) f = f (fill $ const (SplitRes i))

split3_ s x f = Split3 (Just s) x f

split4 :: forall ch. Pos ch => AudioUnit D4 -> (Vec D4 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split4 (SplitRes i) f = f (fill $ const (SplitRes i))

split4 x f = Split4 Nothing x f

split4_ :: forall ch. Pos ch => String -> AudioUnit D4 -> (Vec D4 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split4_ s (SplitRes i) f = f (fill $ const (SplitRes i))

split4_ s x f = Split4 (Just s) x f

split5 :: forall ch. Pos ch => AudioUnit D5 -> (Vec D5 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split5 (SplitRes i) f = f (fill $ const (SplitRes i))

split5 x f = Split5 Nothing x f

split5_ :: forall ch. Pos ch => String -> AudioUnit D5 -> (Vec D5 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split5_ s (SplitRes i) f = f (fill $ const (SplitRes i))

split5_ s x f = Split5 (Just s) x f

panner :: Number -> AudioUnit D2 -> AudioUnit D2
panner n = StereoPanner Nothing (ap_ n)

panner_ :: String -> Number -> AudioUnit D2 -> AudioUnit D2
panner_ s n = StereoPanner (Just s) (ap_ n)

pannerT :: AudioParameter Number -> AudioUnit D2 -> AudioUnit D2
pannerT n = StereoPanner Nothing n

pannerT_ :: String -> AudioParameter Number -> AudioUnit D2 -> AudioUnit D2
pannerT_ s n = StereoPanner (Just s) n

speaker :: forall ch. Pos ch => NonEmpty List (AudioUnit ch) -> AudioUnit ch
speaker = Speaker Nothing

speaker' :: forall ch. Pos ch => AudioUnit ch -> AudioUnit ch
speaker' = Speaker Nothing <<< NE.singleton

speaker_ :: forall ch. Pos ch => String -> NonEmpty List (AudioUnit ch) -> AudioUnit ch
speaker_ = Speaker <<< Just

merger ::
  forall ch.
  Pos ch =>
  Vec ch (AudioUnit D1) ->
  AudioUnit ch
merger = Merger Nothing

merger_ ::
  forall ch.
  Pos ch =>
  String ->
  Vec ch (AudioUnit D1) ->
  AudioUnit ch
merger_ s = Merger (Just s)

mul :: forall ch. Pos ch => NonEmpty List (AudioUnit ch) -> AudioUnit ch
mul = Mul Nothing

mul_ :: forall ch. Pos ch => String -> NonEmpty List (AudioUnit ch) -> AudioUnit ch
mul_ s = Mul (Just s)

add :: forall ch. Pos ch => NonEmpty List (AudioUnit ch) -> AudioUnit ch
add = Add Nothing

add_ :: forall ch. Pos ch => String -> NonEmpty List (AudioUnit ch) -> AudioUnit ch
add_ s = Add (Just s)

constant :: Number -> AudioUnit D1
constant n = Constant Nothing (ap_ n)

constant_ :: String -> Number -> AudioUnit D1
constant_ s n = Constant (Just s) (ap_ n)

constantT :: AudioParameter Number -> AudioUnit D1
constantT n = Constant Nothing n

constantT_ :: String -> AudioParameter Number -> AudioUnit D1
constantT_ s n = Constant (Just s) n

delay :: forall ch. Pos ch => Number -> AudioUnit ch -> AudioUnit ch
delay n = Delay Nothing (ap_ n)

delay_ :: forall ch. Pos ch => String -> Number -> AudioUnit ch -> AudioUnit ch
delay_ s n = Delay (Just s) (ap_ n)

delayT :: forall ch. Pos ch => AudioParameter Number -> AudioUnit ch -> AudioUnit ch
delayT n = Delay Nothing n

delayT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
delayT_ s n = Delay (Just s) n

gain :: forall ch. Pos ch => Number -> NonEmpty List (AudioUnit ch) -> AudioUnit ch
gain n = Gain Nothing (ap_ n)

gain_ :: forall ch. Pos ch => String -> Number -> NonEmpty List (AudioUnit ch) -> AudioUnit ch
gain_ s n = Gain (Just s) (ap_ n)

gainT :: forall ch. Pos ch => AudioParameter Number -> NonEmpty List (AudioUnit ch) -> AudioUnit ch
gainT n = Gain Nothing n

gainT_ :: forall ch. Pos ch => String -> AudioParameter Number -> NonEmpty List (AudioUnit ch) -> AudioUnit ch
gainT_ s n = Gain (Just s) n

gain' :: forall ch. Pos ch => Number -> AudioUnit ch -> AudioUnit ch
gain' n = Gain Nothing (ap_ n) <<< NE.singleton

gainT' :: forall ch. Pos ch => AudioParameter Number -> AudioUnit ch -> AudioUnit ch
gainT' n = Gain Nothing n <<< NE.singleton

gain_' :: forall ch. Pos ch => String -> Number -> AudioUnit ch -> AudioUnit ch
gain_' s n = Gain (Just s) (ap_ n) <<< NE.singleton

gainT_' :: forall ch. Pos ch => String -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
gainT_' s n = Gain (Just s) n <<< NE.singleton

instance semiringAudioUnit :: Semiring (AudioUnit ch) where
  zero = Constant Nothing (ap_ 0.0)
  one = Constant Nothing (ap_ 1.0)
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

ucomp (Play' s0 _) (Play' s1 _) = s0 == s1

ucomp (PlayBuf' s0 _) (PlayBuf' s1 _) = s0 == s1

ucomp (LoopBuf' s0 _ _ _) (LoopBuf' s1 _ _ _) = s0 == s1

ucomp (PlayDynamicBuf' s0 _) (PlayDynamicBuf' s1 _) = true

ucomp (LoopDynamicBuf' s0 _ _ _) (LoopDynamicBuf' s1 _ _ _) = true

ucomp (Lowpass' _ _ _) (Lowpass' _ _ _) = true

ucomp (Highpass' _ _ _) (Highpass' _ _ _) = true

ucomp (Bandpass' _ _ _) (Bandpass' _ _ _) = true

ucomp (Lowshelf' _ _ _) (Lowshelf' _ _ _) = true

ucomp (Highshelf' _ _ _) (Highshelf' _ _ _) = true

ucomp (Peaking' _ _ _) (Peaking' _ _ _) = true

ucomp (Notch' _ _ _) (Notch' _ _ _) = true

ucomp (Allpass' _ _ _) (Allpass' _ _ _) = true

ucomp (Convolver' s0) (Convolver' s1) = s0 == s1

ucomp (DynamicConvolver' _) (DynamicConvolver' _) = true

ucomp (DynamicsCompressor' _ _ _ _ _) (DynamicsCompressor' _ _ _ _ _) = true

ucomp (SawtoothOsc' _) (SawtoothOsc' _) = true

ucomp (TriangleOsc' _) (TriangleOsc' _) = true

ucomp (PeriodicOsc' _ s0) (PeriodicOsc' _ s1) = s0 == s1

ucomp (DynamicPeriodicOsc' _ _ _) (DynamicPeriodicOsc' _ _ _) = true

ucomp (WaveShaper' s0 _) (WaveShaper' s1 _) = s0 == s1

ucomp (DynamicWaveShaper' _ _) (DynamicWaveShaper' _ _) = true

ucomp (Dup') (Dup') = true

ucomp (SinOsc' _) (SinOsc' _) = true

ucomp (SquareOsc' _) (SquareOsc' _) = true

ucomp (Splitter' _) (Splitter' _) = true

ucomp (StereoPanner' _) (StereoPanner' _) = true

ucomp Mul' Mul' = true

ucomp Add' Add' = true

ucomp Swap' Swap' = true

ucomp (Merger' _) (Merger' _) = true

ucomp (Constant' _) (Constant' _) = true

ucomp (Delay' _) (Delay' _) = true

ucomp (Gain' _) (Gain' _) = true

ucomp Speaker' Speaker' = true

ucomp NoSound' NoSound' = true

ucomp (SplitRes' _) (SplitRes' _) = true

ucomp (DupRes') (DupRes') = true

ucomp _ _ = false

oscMULT = 1.0 / 22100.0 :: Number

gainMULT = 1.0 :: Number

qMULT = 1.0 :: Number

constMULT = 1.0 :: Number

delayMULT = 0.1 :: Number

panMULT = 0.5 :: Number

srMULT = 100.0 :: Number -- should never happen

filtCoef :: AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> Number
filtCoef (AudioParameter { param: a }) (AudioParameter { param: b }) (AudioParameter { param: c }) (AudioParameter { param: x }) (AudioParameter { param: y }) (AudioParameter { param: z }) = (Math.abs $ a - x) * oscMULT + (Math.abs $ b - y) * qMULT + (Math.abs $ c - z) * gainMULT

twoCoef :: AudioParameter Number -> AudioParameter Number -> Number
twoCoef (AudioParameter { param: f0 }) (AudioParameter { param: f1 }) = (Math.abs $ f0 - f1)

toCoef :: AudioUnit' -> AudioUnit' -> Number
toCoef Microphone' Microphone' = 0.0

toCoef (Lowpass' a b c) (Lowpass' x y z) = filtCoef a b c x y z

toCoef (Highpass' a b c) (Highpass' x y z) = filtCoef a b c x y z

toCoef (Bandpass' a b c) (Bandpass' x y z) = filtCoef a b c x y z

toCoef (Lowshelf' a b c) (Lowshelf' x y z) = filtCoef a b c x y z

toCoef (Highshelf' a b c) (Highshelf' x y z) = filtCoef a b c x y z

toCoef (Peaking' a b c) (Peaking' x y z) = filtCoef a b c x y z

toCoef (Notch' a b c) (Notch' x y z) = filtCoef a b c x y z

toCoef (Allpass' a b c) (Allpass' x y z) = filtCoef a b c x y z

-- we use types in the function-level constructor to gate closeness
-- we still assess an absurd penalty just as a precaution
-- in case the sizes don't match
toCoef (Convolver' a) (Convolver' b) =
  if a /= b then
    10000.0
  else
    0.0

toCoef (DynamicConvolver' (AudioBuffer sr0 u0)) (DynamicConvolver' (AudioBuffer sr1 u1)) =
  if sr0 /= sr1 || (A.length u0) /= (A.length u1) then
    10000.0
  else
    -- to do, make this a weighted sum dependent on size...
    -- only will really matter if using convolvers of different lengths
    (foldl (+) 0.0 (join (A.zipWith (A.zipWith (\a b -> Math.abs $ a - b)) u0 u1)))

toCoef (DynamicsCompressor' (AudioParameter { param: a }) (AudioParameter { param: b }) (AudioParameter { param: c }) (AudioParameter { param: d }) (AudioParameter { param: e })) (DynamicsCompressor' (AudioParameter { param: v }) (AudioParameter { param: w }) (AudioParameter { param: x }) (AudioParameter { param: y }) (AudioParameter { param: z })) =
  foldl (+)
    0.0
    (zipWith (\i j -> Math.abs $ i - j) [ a, b, c, d, e ] [ v, w, x, y, z ])

toCoef (SawtoothOsc' f0) (SawtoothOsc' f1) = oscMULT * twoCoef f0 f1

toCoef (TriangleOsc' f0) (TriangleOsc' f1) = oscMULT * twoCoef f0 f1

-- todo : make periodic osc wavetable weighted?
toCoef (PeriodicOsc' f0 _) (PeriodicOsc' f1 _) = oscMULT * twoCoef f0 f1

toCoef (DynamicPeriodicOsc' f0 q x) (DynamicPeriodicOsc' f1 r y) =
  (oscMULT * twoCoef f0 f1)
    + foldl (+) 0.0 (A.zipWith (\a b -> Math.abs $ a - b) q r)
    + foldl (+) 0.0 (A.zipWith (\a b -> Math.abs $ a - b) x y)

toCoef (WaveShaper' l0 e0) (WaveShaper' l1 e1) =
  ( if e0 /= e1 then
      5.0
    else
      0.0
  )

toCoef (DynamicWaveShaper' l0 e0) (DynamicWaveShaper' l1 e1) =
  ( if e0 /= e1 then
      5.0
    else
      0.0
  )
    + foldl (+) 0.0 (A.zipWith (\a b -> Math.abs $ a - b) l0 l1)

toCoef (Dup') (Dup') = 0.0

toCoef (SinOsc' f0) (SinOsc' f1) = oscMULT * twoCoef f0 f1

toCoef (SquareOsc' f0) (SquareOsc' f1) = oscMULT * twoCoef f0 f1

toCoef (Splitter' _) (Splitter' _) = 0.0

toCoef (StereoPanner' p0) (StereoPanner' p1) = panMULT * twoCoef p0 p1

toCoef Mul' Mul' = 0.0

toCoef Add' Add' = 0.0

toCoef Swap' Swap' = 0.0

toCoef (Merger' _) (Merger' _) = 0.0

toCoef (Constant' c0) (Constant' c1) = constMULT * twoCoef c0 c1

toCoef (Delay' d0) (Delay' d1) = delayMULT * twoCoef d0 d1

toCoef (Gain' g0) (Gain' g1) = gainMULT * twoCoef g0 g1

toCoef Speaker' Speaker' = 0.0

toCoef NoSound' NoSound' = 0.0

toCoef (SplitRes' i0) (SplitRes' i1) = (Math.abs $ toNumber (i0 - i1)) * srMULT

toCoef DupRes' DupRes' = 0.0

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

makeNaiveReconciliation0 ::
  Reconcilable ->
  Reconcilable ->
  { prev :: Reconcilable, cur :: Reconcilable }
makeNaiveReconciliation0 prev cur =
  let
    cur_ = normalizeReconcilable prev cur
  in
    let
      prev_ = normalizeReconcilable cur prev
    in
      { prev: prev_
      , cur: cur_
      }

-- First step: fix
-- Second step: optimize
makeNaiveReconciliation1 ::
  { prev :: Reconcilable
  , cur :: Reconcilable
  } ->
  Reconciled'
makeNaiveReconciliation1 ipt =
  let
    cur_ = ipt.cur
  in
    let
      prev_ = ipt.prev
    in
      { prev: prev_
      , cur: cur_
      , reconciliation:
          M.fromFoldable
            ( join
                ( map
                    ( \(Tuple idx0 (NonEmpty x' x)) ->
                        ( maybe Nil
                            ( \(NonEmpty i' i) ->
                                DL.zipWith
                                  ( \a b ->
                                      Tuple a.ptr b.ptr
                                  )
                                  (i' : i)
                                  (x' : x)
                            )
                            (M.lookup idx0 prev_.grouped)
                        )
                    )
                    $ M.toUnfoldable cur_.grouped
                )
            )
      }

makeProgram ::
  Reconcilable ->
  Reconcilable ->
  { prev :: Reconcilable
  , cur :: Reconcilable
  , prog :: LinearProgram
  }
makeProgram prev cur =
  let
    cur_ = normalizeReconcilable prev cur
  in
    let
      prev_ = normalizeReconcilable cur prev
    in
      { prev: prev_, cur: cur_, prog: audioToLP prev_ cur_ }

-- | the base time to set at
-- | instructions
-- | audio context
-- | audio stream (ie microphone)
-- | audio sources
-- | audio units
-- | audio units
foreign import touchAudio ::
  forall (microphones :: # Type) (tracks :: # Type) (buffers :: # Type) (floatArrays :: # Type) microphoneT tracksT buffersT floatArraysT.
  Homogeneous microphones microphoneT =>
  Homogeneous tracks tracksT =>
  Homogeneous buffers buffersT =>
  Homogeneous floatArrays floatArraysT =>
  Number ->
  Array Instruction ->
  AudioContext ->
  AudioInfo (Record microphones) (Record tracks) (Record buffers) (Record floatArrays) ->
  Array Foreign ->
  Effect (Array Foreign)

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

type Reconciled'
  = { prev :: Reconcilable
    , cur :: Reconcilable
    , reconciliation :: Map Int Int
    }

type Reconciled
  = { prev :: Reconcilable
    , cur :: Reconcilable
    , reconciliation :: Map Int Int
    , instructionSet :: List Instruction
    }

-- "Assembly like" instruction
-- for sequential programming with audio units
-- treating them as pointers.
data Instruction
  = Stop Int
  | DisconnectFrom Int Int -- id id
  | ConnectTo Int Int (Maybe (Tuple Int Int)) -- id id channelConnections
  | Shuffle (Array (Tuple Int Int)) -- id id, shuffles the map
  | NewUnit Int AudioUnit'' (Maybe Int) (Maybe String) (Maybe Number) -- new audio unit, maybe with channel info, maybe with a source, maybe with a start time
  | SetFrequency Int Number Number -- frequency
  | SetThreshold Int Number Number -- threshold
  | SetKnee Int Number Number -- knee
  | SetRatio Int Number Number -- ratio
  | SetAttack Int Number Number -- attack
  | SetRelease Int Number Number -- release
  | SetBuffer Int Int (Array (Array Number)) -- buffer
  | SetQ Int Number Number -- q
  | SetPlaybackRate Int Number Number -- playback rate
  | SetPeriodicWave Int (Array Number) (Array Number) -- periodic wave
  | SetCurve Int (Array Number) -- curve
  | SetOversample Int String -- oversample
  | SetLoopStart Int Number -- loop start
  | SetLoopEnd Int Number -- loop end
  | SetPan Int Number Number -- pan for pan node
  | SetGain Int Number Number -- gain for gain node
  | SetDelay Int Number Number -- delay for delay node
  | SetOffset Int Number Number -- offset for const node

derive instance genericInstruction :: Generic Instruction _

instance showInstruction :: Show Instruction where
  show s = genericShow s

derive instance eqInstruction :: Eq Instruction

channelConstructor :: AudioUnit' -> Maybe Int
channelConstructor (Merger' l) = Just $ DL.length l

channelConstructor (Splitter' n) = Just n

channelConstructor _ = Nothing

sourceConstructor :: AudioUnit' -> Maybe String
sourceConstructor (Play' s _) = Just s

sourceConstructor (PlayBuf' s _) = Just s

sourceConstructor (LoopBuf' s _ _ _) = Just s

sourceConstructor (PeriodicOsc' _ s) = Just s

sourceConstructor (WaveShaper' s _) = Just s

sourceConstructor _ = Nothing

startConstructor :: AudioUnit' -> Maybe Number
startConstructor (Play' n timingHack) = Just timingHack

startConstructor (PlayBuf' _ n) = Just (apT n)

startConstructor (LoopBuf' _ n _ _) = Just (apT n)

startConstructor (PlayDynamicBuf' _ n) = Just (apT n)

startConstructor (LoopDynamicBuf' _ n _ _) = Just (apT n)

startConstructor (SawtoothOsc' n) = Just (apT n)

startConstructor (TriangleOsc' n) = Just (apT n)

startConstructor (PeriodicOsc' n _) = Just (apT n)

startConstructor (DynamicPeriodicOsc' n _ _) = Just (apT n)

startConstructor (SinOsc' n) = Just (apT n)

startConstructor (SquareOsc' n) = Just (apT n)

startConstructor (Constant' n) = Just (apT n)

startConstructor _ = Nothing

os2s :: Oversample -> String
os2s o = case o of
  None -> "none"
  TwoX -> "2x"
  FourX -> "4x"

napeq :: forall a. Eq a => AudioParameter a -> AudioParameter a -> Boolean
napeq (AudioParameter { param: a }) (AudioParameter { param: b }) = a /= b

describeConnection :: Reconcilable -> Reconcilable -> Map Int Int -> List (Tuple Int Int)
describeConnection start end passage =
  (DL.fromFoldable <<< M.keys)
    ( M.filter
        ( \(Tuple f s) ->
            fromMaybe false
              ((not <<< member s <<< M.keys <<< map (_ == 1) <<< _.next) <$> (M.lookup f end.flat))
        )
        ( M.fromFoldable
            ( DL.catMaybes
                ( map
                    ( \it@(Tuple f s) ->
                        Tuple it
                          <$> ( Tuple
                                <$> M.lookup f passage
                                <*> M.lookup s passage
                            )
                    )
                    $ join
                        ( map
                            ( \au ->
                                map (Tuple au.ptr)
                                  $ ( DL.fromFoldable
                                        <<< M.keys
                                        <<< M.filter (_ == 1)
                                    )
                                      au.next
                            )
                            (M.values start.flat)
                        )
                )
            )
        )
    )

isGen :: AudioUnit' -> Boolean
isGen (Microphone') = true

isGen (Play' _ _) = true

isGen (PlayBuf' _ _) = true

isGen (LoopBuf' _ _ _ _) = true

isGen (PlayDynamicBuf' _ _) = true

isGen (LoopDynamicBuf' _ _ _ _) = true

isGen (SawtoothOsc' _) = true

isGen (TriangleOsc' _) = true

isGen (PeriodicOsc' _ _) = true

isGen (DynamicPeriodicOsc' _ _ _) = true

isGen (SinOsc' _) = true

isGen (SquareOsc' _) = true

isGen (Constant' _) = true

isGen _ = false

reconciliationToInstructionSet :: Reconciled' -> Reconciled
reconciliationToInstructionSet { prev, cur, reconciliation } =
  { prev
  , cur
  , reconciliation
  , instructionSet: disconnect <> stop <> (pure shuffle) <> new <> connect <> set
  }
  where
  reversed =
    ( M.fromFoldable
        <<< map swap
        <<< (M.toUnfoldable :: Map Int Int -> Array (Tuple Int Int))
    )
      reconciliation

  reconciliationAsMap = reconciliation

  reversedAsMap = reversed

  -- disconnections that we need to make
  disconnect = (map (uncurry DisconnectFrom) $ describeConnection prev cur reconciliationAsMap)

  statusChange :: forall f. Unfoldable f ⇒ Maybe Status -> Maybe Status -> f (Tuple Int Int)
  statusChange isNow was =
    ( M.toUnfoldable
        $ M.filter
            ( \v ->
                ( map (_.status)
                    $ M.lookup v cur.flat
                )
                  == isNow
            )
            ( M.filterKeys
                (\k -> (map (_.status) $ M.lookup k prev.flat) == was)
                reconciliationAsMap
            )
    )

  -- turning off
  stop =
    map (Stop <<< fst)
      ( DL.filter
          (maybe false (isGen <<< _.au) <<< flip M.lookup prev.flat <<< fst)
          $ statusChange (Just Off) (Just On)
      )

  -- shuffle instructions represent the new array that we will make out of the old
  shuffle = Shuffle $ statusChange (Just On) (Just On)

  -- new units that were not in the old array
  new =
    map
      ((uncurry <<< uncurry <<< uncurry <<< uncurry) NewUnit)
      ( DL.catMaybes
          ( map
              ( \i ->
                  map
                    ( \ptr ->
                        ( Tuple
                            ( Tuple
                                (Tuple (Tuple i $ au'' ptr.au) (channelConstructor ptr.au))
                                (sourceConstructor ptr.au)
                            )
                            (startConstructor ptr.au)
                        )
                    )
                    $ M.lookup i cur.flat
              )
              ( DL.catMaybes
                  ( map
                      ( \k ->
                          M.lookup k reconciliationAsMap
                      )
                      $ (DL.fromFoldable <<< M.keys) (M.filter (\i -> i.status == Off) prev.flat)
                  )
              )
          )
      )

  harmonizeCurrChannels' :: PtrInfo -> PtrInfo -> Maybe (Tuple Int Int)
  harmonizeCurrChannels' _ { au: SplitRes' n } = Just (Tuple n 0)

  harmonizeCurrChannels' { ptr } { au: Merger' l } = DL.head $ map (Tuple 0 <<< fst) (DL.filter (\(Tuple chan pt) -> ptr == pt) $ DL.mapWithIndex (\i a -> Tuple i a) l)

  harmonizeCurrChannels' { chan } _ = Nothing

  harmonizeCurrChannels :: Tuple Int Int -> Maybe (Tuple Int Int)
  harmonizeCurrChannels (Tuple l r) =
    fromMaybe Nothing
      $ harmonizeCurrChannels'
      <$> (M.lookup l cur.flat)
      <*> (M.lookup r cur.flat)

  connect =
    let
      conn = describeConnection cur prev reversedAsMap
    in
      (map (uncurry $ uncurry ConnectTo) $ map (\i -> Tuple i (harmonizeCurrChannels i)) conn)

  setFilter i a b c x y z =
    [ if napeq a x then Just $ SetFrequency i (apP a) (apT a) else Nothing
    , if napeq b y then Just $ SetQ i (apP b) (apT b) else Nothing
    , if napeq c z then Just $ SetGain i (apP c) (apT c) else Nothing
    ]

  set' i (PlayBuf' _ n) (PlayBuf' _ nx) = pure $ if napeq n nx then Just $ SetPlaybackRate i (apP n) (apT n) else Nothing

  set' i (LoopBuf' _ n s e) (LoopBuf' _ nx sx ex) =
    [ if napeq n nx then Just $ SetPlaybackRate i (apP n) (apT n) else Nothing
    , if s /= sx then Just $ SetLoopStart i s else Nothing
    , if e /= ex then Just $ SetLoopEnd i e else Nothing
    ]

  set' i (PlayDynamicBuf' b@(AudioBuffer v a) n) (PlayDynamicBuf' bx nx) =
    [ if b /= bx then Just $ SetBuffer i v a else Nothing
    , if napeq n nx then Just $ SetPlaybackRate i (apP n) (apT n) else Nothing
    ]

  set' i (LoopDynamicBuf' b@(AudioBuffer v a) n s e) (LoopDynamicBuf' bx nx sx ex) =
    [ if b /= bx then Just $ SetBuffer i v a else Nothing
    , if napeq n nx then Just $ SetPlaybackRate i (apP n) (apT n) else Nothing
    , if s /= sx then Just $ SetLoopStart i s else Nothing
    , if e /= ex then Just $ SetLoopEnd i e else Nothing
    ]

  set' i (Lowpass' a b c) (Lowpass' x y z) = setFilter i a b c x y z

  set' i (Highpass' a b c) (Highpass' x y z) = setFilter i a b c x y z

  set' i (Bandpass' a b c) (Bandpass' x y z) = setFilter i a b c x y z

  set' i (Allpass' a b c) (Allpass' x y z) = setFilter i a b c x y z

  set' i (Highshelf' a b c) (Highshelf' x y z) = setFilter i a b c x y z

  set' i (Lowshelf' a b c) (Lowshelf' x y z) = setFilter i a b c x y z

  set' i (Peaking' a b c) (Peaking' x y z) = setFilter i a b c x y z

  set' i (Notch' a b c) (Notch' x y z) = setFilter i a b c x y z

  set' i (DynamicConvolver' b@(AudioBuffer v a)) (DynamicConvolver' bx) =
    pure
      $ if b /= bx then Just $ SetBuffer i v a else Nothing

  set' i (DynamicsCompressor' a b c d e) (DynamicsCompressor' v w x y z) =
    [ if napeq a v then Just $ SetThreshold i (apP a) (apT a) else Nothing
    , if napeq b w then Just $ SetKnee i (apP b) (apT b) else Nothing
    , if napeq c x then Just $ SetRatio i (apP c) (apT c) else Nothing
    , if napeq d y then Just $ SetAttack i (apP d) (apT d) else Nothing
    , if napeq e z then Just $ SetRelease i (apP e) (apT e) else Nothing
    ]

  set' i (SinOsc' n) (SinOsc' nx) = pure $ if napeq n nx then Just $ SetFrequency i (apP n) (apT n) else Nothing

  set' i (SquareOsc' n) (SquareOsc' nx) = pure $ if napeq n nx then Just $ SetFrequency i (apP n) (apT n) else Nothing

  set' i (SawtoothOsc' n) (SawtoothOsc' nx) = pure $ if napeq n nx then Just $ SetFrequency i (apP n) (apT n) else Nothing

  set' i (TriangleOsc' n) (TriangleOsc' nx) = pure $ if napeq n nx then Just $ SetFrequency i (apP n) (apT n) else Nothing

  set' i (PeriodicOsc' n _) (PeriodicOsc' nx _) = pure $ if napeq n nx then Just $ SetFrequency i (apP n) (apT n) else Nothing

  set' i (DynamicPeriodicOsc' n rl im) (DynamicPeriodicOsc' nx rlx imx) =
    [ if napeq n nx then Just $ SetFrequency i (apP n) (apT n) else Nothing
    , if rl /= rlx || im /= imx then Just $ SetPeriodicWave i rl im else Nothing
    ]

  set' i (WaveShaper' _ o) (WaveShaper' _ ox) =
    [ if o /= ox then
        Just
          ( SetOversample i
              $ os2s o
          )
      else
        Nothing
    ]

  set' i (DynamicWaveShaper' a o) (DynamicWaveShaper' ax ox) =
    [ if a /= ax then Just $ SetCurve i a else Nothing
    , if o /= ox then
        Just
          ( SetOversample i
              $ os2s o
          )
      else
        Nothing
    ]

  set' i (StereoPanner' n) (StereoPanner' nx) = pure $ if napeq n nx then Just $ SetPan i (apP n) (apT n) else Nothing

  set' i (Constant' n) (Constant' nx) = pure $ if napeq n nx then Just $ SetOffset i (apP n) (apT n) else Nothing

  set' i (Delay' n) (Delay' nx) = pure $ if napeq n nx then Just $ SetDelay i (apP n) (apT n) else Nothing

  set' i (Gain' n) (Gain' nx) = pure $ if napeq n nx then Just $ SetGain i (apP n) (apT n) else Nothing

  set' i _ _ = pure Nothing

  -- NOTE:
  -- roundtrip to array and back a little silly
  -- makes it easier to type [] in set', though...
  set =
    (DL.catMaybes <<< DL.fromFoldable)
      ( join
          ( map
              ( \v ->
                  set' v.ptr v.au
                    (fromMaybe v.au $ (map _.au $ M.lookup v.ptr reversedAsMap >>= flip M.lookup prev.flat))
              )
              $ (A.fromFoldable $ M.values cur.flat)
          )
      )

type AudioInfo microphones tracks buffers floatArrays
  = { microphones :: microphones
    , tracks :: tracks
    , buffers :: buffers
    , floatArrays :: floatArrays
    }

type VisualInfo
  = { canvases :: Object CanvasElement
    }

foreign import getAudioClockTime :: AudioContext -> Effect Number

foreign import unsafeCanvasHack :: (CanvasElement -> Effect Number) -> CanvasElement -> Effect Number

type CanvasInfo
  = { w :: Number, h :: Number }

class RunnableMedia a accumulator where
  runInBrowser ::
    forall (microphones :: # Type) (tracks :: # Type) (buffers :: # Type) (floatArrays :: # Type) microphoneT tracksT buffersT floatArraysT.
    Homogeneous microphones microphoneT =>
    Homogeneous tracks tracksT =>
    Homogeneous buffers buffersT =>
    Homogeneous floatArrays floatArraysT =>
    (accumulator -> CanvasInfo -> Number -> Behavior a) ->
    accumulator ->
    Int ->
    Int ->
    AudioContext ->
    AudioInfo (Record microphones) (Record tracks) (Record buffers) (Record floatArrays) ->
    VisualInfo ->
    Effect (Effect Unit)

data AV ch accumulator
  = AV (Maybe (AudioUnit ch)) (Maybe Drawing) accumulator

data Animation
  = Animation Drawing

data IAnimation accumulator
  = IAnimation Drawing accumulator

data IAudioUnit ch accumulator
  = IAudioUnit (AudioUnit ch) accumulator

getFirstCanvas :: Object CanvasElement -> Maybe CanvasElement
getFirstCanvas = map snd <<< A.head <<< O.toUnfoldable

instance soundscapeRunnableMedia :: Pos ch => RunnableMedia (AudioUnit ch) accumulator where
  runInBrowser f = runInBrowser (\z wh s -> map (\x -> AV (Just x) Nothing z) (f z wh s))

instance iSoundscapeRunnableMedia :: Pos ch => RunnableMedia (IAudioUnit ch accumulator) accumulator where
  runInBrowser f = runInBrowser (\z wh s -> map (\(IAudioUnit xa xz) -> AV (Just xa) Nothing xz) (f z wh s))

instance animationRunnable :: RunnableMedia Animation accumulator where
  runInBrowser f = runInBrowser (\z wh s -> map (\(Animation x) -> (AV (Nothing :: Maybe (AudioUnit D1)) (Just x) z)) (f z wh s))

instance iAnimationRunnable :: RunnableMedia (IAnimation accumulator) accumulator where
  runInBrowser f = runInBrowser (\z wh s -> map (\(IAnimation xv xz) -> (AV (Nothing :: Maybe (AudioUnit D1)) (Just xv) xz)) (f z wh s))

instance avRunnableMedia :: Pos ch => RunnableMedia (AV ch accumulator) accumulator where
  runInBrowser scene accumulator pingEvery actualSpeed ctx audioInfo visualInfo = do
    let
      __contract = toNumber $ pingEvery
    __accumulator <- new accumulator
    __totalFromStart <- new 0.0
    ciRef <- new 0
    __totalTillProgram <- new 0.0
    __totalProgram <- new 0.0
    __totalPostProgram <- new 0.0
    reconRef <-
      new
        { grouped: M.empty
        , flat: M.empty
        }
    let
      tOffset = 100
    clock <- new 0
    units <- new ([] :: Array Foreign)
    audioClockStart <- getAudioClockTime ctx
    bam <-
      subscribe
        (interval actualSpeed)
        ( const do
            ct <- read clock
            write (ct + pingEvery) clock
            acc_ <- getAudioClockTime ctx
            curIt <- read ciRef
            write (curIt + 1) ciRef
            clockNow_ <- read clock
            let
              startingPosWRT =
                ( (toNumber (clockNow_ + tOffset) / 1000.0)
                    - (acc_ - audioClockStart)
                )
            if (startingPosWRT > 0.15) then
              -- reset the clock
              ( do
                  let
                    newV = (clockNow_ - pingEvery)
                  -- log $ "Rewinding " <> show clockNow_ <> " " <> show newV <> " " <> show startingPosWRT
                  write newV clock
              )
            else do
              if (startingPosWRT < 0.025) then
                ( do
                    let
                      newV = clockNow_ + pingEvery
                    log $ "Fastforwarding " <> show clockNow_ <> " " <> show newV <> " " <> show startingPosWRT
                    write newV clock
                )
              else
                pure unit
              __startTime <- map getTime now
              _accNow <- read __accumulator
              __w <-
                fromMaybe (pure 0.0)
                  $ map
                      (unsafeCanvasHack getCanvasWidth)
                      (getFirstCanvas visualInfo.canvases)
              __h <-
                fromMaybe (pure 0.0)
                  $ map
                      (unsafeCanvasHack getCanvasHeight)
                      (getFirstCanvas visualInfo.canvases)
              let
                behavior = scene _accNow { w: __w, h: __h } (toNumber ct / 1000.0)
              bang <- create :: Effect (EventIO Unit)
              let
                behaviorSampled = sample_ behavior bang.event
              unsub <-
                subscribe behaviorSampled
                  ( \(AV ava avv avz) -> do
                      write avz __accumulator
                      maybe (pure unit)
                        ( \viz -> do
                            let
                              cvs_ = getFirstCanvas visualInfo.canvases
                            maybe
                              (pure unit)
                              ( \cvs -> do
                                  canvasCtx <- getContext2D cvs
                                  clearRect canvasCtx { height: __h, width: __w, x: 0.0, y: 0.0 }
                                  render canvasCtx viz
                              )
                              cvs_
                        )
                        avv
                      maybe (pure unit)
                        ( \aud -> do
                            let
                              i = audioToPtr aud
                            let
                              cur = { flat: i.flat, grouped: audioGrouper (DL.fromFoldable i.flat) }
                            prev <- read reconRef
                            write cur reconRef
                            let
                              prog' = makeNaiveReconciliation0 prev cur
                            let
                              prog = makeNaiveReconciliation1 prog'
                            let
                              instr = reconciliationToInstructionSet prog
                            audioClockCur <- getAudioClockTime ctx
                            let
                              instructions =
                                { t: clockNow_
                                , i: (A.fromFoldable instr.instructionSet)
                                }
                            uts <- read units
                            uts' <-
                              touchAudio
                                (audioClockStart + (toNumber (instructions.t + tOffset) / 1000.0))
                                instructions.i
                                ctx
                                audioInfo
                                uts
                            write uts' units
                            __endTime <- map getTime now
                            if (__endTime - __startTime) >= __contract then
                              log
                                ( "Audio control processing is too slow. It took this long: "
                                    <> show (__endTime - __startTime)
                                    <> " but it needs to take this long: "
                                    <> show __contract
                                )
                            else
                              pure unit
                            pure unit
                        )
                        ava
                  )
              bang.push unit
              unsub
        )
    pure bam

-- | The main executor loop in the browser
-- | Accepts an effectful scene
runInBrowser_ ::
  forall accumulator (microphones :: # Type) (tracks :: # Type) (buffers :: # Type) (floatArrays :: # Type) microphoneT tracksT buffersT floatArraysT a.
  Homogeneous microphones microphoneT =>
  Homogeneous tracks tracksT =>
  Homogeneous buffers buffersT =>
  Homogeneous floatArrays floatArraysT =>
  RunnableMedia a accumulator =>
  Effect (accumulator -> CanvasInfo -> Number -> Behavior a) ->
  accumulator ->
  Int ->
  Int ->
  AudioContext ->
  AudioInfo (Record microphones) (Record tracks) (Record buffers) (Record floatArrays) ->
  VisualInfo ->
  Effect (Effect Unit)
runInBrowser_ scene' accumulator pingEvery actualSpeed ctx audioInfo visualInfo = do
  scene <- scene'
  runInBrowser scene accumulator pingEvery actualSpeed ctx audioInfo visualInfo
