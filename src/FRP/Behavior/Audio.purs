-- | This documents the audio units exported by FRP.Behavior.Audio.
-- | The convention for audio units is the following:
-- |
-- | - highpass :: A highpass filter
-- | - highpassT :: A highpass filter using an AudioParameter, which has a temporal offset. 
-- | - highpass_ :: A named highpass filter.  Naming audio units speeds up computation a bit and may get rid of some artifacts.
-- | - highpassT_ :: A named highpass filter with a AudioParameters.
-- |
-- | All audio units have these four varieties.  Any audio unit that is not a generator takes one or many audio units as inputs.  In addition, some audio units (like `speaker` and `gain`) have a variety with an apostrophe (`speaker'` and `gain'`) that accept a single audio unit instead of a list.
module FRP.Behavior.Audio
  ( speaker
  , microphone
  , audioWorkletGenerator
  , audioWorkletProcessor
  , audioWorkletAggregator
  , play
  , playBuf
  , playBufWithOffset
  , loopBuf
  , iirFilter
  , lowpass
  , highpass
  , bandpass
  , lowshelf
  , highshelf
  , peaking
  , notch
  , allpass
  , convolver
  , dynamicsCompressor
  , dup1
  , dup2
  , dup3
  , dup4
  , dup5
  , waveShaper
  , decodeAudioDataFromUri
  , periodicOsc
  , sinOsc
  , sawtoothOsc
  , triangleOsc
  , squareOsc
  , split1
  , split2
  , split3
  , split4
  , split5
  , panner
  , pannerMono
  , spatialPanner
  , spatialPannerMono
  , mul
  , add
  , merger
  , constant
  , delay
  , gain
  , speaker_
  , microphone_
  , audioWorkletGenerator_
  , audioWorkletProcessor_
  , audioWorkletAggregator_
  , play_
  , playBuf_
  , playBufWithOffset_
  , loopBuf_
  , iirFilter_
  , lowpass_
  , highpass_
  , bandpass_
  , lowshelf_
  , highshelf_
  , peaking_
  , notch_
  , allpass_
  , convolver_
  , dynamicsCompressor_
  , dup1_
  , dup2_
  , dup3_
  , dup4_
  , dup5_
  , waveShaper_
  , periodicOsc_
  , sinOsc_
  , sawtoothOsc_
  , triangleOsc_
  , squareOsc_
  , split1_
  , split2_
  , split3_
  , split4_
  , split5_
  , panner_
  , pannerMono_
  , spatialPanner_
  , spatialPannerMono_
  , mul_
  , add_
  , merger_
  , constant_
  , delay_
  , gain_
  , audioWorkletGeneratorT
  , audioWorkletProcessorT
  , audioWorkletAggregatorT
  , playBufT
  , playBufWithOffsetT
  , loopBufT
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
  , sinOscT
  , sawtoothOscT
  , triangleOscT
  , squareOscT
  , pannerT
  , pannerMonoT
  , spatialPannerT
  , spatialPannerMonoT
  , constantT
  , delayT
  , gainT
  , audioWorkletGeneratorT_
  , audioWorkletProcessorT_
  , audioWorkletAggregatorT_
  , playBufT_
  , playBufWithOffsetT_
  , loopBufT_
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
  , sinOscT_
  , sawtoothOscT_
  , triangleOscT_
  , squareOscT_
  , pannerT_
  , pannerMonoT_
  , spatialPannerT_
  , spatialPannerMonoT_
  , constantT_
  , delayT_
  , gainT_
  , speaker'
  , gain'
  , gainT'
  , gain_'
  , gainT_'
  , graph
  , graph_
  , class AsProcessorObject
  , class ProcessorsToGraph
  , class LookupAggregators
  , class AudioGraphGenerators
  , class GeneratorsToGraphInternal
  , class LookupProcessors
  , class HasOneGenerator
  , class LookUpProcessorsInternal
  , class RecordNotEmptyInternal
  , class ProcessorsToGraphInternal
  , class AudioGraphToGraph
  , class AggregatorsToGraphInternal
  , class LookUpAggregatorsInternal
  , class LookUpGeneratorsInternal
  , class AggregatorsToGraph
  , class LookupGenerators
  , class AudioGraphProcessors
  , class AsAggregatorObject
  , class AudioGraphAggregators
  , class IsValidAudioGraph
  , class ValidAudioGraph
  , class HasOneGeneratorInternal
  , class AudioGraphToObject
  , class GeneratorsToGraph
  , class ReflectSymbols
  , class AsProcessor
  , class AsAggregator
  , class RecordHomogeneousInAudioUnits
  , toObject
  , asProcessor
  , asProcessorObject
  , asAggregator
  , asAggregatorObject
  , generators
  , processors
  , aggregators
  , reflectSymbols
  , g'dynamicsCompressorT_
  , g'iirFilter_
  , g'lowpass_
  , g'lowpassT_
  , g'peakingT_
  , g'notchT
  , g'convolver_
  , g'delayT
  , g'highpassT
  , g'lowshelf
  , g'notchT_
  , g'panner
  , g'spatialPanner
  , g'gain
  , g'panner_
  , g'spatialPanner_
  , g'bandpassT
  , g'delay_
  , g'bandpass_
  , g'dynamicsCompressorT
  , g'mul_
  , g'gain_
  , g'audioWorkletAggregator
  , g'audioWorkletAggregatorT
  , g'audioWorkletAggregator_
  , g'audioWorkletAggregatorT_
  , g'audioWorkletProcessorT
  , g'audioWorkletProcessorT_
  , g'peaking
  , g'pannerT_
  , g'spatialPannerT_
  , g'audioWorkletProcessor_
  , g'delayT_
  , g'iirFilter
  , g'lowpass
  , g'bandpass
  , g'notch_
  , g'allpass
  , g'add_
  , g'dynamicsCompressor
  , g'gainT
  , g'highpass_
  , g'lowshelfT
  , g'peaking_
  , g'allpassT_
  , g'highshelf
  , g'audioWorkletProcessor
  , g'mul
  , g'pannerT
  , g'spatialPannerT
  , g'lowshelf_
  , g'bandpassT_
  , g'lowshelfT_
  , g'allpassT
  , g'convolver
  , g'highshelfT_
  , g'notch
  , g'gainT_
  , g'dynamicsCompressor_
  , g'waveShaper
  , g'highpass
  , g'lowpassT
  , g'allpass_
  , g'delay
  , g'add
  , g'highshelfT
  , g'highpassT_
  , g'peakingT
  , g'waveShaper_
  , g'highshelf_
  , defaultExporter
  , audioGrouper
  , pannerVars
  , pannerVars'
  , PannerVars
  , PannerVars'
  , AudioUnit'(..)
  , SampleFrame
  , AudioProcessor
  , AudioParameter(..)
  , AudioBuffer
  , AudioUnit
  , AudioContext
  , CanvasInfo(..)
  , CanvasInfo'
  , EngineInfo
  , AudioInfo
  , VisualInfo
  , BrowserPeriodicWave
  , BrowserAudioTrack
  , BrowserAudioBuffer
  , BrowserFloatArray
  , Exporter
  , BuildingBlocks
  , RunInBrowser
  , RunInBrowser_
  , RunInBrowserAudioUnit
  , RunInBrowserAudioUnit_
  , RunInBrowserIAudioUnit
  , RunInBrowserIAudioUnit_
  , RunInBrowserIAnimation
  , RunInBrowserIAnimation_
  , RunInBrowserAV
  , RunInBrowserAV_
  , AV(..)
  , Animation(..)
  , IAudioUnit(..)
  , IAnimation(..)
  , DistanceModel(..)
  , PanningModel(..)
  , AudioGraph
  , AudioGraphProcessor
  , AudioGraphAggregator
  , class RunnableMedia
  , makePeriodicWave
  , reconciliationToInstructionSet
  , touchAudio
  , objectToMapping
  , runInBrowser
  , runInBrowser_
  , audioBuffer
  , Oversample(..)
  , FFIPredicates
  , audioWorkletAddModule
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
  ) where

import Prelude
import Control.Bind (bindFlipped)
import Control.Promise (Promise)
import Data.Array (catMaybes, fold, foldl, head, index, length, mapWithIndex, range, replicate, snoc, takeEnd, zipWith, (!!))
import Data.Array as A
import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (floor, toNumber)
import Data.Int.Parse (parseInt, toRadix)
import Data.JSDate (getTime, now)
import Data.List (List(..), fromFoldable, partition, (:))
import Data.List as DL
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.NonEmpty as NE
import Data.Set (Set, member)
import Data.Set as DS
import Data.String (Pattern(..), split, take)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd, swap, uncurry)
import Data.Typelevel.Num (class LtEq, class Pos, D1, D2, D3, D4, D5, D20, toInt')
import Data.Unfoldable (class Unfoldable)
import Data.Unfoldable1 as DU
import Data.Vec (Vec, fill)
import Data.Vec as V
import Effect (Effect, whileE)
import Effect.Aff (Aff, joinFiber, launchAff, launchAff_)
import Effect.Class.Console (log)
import Effect.Exception (try)
import Effect.Ref (modify_, new, read, write)
import FRP.Behavior (ABehavior, Behavior, behavior, sample_)
import FRP.Event (Event, EventIO, create, makeEvent, subscribe)
import FRP.Event.Time (interval)
import Foreign (Foreign)
import Foreign.Object (Object, filterWithKey)
import Foreign.Object as O
import Graphics.Canvas (CanvasElement, Rectangle, clearRect, getCanvasHeight, getCanvasWidth, getContext2D)
import Graphics.Drawing (Drawing, render)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Boolean (False, True, kind Boolean)
import Prim.Row (class Union)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Prim.Symbol (class Compare)
import Prim.TypeError (class Fail, Text)
import Record (merge)
import Record.Extra (SLProxy(..), SNil, kind SList)
import Record.Unsafe (unsafeGet)
import Type.Data.Boolean (class And, class Not)
import Type.Data.Graph (class FlipDirection, class HasDuplicateEdges, class HasDuplicateNodes, class HasOrphanNodes, class HasUniqueTerminus, class IsConnected, class IsEq, type (:/))
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Type.RowList (class ListToRow, RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data BrowserPeriodicWave :: Type

foreign import data BrowserAudioBuffer :: Type

foreign import data BrowserFloatArray :: Type

foreign import data BrowserAudioTrack :: Type

foreign import data AudioContext :: Type

foreign import decodeAudioDataFromUri :: AudioContext -> String -> Effect (Promise BrowserAudioBuffer)

foreign import decodeAudioDataFromBase64EncodedString :: AudioContext -> String -> Effect (Promise BrowserAudioBuffer)

foreign import audioWorkletAddModule :: AudioContext -> String -> Effect (Promise Unit)

foreign import makeAudioContext :: Effect AudioContext

foreign import makePeriodicWaveImpl :: AudioContext -> Array Number -> Array Number -> Effect BrowserPeriodicWave

foreign import makeAudioTrack :: String -> Effect BrowserAudioTrack

foreign import makeAudioBuffer :: AudioContext -> AudioBuffer -> Effect BrowserAudioBuffer

foreign import makeFloatArray :: Array Number -> Effect BrowserFloatArray

foreign import getBoundingClientRect :: CanvasElement -> Effect Rectangle

makePeriodicWave ::
  forall len.
  Pos len =>
  AudioContext ->
  Vec len Number ->
  Vec len Number ->
  Effect BrowserPeriodicWave
makePeriodicWave ctx a b = makePeriodicWaveImpl ctx (V.toArray a) (V.toArray b)

audioBuffer ::
  forall bch blen.
  Pos bch =>
  Pos blen =>
  Int ->
  Vec bch (Vec blen Number) ->
  AudioBuffer
audioBuffer i v = AudioBuffer i (map V.toArray $ V.toArray v)

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
    , next :: Set Int
    }

type PtrInfo
  = { ptr :: Int
    , chan :: Int
    , prev :: Set Int
    , next :: Set Int
    , head :: Int
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

data DistanceModel
  = Linear
  | Inverse
  | Exponential

dm2str :: DistanceModel -> String
dm2str Linear = "linear"

dm2str Inverse = "inverse"

dm2str Exponential = "exponential"

derive instance genericDistanceModel :: Generic DistanceModel _

instance showDistanceModel :: Show DistanceModel where
  show s = genericShow s

derive instance eqDistanceModel :: Eq DistanceModel

data PanningModel
  = EqualPower
  | HRTF

pm2str :: PanningModel -> String
pm2str EqualPower = "equalpower"

pm2str HRTF = "HRTF"

derive instance genericPanningModel :: Generic PanningModel _

instance showPanningModel :: Show PanningModel where
  show s = genericShow s

derive instance eqPanningModel :: Eq PanningModel

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

type PannerVars
  = { coneInnerAngle :: (AudioParameter Number)
    , coneOuterAngle :: (AudioParameter Number)
    , coneOuterGain :: (AudioParameter Number)
    , distanceModel :: DistanceModel
    , maxDistance :: (AudioParameter Number)
    , orientationX :: (AudioParameter Number)
    , orientationY :: (AudioParameter Number)
    , orientationZ :: (AudioParameter Number)
    , panningModel :: PanningModel
    , positionX :: (AudioParameter Number)
    , positionY :: (AudioParameter Number)
    , positionZ :: (AudioParameter Number)
    , refDistance :: (AudioParameter Number)
    , rolloffFactor :: (AudioParameter Number)
    }

type PannerVars'
  = { coneInnerAngle :: Number
    , coneOuterAngle :: Number
    , coneOuterGain :: Number
    , distanceModel :: DistanceModel
    , maxDistance :: Number
    , orientationX :: Number
    , orientationY :: Number
    , orientationZ :: Number
    , panningModel :: PanningModel
    , positionX :: Number
    , positionY :: Number
    , positionZ :: Number
    , refDistance :: Number
    , rolloffFactor :: Number
    }

pannerVars' :: PannerVars'
pannerVars' =
  { coneInnerAngle: 360.0
  , coneOuterAngle: 360.0
  , coneOuterGain: 0.0
  , distanceModel: Inverse
  , maxDistance: 10000.0
  , orientationX: 1.0
  , orientationY: 0.0
  , orientationZ: 0.0
  , panningModel: EqualPower
  , positionX: 0.0
  , positionY: 0.0
  , positionZ: 0.0
  , refDistance: 1.0
  , rolloffFactor: 1.0
  }

pannerVars :: PannerVars
pannerVars = pannerVarsAsAudioParams pannerVars'

data AudioGraphProcessor
  = GAudioWorkletProcessor MString String (Object (AudioParameter Number))
  | GIIRFilter MString (Array Number) (Array Number)
  | GLowpass MString (AudioParameter Number) (AudioParameter Number)
  | GHighpass MString (AudioParameter Number) (AudioParameter Number)
  | GBandpass MString (AudioParameter Number) (AudioParameter Number)
  | GLowshelf MString (AudioParameter Number) (AudioParameter Number)
  | GHighshelf MString (AudioParameter Number) (AudioParameter Number)
  | GPeaking MString (AudioParameter Number) (AudioParameter Number) (AudioParameter Number)
  | GNotch MString (AudioParameter Number) (AudioParameter Number)
  | GAllpass MString (AudioParameter Number) (AudioParameter Number)
  | GConvolver MString String
  | GDynamicsCompressor MString (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioParameter Number)
  | GWaveShaper MString String Oversample
  | GStereoPanner MString (AudioParameter Number)
  | GPanner MString PannerVars
  | GDelay MString (AudioParameter Number)

data AudioGraphAggregator
  = GAudioWorkletAggregator MString String (Object (AudioParameter Number))
  | GMul MString
  | GAdd MString
  | GGain MString (AudioParameter Number)

type AudioGraph ch
  = { generators :: O.Object (AudioUnit ch)
    , processors :: O.Object (Tuple (AudioGraphProcessor) String)
    , aggregators :: O.Object (Tuple (AudioGraphAggregator) (Set String))
    }

data AudioUnit ch
  = Microphone MString
  | AudioWorkletGenerator MString String (Object (AudioParameter Number))
  | AudioWorkletProcessor MString String (Object (AudioParameter Number)) (AudioUnit ch)
  | AudioWorkletAggregator MString String (Object (AudioParameter Number)) (NonEmpty List (AudioUnit ch))
  | Play MString String Number
  | PlayBuf MString String (AudioParameter Number) (AudioParameter Number)
  | LoopBuf MString String (AudioParameter Number) Number Number
  | IIRFilter MString (Array Number) (Array Number) (AudioUnit ch)
  | Lowpass MString (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Highpass MString (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Bandpass MString (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Lowshelf MString (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Highshelf MString (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Peaking MString (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Notch MString (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Allpass MString (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | Convolver MString String (AudioUnit ch)
  | DynamicsCompressor MString (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioUnit ch)
  | SawtoothOsc MString (AudioParameter Number)
  | TriangleOsc MString (AudioParameter Number)
  | PeriodicOsc MString (AudioParameter Number) String
  | WaveShaper MString String Oversample (AudioUnit ch)
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
  | Panner MString PannerVars (AudioUnit ch)
  | StereoPanner MString (AudioParameter Number) (AudioUnit ch)
  | Mul MString (NonEmpty List (AudioUnit ch))
  | Add MString (NonEmpty List (AudioUnit ch))
  | Merger MString (Vec ch (AudioUnit D1))
  | Constant MString (AudioParameter Number)
  | Delay MString (AudioParameter Number) (AudioUnit ch)
  | Gain MString (AudioParameter Number) (NonEmpty List (AudioUnit ch))
  | Speaker MString (NonEmpty List (AudioUnit ch))
  | NoSound MString
  | Graph MString (AudioGraph ch)
  | SplitRes Int
  | DupRes

data AudioUnit'
  = Microphone'
  | AudioWorkletGenerator' String (Object (AudioParameter Number))
  | AudioWorkletProcessor' String (Object (AudioParameter Number))
  | AudioWorkletAggregator' String (Object (AudioParameter Number))
  | Play' String Number
  | PlayBuf' String (AudioParameter Number) (AudioParameter Number)
  | LoopBuf' String (AudioParameter Number) Number Number
  | IIRFilter' (Array Number) (Array Number)
  | Lowpass' (AudioParameter Number) (AudioParameter Number)
  | Highpass' (AudioParameter Number) (AudioParameter Number)
  | Bandpass' (AudioParameter Number) (AudioParameter Number)
  | Lowshelf' (AudioParameter Number) (AudioParameter Number)
  | Highshelf' (AudioParameter Number) (AudioParameter Number)
  | Peaking' (AudioParameter Number) (AudioParameter Number) (AudioParameter Number)
  | Notch' (AudioParameter Number) (AudioParameter Number)
  | Allpass' (AudioParameter Number) (AudioParameter Number)
  | Convolver' String
  | DynamicsCompressor' (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioParameter Number) (AudioParameter Number)
  | SawtoothOsc' (AudioParameter Number)
  | TriangleOsc' (AudioParameter Number)
  | PeriodicOsc' (AudioParameter Number) String
  | WaveShaper' String Oversample
  | Dup'
  | SinOsc' (AudioParameter Number)
  | SquareOsc' (AudioParameter Number)
  | Splitter' Int
  | StereoPanner' (AudioParameter Number)
  | Panner'
    { coneInnerAngle :: (AudioParameter Number)
    , coneOuterAngle :: (AudioParameter Number)
    , coneOuterGain :: (AudioParameter Number)
    , distanceModel :: DistanceModel
    , maxDistance :: (AudioParameter Number)
    , orientationX :: (AudioParameter Number)
    , orientationY :: (AudioParameter Number)
    , orientationZ :: (AudioParameter Number)
    , panningModel :: PanningModel
    , positionX :: (AudioParameter Number)
    , positionY :: (AudioParameter Number)
    , positionZ :: (AudioParameter Number)
    , refDistance :: (AudioParameter Number)
    , rolloffFactor :: (AudioParameter Number)
    }
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

isMicrophone_ :: AudioUnit'' -> Boolean
isMicrophone_ Microphone'' = true

isMicrophone_ _ = false

isAudioWorkletGenerator_ :: AudioUnit'' -> Boolean
isAudioWorkletGenerator_ AudioWorkletGenerator'' = true

isAudioWorkletGenerator_ _ = false

isAudioWorkletProcessor_ :: AudioUnit'' -> Boolean
isAudioWorkletProcessor_ AudioWorkletProcessor'' = true

isAudioWorkletProcessor_ _ = false

isAudioWorkletAggregator_ :: AudioUnit'' -> Boolean
isAudioWorkletAggregator_ AudioWorkletAggregator'' = true

isAudioWorkletAggregator_ _ = false

isPlay_ :: AudioUnit'' -> Boolean
isPlay_ Play'' = true

isPlay_ _ = false

isPlayBuf_ :: AudioUnit'' -> Boolean
isPlayBuf_ PlayBuf'' = true

isPlayBuf_ _ = false

isLoopBuf_ :: AudioUnit'' -> Boolean
isLoopBuf_ LoopBuf'' = true

isLoopBuf_ _ = false

isIIRFilter_ :: AudioUnit'' -> Boolean
isIIRFilter_ IIRFilter'' = true

isIIRFilter_ _ = false

isLowpass_ :: AudioUnit'' -> Boolean
isLowpass_ Lowpass'' = true

isLowpass_ _ = false

isHighpass_ :: AudioUnit'' -> Boolean
isHighpass_ Highpass'' = true

isHighpass_ _ = false

isBandpass_ :: AudioUnit'' -> Boolean
isBandpass_ Bandpass'' = true

isBandpass_ _ = false

isLowshelf_ :: AudioUnit'' -> Boolean
isLowshelf_ Lowshelf'' = true

isLowshelf_ _ = false

isHighshelf_ :: AudioUnit'' -> Boolean
isHighshelf_ Highshelf'' = true

isHighshelf_ _ = false

isPeaking_ :: AudioUnit'' -> Boolean
isPeaking_ Peaking'' = true

isPeaking_ _ = false

isNotch_ :: AudioUnit'' -> Boolean
isNotch_ Notch'' = true

isNotch_ _ = false

isAllpass_ :: AudioUnit'' -> Boolean
isAllpass_ Allpass'' = true

isAllpass_ _ = false

isConvolver_ :: AudioUnit'' -> Boolean
isConvolver_ Convolver'' = true

isConvolver_ _ = false

isDynamicsCompressor_ :: AudioUnit'' -> Boolean
isDynamicsCompressor_ DynamicsCompressor'' = true

isDynamicsCompressor_ _ = false

isSawtoothOsc_ :: AudioUnit'' -> Boolean
isSawtoothOsc_ SawtoothOsc'' = true

isSawtoothOsc_ _ = false

isTriangleOsc_ :: AudioUnit'' -> Boolean
isTriangleOsc_ TriangleOsc'' = true

isTriangleOsc_ _ = false

isPeriodicOsc_ :: AudioUnit'' -> Boolean
isPeriodicOsc_ PeriodicOsc'' = true

isPeriodicOsc_ _ = false

isWaveShaper_ :: AudioUnit'' -> Boolean
isWaveShaper_ WaveShaper'' = true

isWaveShaper_ _ = false

isDup_ :: AudioUnit'' -> Boolean
isDup_ Dup'' = true

isDup_ _ = false

isSinOsc_ :: AudioUnit'' -> Boolean
isSinOsc_ SinOsc'' = true

isSinOsc_ _ = false

isSquareOsc_ :: AudioUnit'' -> Boolean
isSquareOsc_ SquareOsc'' = true

isSquareOsc_ _ = false

isSplitter_ :: AudioUnit'' -> Boolean
isSplitter_ Splitter'' = true

isSplitter_ _ = false

isStereoPanner_ :: AudioUnit'' -> Boolean
isStereoPanner_ StereoPanner'' = true

isStereoPanner_ _ = false

isPanner_ :: AudioUnit'' -> Boolean
isPanner_ Panner'' = true

isPanner_ _ = false

isMul_ :: AudioUnit'' -> Boolean
isMul_ Mul'' = true

isMul_ _ = false

isAdd_ :: AudioUnit'' -> Boolean
isAdd_ Add'' = true

isAdd_ _ = false

isSwap_ :: AudioUnit'' -> Boolean
isSwap_ Swap'' = true

isSwap_ _ = false

isMerger_ :: AudioUnit'' -> Boolean
isMerger_ Merger'' = true

isMerger_ _ = false

isConstant_ :: AudioUnit'' -> Boolean
isConstant_ Constant'' = true

isConstant_ _ = false

isDelay_ :: AudioUnit'' -> Boolean
isDelay_ Delay'' = true

isDelay_ _ = false

isGain_ :: AudioUnit'' -> Boolean
isGain_ Gain'' = true

isGain_ _ = false

isSpeaker_ :: AudioUnit'' -> Boolean
isSpeaker_ Speaker'' = true

isSpeaker_ _ = false

isNoSound_ :: AudioUnit'' -> Boolean
isNoSound_ NoSound'' = true

isNoSound_ _ = false

isSplitRes_ :: AudioUnit'' -> Boolean
isSplitRes_ SplitRes'' = true

isSplitRes_ _ = false

isDupRes_ :: AudioUnit'' -> Boolean
isDupRes_ DupRes'' = true

isDupRes_ _ = false

data AudioUnit''
  = Microphone''
  | AudioWorkletGenerator''
  | AudioWorkletProcessor''
  | AudioWorkletAggregator''
  | Play''
  | PlayBuf''
  | LoopBuf''
  | IIRFilter''
  | Lowpass''
  | Highpass''
  | Bandpass''
  | Lowshelf''
  | Highshelf''
  | Peaking''
  | Notch''
  | Allpass''
  | Convolver''
  | DynamicsCompressor''
  | SawtoothOsc''
  | TriangleOsc''
  | PeriodicOsc''
  | WaveShaper''
  | Dup''
  | SinOsc''
  | SquareOsc''
  | Splitter''
  | StereoPanner''
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
  | DupRes''

derive instance genericAudioUnit'' :: Generic AudioUnit'' _

instance showAudioUnit'' :: Show AudioUnit'' where
  show s = genericShow s

derive instance eqAudioUnit'' :: Eq AudioUnit''

instance ordAudioUnit'' :: Ord AudioUnit'' where
  compare a b = compare (show a) (show b)

aga2au' :: AudioGraphAggregator -> { au :: AudioUnit', name :: MString }
aga2au' (GAudioWorkletAggregator name unit params) = { au: AudioWorkletAggregator' unit params, name }

aga2au' (GMul name) = { au: Mul', name }

aga2au' (GAdd name) = { au: Add', name }

aga2au' (GGain name n) = { au: (Gain' n), name }

agp2au' :: AudioGraphProcessor -> { au :: AudioUnit', name :: MString }
agp2au' (GAudioWorkletProcessor name unit params) = { au: AudioWorkletProcessor' unit params, name }

agp2au' (GLowpass name f q) = { au: Lowpass' f q, name }

agp2au' (GIIRFilter name ff fb) = { au: IIRFilter' ff fb, name }

agp2au' (GHighpass name f q) = { au: Highpass' f q, name }

agp2au' (GBandpass name f q) = { au: Bandpass' f q, name }

agp2au' (GLowshelf name f g) = { au: Lowshelf' f g, name }

agp2au' (GHighshelf name f g) = { au: Highshelf' f g, name }

agp2au' (GPeaking name f q g) = { au: Peaking' f q g, name }

agp2au' (GNotch name f q) = { au: Notch' f q, name }

agp2au' (GDelay name v) = { au: Delay' v, name }

agp2au' (GAllpass name f q) = { au: Allpass' f q, name }

agp2au' (GConvolver name buf) = { au: Convolver' buf, name }

agp2au' (GDynamicsCompressor name thresh knee ratio attack release) =
  { au: DynamicsCompressor' thresh knee ratio attack release
  , name
  }

agp2au' (GWaveShaper name curve os) = { au: (WaveShaper' curve os), name }

agp2au' (GStereoPanner name n) = { au: (StereoPanner' n), name }

agp2au' (GPanner name vars) = { au: (Panner' vars), name }

au' :: forall ch. Pos ch => AudioUnit ch -> { au :: AudioUnit', name :: MString }
au' (Microphone name) = { au: Microphone', name }

au' (AudioWorkletGenerator name unit params) = { au: AudioWorkletGenerator' unit params, name }

au' (AudioWorkletProcessor name unit params _) = { au: AudioWorkletProcessor' unit params, name }

au' (AudioWorkletAggregator name unit params _) = { au: AudioWorkletAggregator' unit params, name }

au' (Play name file timingHack) = { au: Play' file timingHack, name }

au' (PlayBuf name buf speed offsetInBuffer) = { au: PlayBuf' buf speed offsetInBuffer, name }

au' (LoopBuf name buf speed start end) = { au: LoopBuf' buf speed start end, name }

au' (IIRFilter name ff fb _) = { au: IIRFilter' ff fb, name }

au' (Lowpass name f q _) = { au: Lowpass' f q, name }

au' (Highpass name f q _) = { au: Highpass' f q, name }

au' (Bandpass name f q _) = { au: Bandpass' f q, name }

au' (Lowshelf name f g _) = { au: Lowshelf' f g, name }

au' (Highshelf name f g _) = { au: Highshelf' f g, name }

au' (Peaking name f q g _) = { au: Peaking' f q g, name }

au' (Notch name f q _) = { au: Notch' f q, name }

au' (Allpass name f q _) = { au: Allpass' f q, name }

au' (Convolver name buf _) = { au: Convolver' buf, name }

au' (DynamicsCompressor name thresh knee ratio attack release _) =
  { au: DynamicsCompressor' thresh knee ratio attack release
  , name
  }

au' (SawtoothOsc name n) = { au: (SawtoothOsc' n), name }

au' (TriangleOsc name n) = { au: (TriangleOsc' n), name }

au' (PeriodicOsc name n s) = { au: (PeriodicOsc' n s), name }

au' (WaveShaper name curve os _) = { au: (WaveShaper' curve os), name }

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

au' (Panner name vars _) = { au: (Panner' vars), name }

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

------------ we only use this for the name
----------- a code smell that it even needs to exist
---------- perhaps the AudioUnit structure & pattern match
--------- has outgrown its usefulness...
au' (Graph name _) = { au: NoSound', name: name }

au' (SplitRes n) = { au: (SplitRes' n), name: Nothing }

au' DupRes = { au: DupRes', name: Nothing }

au'' :: AudioUnit' -> AudioUnit''
au'' Microphone' = Microphone''

au'' (AudioWorkletGenerator' _ _) = AudioWorkletGenerator''

au'' (AudioWorkletProcessor' _ _) = AudioWorkletProcessor''

au'' (AudioWorkletAggregator' _ _) = AudioWorkletAggregator''

au'' (Play' _ _) = Play''

au'' (PlayBuf' _ _ _) = PlayBuf''

au'' (LoopBuf' _ _ _ _) = LoopBuf''

au'' (IIRFilter' _ _) = IIRFilter''

au'' (Lowpass' _ _) = Lowpass''

au'' (Highpass' _ _) = Highpass''

au'' (Bandpass' _ _) = Bandpass''

au'' (Lowshelf' _ _) = Lowshelf''

au'' (Highshelf' _ _) = Highshelf''

au'' (Peaking' _ _ _) = Peaking''

au'' (Notch' _ _) = Notch''

au'' (Allpass' _ _) = Allpass''

au'' (Convolver' _) = Convolver''

au'' (DynamicsCompressor' _ _ _ _ _) = DynamicsCompressor''

au'' (SawtoothOsc' _) = SawtoothOsc''

au'' (TriangleOsc' _) = TriangleOsc''

au'' (PeriodicOsc' _ _) = PeriodicOsc''

au'' (WaveShaper' _ _) = WaveShaper''

au'' Dup' = Dup''

au'' (SinOsc' _) = SinOsc''

au'' (SquareOsc' _) = SquareOsc''

au'' (Splitter' _) = Splitter''

au'' (StereoPanner' _) = StereoPanner''

au'' (Panner' _) = Panner''

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

tagToAU AudioWorkletGenerator'' = AudioWorkletGenerator' "" O.empty

tagToAU AudioWorkletProcessor'' = AudioWorkletProcessor' "" O.empty

tagToAU AudioWorkletAggregator'' = AudioWorkletAggregator' "" O.empty

tagToAU PlayBuf'' = PlayBuf' "" (ap_ (-1.0)) (ap_ (-1.0))

tagToAU LoopBuf'' = LoopBuf' "" (ap_ (-1.0)) (-1.0) (-1.0)

tagToAU IIRFilter'' = IIRFilter' [] []

tagToAU Lowpass'' = Lowpass' (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Highpass'' = Highpass' (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Bandpass'' = Bandpass' (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Lowshelf'' = Lowshelf' (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Highshelf'' = Highshelf' (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Peaking'' = Peaking' (ap_ (-1.0)) (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Notch'' = Notch' (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Allpass'' = Allpass' (ap_ (-1.0)) (ap_ (-1.0))

tagToAU Convolver'' = Convolver' ""

tagToAU DynamicsCompressor'' = DynamicsCompressor' (ap_ (-1.0)) (ap_ (-1.0)) (ap_ (-1.0)) (ap_ (-1.0)) (ap_ (-1.0))

tagToAU SawtoothOsc'' = SawtoothOsc' (ap_ 50000.0)

tagToAU TriangleOsc'' = TriangleOsc' (ap_ 50000.0)

tagToAU PeriodicOsc'' = PeriodicOsc' (ap_ 50000.0) ""

tagToAU WaveShaper'' = WaveShaper' "" None

tagToAU Dup'' = Dup'

tagToAU SinOsc'' = SinOsc' (ap_ 50000.0)

tagToAU SquareOsc'' = SquareOsc' (ap_ 50000.0)

tagToAU Splitter'' = Splitter' (-1)

tagToAU Panner'' =
  Panner'
    { coneInnerAngle: (ap_ (-3.0))
    , coneOuterAngle: (ap_ (-3.0))
    , coneOuterGain: (ap_ (-3.0))
    , distanceModel: Inverse
    , maxDistance: (ap_ (-3.0))
    , orientationX: (ap_ (-3.0))
    , orientationY: (ap_ (-3.0))
    , orientationZ: (ap_ (-3.0))
    , panningModel: EqualPower
    , positionX: (ap_ (-3.0))
    , positionY: (ap_ (-3.0))
    , positionZ: (ap_ (-3.0))
    , refDistance: (ap_ (-3.0))
    , rolloffFactor: (ap_ (-3.0))
    }

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

dupResGetImpetus :: ∀ t8245 t8248 t8263 t8266 t8267. Ord t8245 ⇒ Ord t8248 ⇒ { flat ∷ Map t8248 { au :: AudioUnit', prev :: Set t8263, ptr :: t8245 | t8267 } | t8266 } → Set t8245
dupResGetImpetus s =
  DS.fromFoldable
    $ M.values
        ( map _.ptr
            $ M.filter
                (\i -> i.au == DupRes' && DS.size i.prev == 0)
                s.flat
        )

isSplitRes' :: AudioUnit' -> Boolean
isSplitRes' (SplitRes' _) = true

isSplitRes' _ = false

splitResGetImpetus :: ∀ t439 t442 t455 t458 t459. Ord t439 ⇒ Ord t442 ⇒ { flat ∷ Map t442 { au :: AudioUnit', prev :: Set t455, ptr :: t439 | t459 } | t458 } → Set t439
splitResGetImpetus s =
  DS.fromFoldable
    $ M.values
        ( map (_.ptr)
            $ M.filter
                (\i -> isSplitRes' i.au && DS.size i.prev == 0)
                s.flat
        )

getNextFromProcessors :: String -> Map String Int -> O.Object (Tuple (AudioGraphProcessor) String) -> Set Int
getNextFromProcessors k pag proc =
  DS.fromFoldable
    ( catMaybes
        $ map (flip M.lookup pag <<< fst)
            ( A.filter (\(Tuple _ (Tuple _ b)) -> b == k)
                (O.toUnfoldable proc)
            )
    )

getNextFromAggregators :: String -> Map String Int -> O.Object (Tuple (AudioGraphAggregator) (Set String)) -> Set Int
getNextFromAggregators k pag proc =
  DS.fromFoldable
    ( catMaybes
        $ map (flip M.lookup pag <<< fst)
            ( A.filter (\(Tuple _ (Tuple _ b)) -> k `member` b)
                (O.toUnfoldable proc)
            )
    )

getNexts :: forall ch. String -> Map String Int -> AudioGraph ch -> Set Int
getNexts k pag g = getNextFromProcessors k pag g.processors <> getNextFromAggregators k pag g.aggregators

chainsForProcessor :: forall ch. Pos ch => Set Int -> Maybe String -> String -> Maybe Int -> Map String Int -> (Tuple (AudioGraphProcessor) String) -> AudioGraph ch -> Maybe PtrInfo
chainsForProcessor nextIfTerminus toplevelName myName ptr' pag (Tuple proc input) g = do
  ptr <- ptr'
  pv <- M.lookup input pag
  let
    nexts = getNexts myName pag g
  pure
    { ptr: ptr
    , chan: toInt' (Proxy :: Proxy ch)
    , prev: DS.singleton pv
    , next: if DS.isEmpty nexts then nextIfTerminus else nexts
    , head: ptr
    , au: (agp2au' proc).au
    , status: On
    , name: (myName <> _) <$> toplevelName
    }

-------------- todo, merge with above?
chainsForAggregator :: forall ch. Pos ch => Set Int -> Maybe String -> String -> Maybe Int -> Map String Int -> (Tuple (AudioGraphAggregator) (Set String)) -> AudioGraph ch -> Maybe PtrInfo
chainsForAggregator nextIfTerminus toplevelName myName ptr' pag (Tuple proc input) g = do
  ptr <- ptr'
  pv <-
    DS.fromFoldable
      <$> ( sequence
            $ map (flip M.lookup pag) (DS.toUnfoldable input :: Array String)
        )
  let
    nexts = getNexts myName pag g
  pure
    { ptr: ptr
    , chan: toInt' (Proxy :: Proxy ch)
    , prev: pv
    , next: if DS.isEmpty nexts then nextIfTerminus else nexts
    , head: ptr
    , au: (aga2au' proc).au
    , status: On
    , name: (myName <> _) <$> toplevelName
    }

chainer :: ∀ t8471 t8472 t8475 t8476 t8483 t8488. (t8471 → t8472 → String → Maybe t8488 → Map String t8488 → t8483 → t8475 → Maybe t8476) → t8471 → t8472 → Map String t8488 → Object t8483 → t8475 → List t8476
chainer f nextIfTerminus toplevelName pag vs g =
  DL.catMaybes
    ( map
        ( \(Tuple k v) ->
            f nextIfTerminus toplevelName k (M.lookup k pag) pag v g
        )
        $ O.toUnfoldable vs
    )

chainsForProcessors :: forall ch. Pos ch => Set Int -> Maybe String -> Map String Int -> Object (Tuple (AudioGraphProcessor) String) -> AudioGraph ch -> List PtrInfo
chainsForProcessors = chainer chainsForProcessor

chainsForAggregators :: forall ch. Pos ch => Set Int -> Maybe String -> Map String Int -> Object (Tuple (AudioGraphAggregator) (Set String)) -> AudioGraph ch -> List PtrInfo
chainsForAggregators = chainer chainsForAggregator

audioToPtr ::
  forall channels.
  Pos channels =>
  AudioUnit channels -> AlgStep
audioToPtr = go (-1) DS.empty
  where
  go :: forall ch. Pos ch => Int -> Set Int -> AudioUnit ch -> AlgStep
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

      p =
        merge
          { head: ptr.ptr
          , next: ptr.next
          , prev: DS.empty :: Set Int
          , au: au.au
          , name: au.name
          }
          ptr
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
      r = go ptr.ptr (DS.singleton ptr.ptr) a

      au = au' v

      p =
        merge
          { head: ptr.ptr
          , next: ptr.next
          , prev: DS.singleton r.p.head
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
      r =
        foldl
          ( \b@(h :| tl) a ->
              ( go (h.p.ptr + h.len - 1) (DS.singleton ptr.ptr) a
              )
                :| (h : tl)
          )
          ( NE.singleton
              (go ptr.ptr (DS.singleton ptr.ptr) $ NE.head l)
          )
          (NE.tail l)

      au =
        ( \{ au: awd, name } ->
            { au: mergerHack awd (map _.p.ptr r), name }
        )
          (au' v)

      p =
        merge
          { head: ptr.ptr
          , next: ptr.next
          , prev: DS.fromFoldable $ (map (_.p.head) r)
          , au: au.au
          , name: au.name
          }
          ptr
    in
      { len: (foldl (+) 0 (map _.len r)) + 1
      , flat: (foldl (<>) M.empty (map _.flat r)) <> M.singleton ptr.ptr p
      , p
      }

  graphthrough ::
    forall ch ich.
    Pos ch =>
    Pos ich =>
    PtrInfo' ->
    AudioUnit ch ->
    AudioGraph ich ->
    AlgStep
  -- the constructor below guarantees this will be typesafe
  -- as a result, this takes a lot of shortcuts, assuming things
  -- will be correctly set up
  -- if they are not, the behavior is undefined
  graphthrough ptr v g =
    let
      auHack = au' v

      dummy =
        merge ptr
          { head: ptr.ptr
          , prev: DS.empty :: Set Int
          , au: (Constant' (AudioParameter { param: 0.0, timeOffset: 0.0 }))
          , name: auHack.name
          }
    in
      -- this should never happen from the type safety before, but is necessary
      -- so that we have a sort of "monoid" in case there is nothing here for some
      -- odd reason
      if O.size g.aggregators == 0 && O.size g.generators == 0 && O.size g.processors == 0 then
        { len: 1
        , flat: M.singleton ptr.ptr dummy
        , p: dummy
        }
      -- real code for graph
      else
        let
          -- attribute pointers to all processors and aggregators
          pag =
            M.fromFoldable
              $ A.mapWithIndex (\i k -> Tuple k (i + ptr.ptr)) (O.keys g.processors <> O.keys g.aggregators)

          -- run processing chains on all generators, giving them the correct next from the graph and offsetting the pointer by the length of all incoming nodes
          r =
            foldl
              ( \{ curP, algSteps } (Tuple k z) ->
                  let
                    o = go curP (getNexts k pag g) z
                  in
                    { curP: o.len + curP, algSteps: M.singleton k o <> algSteps }
              )
              { curP: ptr.ptr + (M.size pag) - 1, algSteps: M.empty }
              ((O.toUnfoldable g.generators) :: Array (Tuple String (AudioUnit ich)))

          -- add the generators to the pag map so that we can reason about their ids
          pagWithGens = pag <> M.mapMaybeWithKey (\_ z -> Just z.p.ptr) r.algSteps

          -- run processing chains on all processors and aggregators, giving them correct prev and next
          pc = chainsForProcessors ptr.next auHack.name pagWithGens g.processors g <> chainsForAggregators ptr.next auHack.name pagWithGens g.aggregators g

          -- tack on all processors, aggregators and generators to flat
          flat =
            (M.fromFoldable $ map (\i -> Tuple i.ptr i) pc)
              <> (fold (map _.flat (M.values r.algSteps)))

          p = DL.head (DL.filter (\i -> i.next == ptr.next) $ M.values flat)
        -- length is sum of generator lengths plus sum of aggregators plus sum of processors
        in
          maybe
            { len: 1
            , flat: M.singleton ptr.ptr dummy
            , p: dummy
            }
            { len: M.size flat, flat, p: _ }
            p

  {-type PtrInfo'
  = { ptr :: Int
    , chan :: Int
    , status :: Status
    , next :: Set Int
    }

type PtrInfo
  = { ptr :: Int
    , chan :: Int
    , next :: Set Int
    , status :: Status
    , prev :: Set Int
    , head :: Int
    , au :: AudioUnit'
    , name :: MString
    }
-}
  {-merge
          { head: ptr.ptr
          , prev: DS.fromFoldable $ (map (_.p.head) r)
          , au: au.au
          , name: au.name
          }
          ptr-}
  closurethrough ::
    forall ch ix.
    Pos ch =>
    Pos ix =>
    PtrInfo' ->
    AudioUnit ch ->
    AudioUnit ix ->
    AudioUnit ch ->
    (AlgStep -> Set Int) ->
    AlgStep
  closurethrough ptr v a evaluatedClosure getImpeti =
    let
      closureResult = go (ptr.ptr - 1) ptr.next evaluatedClosure

      myPtr = ptr.ptr + closureResult.len

      continuation =
        go
          (ptr.ptr + closureResult.len)
          (DS.singleton myPtr)
          a

      au = au' v

      impeti = getImpeti closureResult

      p =
        merge
          { head: closureResult.p.head
          , ptr: myPtr
          , prev: DS.singleton continuation.p.head
          , next: impeti
          , au: au.au
          , name: au.name
          }
          ptr

      out =
        { len: continuation.len + closureResult.len + 1
        -- everything that is in closureResult that is an impetus
        -- needs to have myPtr as its prev
        , flat:
            closureResult.flat
              <> continuation.flat
              <> (M.singleton myPtr p)
        , p
        }
    in
      out

  go' :: forall ch. Pos ch => PtrInfo' -> AudioUnit ch -> AlgStep
  go' ptr v@(Microphone name) = terminus ptr v

  go' ptr v@(AudioWorkletGenerator _ _ _) = terminus ptr v

  go' ptr v@(AudioWorkletProcessor _ _ _ a) = passthrough ptr v a

  go' ptr v@(AudioWorkletAggregator _ _ _ l) = listthrough ptr v l

  go' ptr v@(Play _ _ _) = terminus ptr v

  go' ptr v@(PlayBuf _ _ _ _) = terminus ptr v

  go' ptr v@(LoopBuf _ _ _ _ _) = terminus ptr v

  go' ptr v@(IIRFilter _ _ _ a) = passthrough ptr v a

  go' ptr v@(Lowpass _ _ _ a) = passthrough ptr v a

  go' ptr v@(Highpass _ _ _ a) = passthrough ptr v a

  go' ptr v@(Bandpass _ _ _ a) = passthrough ptr v a

  go' ptr v@(Lowshelf _ _ _ a) = passthrough ptr v a

  go' ptr v@(Highshelf _ _ _ a) = passthrough ptr v a

  go' ptr v@(Peaking _ _ _ _ a) = passthrough ptr v a

  go' ptr v@(Notch _ _ _ a) = passthrough ptr v a

  go' ptr v@(Allpass _ _ _ a) = passthrough ptr v a

  go' ptr v@(Convolver _ _ a) = passthrough ptr v a

  go' ptr v@(DynamicsCompressor _ _ _ _ _ _ a) = passthrough ptr v a

  go' ptr v@(SawtoothOsc _ _) = terminus ptr v

  go' ptr v@(TriangleOsc _ _) = terminus ptr v

  go' ptr v@(PeriodicOsc _ _ _) = terminus ptr v

  go' ptr v@(WaveShaper _ _ _ a) = passthrough ptr v a

  go' ptr v@(Dup1 name a f) = closurethrough ptr v a (f DupRes) dupResGetImpetus

  go' ptr v@(Dup2 name a f) = closurethrough ptr v a (f DupRes) dupResGetImpetus

  go' ptr v@(Dup3 name a f) = closurethrough ptr v a (f DupRes) dupResGetImpetus

  go' ptr v@(Dup4 name a f) = closurethrough ptr v a (f DupRes) dupResGetImpetus

  go' ptr v@(Dup5 name a f) = closurethrough ptr v a (f DupRes) dupResGetImpetus

  go' ptr v@(SinOsc name n) = terminus ptr v

  go' ptr v@(SquareOsc name n) = terminus ptr v

  go' ptr v@(Constant name n) = terminus ptr v

  go' ptr v@(NoSound name) = terminus ptr v

  go' ptr v@(SplitRes n) = terminus ptr v

  go' ptr v@(DupRes) = terminus ptr v

  go' ptr v@(StereoPanner name n a) = passthrough ptr v a

  go' ptr v@(Panner name vars a) = passthrough ptr v a

  go' ptr v@(Delay name n a) = passthrough ptr v a

  go' ptr v@(Mul name l) = listthrough ptr v l

  go' ptr v@(Merger name l) = listthrough ptr v (V.head l :| ((chopHack <<< fromFoldable <<< V.toArray) l))

  go' ptr v@(Add name l) = listthrough ptr v l

  go' ptr v@(Gain name n l) = listthrough ptr v l

  go' ptr v@(Speaker name l) = listthrough ptr v l

  go' ptr v@(Split1 name a f) = closurethrough ptr v a (f $ fill SplitRes) splitResGetImpetus

  go' ptr v@(Split2 name a f) = closurethrough ptr v a (f $ fill SplitRes) splitResGetImpetus

  go' ptr v@(Split3 name a f) = closurethrough ptr v a (f $ fill SplitRes) splitResGetImpetus

  go' ptr v@(Split4 name a f) = closurethrough ptr v a (f $ fill SplitRes) splitResGetImpetus

  go' ptr v@(Split5 name a f) = closurethrough ptr v a (f $ fill SplitRes) splitResGetImpetus

  go' ptr v@(Graph name g) = graphthrough ptr v g

apP :: forall a. AudioParameter a -> a
apP (AudioParameter { param }) = param

apT :: forall a. AudioParameter a -> Number
apT (AudioParameter { timeOffset }) = timeOffset

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

-- | The microphone.
-- |
-- | Make sure to enable the microphone before using this.
microphone :: AudioUnit D1
microphone = Microphone Nothing

microphone_ :: String -> AudioUnit D1
microphone_ = Microphone <<< Just

-- | Play an audio track.
-- |
-- | - s: A unique identifier for the audio track to be played. This should match an identifier passed to `runInBrowser`.
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

-- | A custom audio worklet generator.
-- |
-- | - s: A unique identifier for the audio worklet to be used. The worklet should be preloaded before using this.
-- | - params: The custom params passed to the audio worklet.
audioWorkletGenerator ::
  forall ch.
  Pos ch =>
  String ->
  Object Number ->
  AudioUnit ch
audioWorkletGenerator handle n = AudioWorkletGenerator Nothing handle (map ap_ n)

audioWorkletGenerator_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  Object Number ->
  AudioUnit ch
audioWorkletGenerator_ s handle n = AudioWorkletGenerator (Just s) handle (map ap_ n)

audioWorkletGeneratorT ::
  forall ch.
  Pos ch =>
  String ->
  Object (AudioParameter Number) ->
  AudioUnit ch
audioWorkletGeneratorT handle n = AudioWorkletGenerator Nothing handle n

audioWorkletGeneratorT_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  Object (AudioParameter Number) ->
  AudioUnit ch
audioWorkletGeneratorT_ s handle n = AudioWorkletGenerator (Just s) handle n

-- | A custom audio worklet processor
-- |
-- | - s: A unique identifier for the audio worklet to be used. The worklet should be preloaded before using this.
-- | - params: The custom params passed to the audio worklet
audioWorkletProcessor ::
  forall ch.
  Pos ch =>
  String ->
  Object Number ->
  AudioUnit ch ->
  AudioUnit ch
audioWorkletProcessor handle n = AudioWorkletProcessor Nothing handle (map ap_ n)

audioWorkletProcessor_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  Object Number ->
  AudioUnit ch ->
  AudioUnit ch
audioWorkletProcessor_ s handle n = AudioWorkletProcessor (Just s) handle (map ap_ n)

audioWorkletProcessorT ::
  forall ch.
  Pos ch =>
  String ->
  Object (AudioParameter Number) ->
  AudioUnit ch ->
  AudioUnit ch
audioWorkletProcessorT handle n = AudioWorkletProcessor Nothing handle n

audioWorkletProcessorT_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  Object (AudioParameter Number) ->
  AudioUnit ch ->
  AudioUnit ch
audioWorkletProcessorT_ s handle n = AudioWorkletProcessor (Just s) handle n

-- | A custom audio worklet aggregator
-- |
-- | - s: A unique identifier for the audio worklet to be used. The worklet should be preloaded before using this.
-- | - params: The custom params passed to the audio worklet
audioWorkletAggregator ::
  forall ch.
  Pos ch =>
  String ->
  Object Number ->
  AudioUnit ch ->
  AudioUnit ch ->
  AudioUnit ch
audioWorkletAggregator handle n a b = AudioWorkletAggregator Nothing handle (map ap_ n) (a :| b : Nil)

audioWorkletAggregator_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  Object Number ->
  AudioUnit ch ->
  AudioUnit ch ->
  AudioUnit ch
audioWorkletAggregator_ s handle n a b = AudioWorkletAggregator (Just s) handle (map ap_ n) (a :| b : Nil)

audioWorkletAggregatorT ::
  forall ch.
  Pos ch =>
  String ->
  Object (AudioParameter Number) ->
  AudioUnit ch ->
  AudioUnit ch ->
  AudioUnit ch
audioWorkletAggregatorT handle n a b = AudioWorkletAggregator Nothing handle n (a :| b : Nil)

audioWorkletAggregatorT_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  Object (AudioParameter Number) ->
  AudioUnit ch ->
  AudioUnit ch ->
  AudioUnit ch
audioWorkletAggregatorT_ s handle n a b = AudioWorkletAggregator (Just s) handle n (a :| b : Nil)

-- | Play a sound from a buffer
-- |
-- | - s: A unique identifier for the audio buffer to be played. This should match an identifier passed to `runInBrowser`
-- | - n: The playback rate. 1.0 is unit, 0.5 is twice as slow, 2.0 is twice as fast.
playBuf ::
  forall ch.
  Pos ch =>
  String ->
  Number ->
  AudioUnit ch
playBuf handle n = PlayBuf Nothing handle (ap_ n) (ap_ 0.0)

playBuf_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  Number ->
  AudioUnit ch
playBuf_ s handle n = PlayBuf (Just s) handle (ap_ n) (ap_ 0.0)

playBufT ::
  forall ch.
  Pos ch =>
  String ->
  AudioParameter Number ->
  AudioUnit ch
playBufT handle n = PlayBuf Nothing handle n (ap_ 0.0)

playBufT_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  AudioParameter Number ->
  AudioUnit ch
playBufT_ s handle n = PlayBuf (Just s) handle n (ap_ 0.0)

playBufWithOffset ::
  forall ch.
  Pos ch =>
  String ->
  Number ->
  Number ->
  AudioUnit ch
playBufWithOffset handle n o = PlayBuf Nothing handle (ap_ n) (ap_ o)

playBufWithOffset_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  Number ->
  Number ->
  AudioUnit ch
playBufWithOffset_ s handle n o = PlayBuf (Just s) handle (ap_ n) (ap_ o)

playBufWithOffsetT ::
  forall ch.
  Pos ch =>
  String ->
  AudioParameter Number ->
  AudioParameter Number ->
  AudioUnit ch
playBufWithOffsetT handle n o = PlayBuf Nothing handle n o

playBufWithOffsetT_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  AudioParameter Number ->
  AudioParameter Number ->
  AudioUnit ch
playBufWithOffsetT_ s handle n o = PlayBuf (Just s) handle n o

-- | Loop a sound from a buffer
-- |
-- | - s: A unique identifier for the audio buffer to be played. This should match an identifier passed to `runInBrowser`
-- | - n: The playback rate. 1.0 is unit, 0.5 is twice as slow, 2.0 is twice as fast.
-- | - st: Where in the sound the loop should start (in seconds)
-- | - st: Where in the sound the loop should end (in seconds). Set to 0.0 to go to the end of the sound.
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

-- | An IIR filter. See the [web audio API docs](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API/Using_IIR_filters) for more information.
-- |
-- | - ff: Feedforward coefficients.
-- | - fb: Feedback coefficients
iirFilter ::
  forall ch len.
  Pos ch =>
  Pos len =>
  LtEq len D20 =>
  Vec len Number ->
  Vec len Number ->
  AudioUnit ch ->
  AudioUnit ch
iirFilter a b = IIRFilter Nothing (V.toArray a) (V.toArray b)

iirFilter_ ::
  forall ch len.
  Pos ch =>
  Pos len =>
  LtEq len D20 =>
  String ->
  Vec len Number ->
  Vec len Number ->
  AudioUnit ch ->
  AudioUnit ch
iirFilter_ s a b = IIRFilter (Just s) (V.toArray a) (V.toArray b)

-- |
-- | - f:  The cutoff frequency.
-- | - q:  The Q value in positive decibels. See [BiquadFilterNode](https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode) in the WebAudio documentation for more information on Q values.
lowpass :: forall ch. Pos ch => Number -> Number -> AudioUnit ch -> AudioUnit ch
lowpass a b = Lowpass Nothing (ap_ a) (ap_ b)

lowpass_ :: forall ch. Pos ch => String -> Number -> Number -> AudioUnit ch -> AudioUnit ch
lowpass_ s a b = Lowpass (Just s) (ap_ a) (ap_ b)

lowpassT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
lowpassT a b = Lowpass Nothing a b

lowpassT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
lowpassT_ s a b = Lowpass (Just s) a b

-- | A highpass filter.
-- |
-- | - f:  The cutoff frequency.
-- | - q:  The Q value in positive decibels. See [BiquadFilterNode](https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode) in the WebAudio documentation for more information on Q values.
highpass :: forall ch. Pos ch => Number -> Number -> AudioUnit ch -> AudioUnit ch
highpass a b = Highpass Nothing (ap_ a) (ap_ b)

highpass_ :: forall ch. Pos ch => String -> Number -> Number -> AudioUnit ch -> AudioUnit ch
highpass_ s a b = Highpass (Just s) (ap_ a) (ap_ b)

highpassT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
highpassT a b = Highpass Nothing a b

highpassT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
highpassT_ s a b = Highpass (Just s) a b

-- | A bandpass filter.
-- |
-- | - f:  The frequency to allow to pass.
-- | - q:  The Q value in positive decibels. See [BiquadFilterNode](https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode) in the WebAudio documentation for more information on Q values.
bandpass :: forall ch. Pos ch => Number -> Number -> AudioUnit ch -> AudioUnit ch
bandpass a b = Bandpass Nothing (ap_ a) (ap_ b)

bandpass_ :: forall ch. Pos ch => String -> Number -> Number -> AudioUnit ch -> AudioUnit ch
bandpass_ s a b = Bandpass (Just s) (ap_ a) (ap_ b)

bandpassT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
bandpassT a b = Bandpass Nothing a b

bandpassT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
bandpassT_ s a b = Bandpass (Just s) a b

-- | A lowshelf filter.
-- |
-- | - f:  The frequency.
-- | - g:  The rolloff gain in decibels. See [BiquadFilterNode](https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode) in the WebAudio documentation for more information on how gain effects rolloff.
lowshelf :: forall ch. Pos ch => Number -> Number -> AudioUnit ch -> AudioUnit ch
lowshelf a b = Lowshelf Nothing (ap_ a) (ap_ b)

lowshelf_ :: forall ch. Pos ch => String -> Number -> Number -> AudioUnit ch -> AudioUnit ch
lowshelf_ s a b = Lowshelf (Just s) (ap_ a) (ap_ b)

lowshelfT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
lowshelfT a b = Lowshelf Nothing a b

lowshelfT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
lowshelfT_ s a b = Lowshelf (Just s) a b

-- | A highshelf filter.
-- |
-- | - f:  The frequency.
-- | - g:  The rolloff gain in decibels. See [BiquadFilterNode](https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode) in the WebAudio documentation for more information on how gain effects rolloff.
highshelf :: forall ch. Pos ch => Number -> Number -> AudioUnit ch -> AudioUnit ch
highshelf a b = Highshelf Nothing (ap_ a) (ap_ b)

highshelf_ :: forall ch. Pos ch => String -> Number -> Number -> AudioUnit ch -> AudioUnit ch
highshelf_ s a b = Highshelf (Just s) (ap_ a) (ap_ b)

highshelfT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
highshelfT a b = Highshelf Nothing a b

highshelfT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
highshelfT_ s a b = Highshelf (Just s) a b

-- | A peaking filter.
-- | This is like a bandpass filter with an extra vector of control
-- |
-- | - f:  The frequency.
-- | - q:  The Q value in positive decibels. See [BiquadFilterNode](https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode) in the WebAudio documentation for more information on Q values.
-- | - g:  The rolloff gain in decibels. See [BiquadFilterNode](https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode) in the WebAudio documentation for more information on how gain effects rolloff.
peaking :: forall ch. Pos ch => Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
peaking a b c = Peaking Nothing (ap_ a) (ap_ b) (ap_ c)

peaking_ :: forall ch. Pos ch => String -> Number -> Number -> Number -> AudioUnit ch -> AudioUnit ch
peaking_ s a b c = Peaking (Just s) (ap_ a) (ap_ b) (ap_ c)

peakingT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
peakingT a b c = Peaking Nothing a b c

peakingT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
peakingT_ s a b c = Peaking (Just s) a b c

-- | A notch filter.
-- | The opposite of bandpass.
-- |
-- | - f:  The frequency.
-- | - q:  The Q value in positive decibels. See [BiquadFilterNode](https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode) in the WebAudio documentation for more information on Q values.
notch :: forall ch. Pos ch => Number -> Number -> AudioUnit ch -> AudioUnit ch
notch a b = Notch Nothing (ap_ a) (ap_ b)

notch_ :: forall ch. Pos ch => String -> Number -> Number -> AudioUnit ch -> AudioUnit ch
notch_ s a b = Notch (Just s) (ap_ a) (ap_ b)

notchT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
notchT a b = Notch Nothing a b

notchT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
notchT_ s a b = Notch (Just s) a b

-- | An allpass filter.
-- | Lets all frequencies through, but alters the phase relationship.
-- |
-- | - f:  The frequency.
-- | - q:  The Q value in positive decibels. See [BiquadFilterNode](https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode) in the WebAudio documentation for more information on Q values.
allpass :: forall ch. Pos ch => Number -> Number -> AudioUnit ch -> AudioUnit ch
allpass a b = Allpass Nothing (ap_ a) (ap_ b)

allpass_ :: forall ch. Pos ch => String -> Number -> Number -> AudioUnit ch -> AudioUnit ch
allpass_ s a b = Allpass (Just s) (ap_ a) (ap_ b)

allpassT :: forall ch. Pos ch => AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
allpassT a b = Allpass Nothing a b

allpassT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
allpassT_ s a b = Allpass (Just s) a b

-- | A convolver (reverb)
-- |
-- | - s: A unique identifier for the audio buffer to be used as the impulse response. This should match an identifier passed to `runInBrowser`.
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

-- | A compressor
-- |
-- | - threshold: the decibel value above which the compression will start taking effect.
-- | - knee: a decibel value representing the range above the threshold where the curve smoothly transitions to the compressed portion.
-- | - ratio: the amount of change, in dB, needed in the input for a 1 dB change in the output.
-- | - attack: the amount of time, in seconds, required to reduce the gain by 10 dB.
-- | - release: the amount of time, in seconds, required to increase the gain by 10 dB.
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

-- | Duplicate a mono sound.
-- |
-- | Equivalent to creating a sound twice, but more memory efficient in that it avoids duplication.
dup1 :: forall ch. Pos ch => AudioUnit D1 -> (AudioUnit D1 -> AudioUnit ch) -> AudioUnit ch
dup1 DupRes f = f DupRes

dup1 x f = Dup1 Nothing x f

dup1_ :: forall ch. Pos ch => String -> AudioUnit D1 -> (AudioUnit D1 -> AudioUnit ch) -> AudioUnit ch
dup1_ s DupRes f = f DupRes

dup1_ s x f = Dup1 (Just s) x f

-- | Duplicate a stereo sound.
-- |
-- | Equivalent to creating a sound twice, but more memory efficient in that it avoids duplication.
dup2 :: forall ch. Pos ch => AudioUnit D2 -> (AudioUnit D2 -> AudioUnit ch) -> AudioUnit ch
dup2 DupRes f = f DupRes

dup2 x f = Dup2 Nothing x f

dup2_ :: forall ch. Pos ch => String -> AudioUnit D2 -> (AudioUnit D2 -> AudioUnit ch) -> AudioUnit ch
dup2_ s DupRes f = f DupRes

dup2_ s x f = Dup2 (Just s) x f

-- | Duplicate a three-channel sound
dup3 :: forall ch. Pos ch => AudioUnit D3 -> (AudioUnit D3 -> AudioUnit ch) -> AudioUnit ch
dup3 DupRes f = f DupRes

dup3 x f = Dup3 Nothing x f

dup3_ :: forall ch. Pos ch => String -> AudioUnit D3 -> (AudioUnit D3 -> AudioUnit ch) -> AudioUnit ch
dup3_ s DupRes f = f DupRes

dup3_ s x f = Dup3 (Just s) x f

-- | Duplicate a four-channel sound
dup4 :: forall ch. Pos ch => AudioUnit D4 -> (AudioUnit D4 -> AudioUnit ch) -> AudioUnit ch
dup4 DupRes f = f DupRes

dup4 x f = Dup4 Nothing x f

dup4_ :: forall ch. Pos ch => String -> AudioUnit D4 -> (AudioUnit D4 -> AudioUnit ch) -> AudioUnit ch
dup4_ s DupRes f = f DupRes

dup4_ s x f = Dup4 (Just s) x f

-- | Duplicate a five-channel sound
dup5 :: forall ch. Pos ch => AudioUnit D5 -> (AudioUnit D5 -> AudioUnit ch) -> AudioUnit ch
dup5 DupRes f = f DupRes

dup5 x f = Dup5 Nothing x f

dup5_ :: forall ch. Pos ch => String -> AudioUnit D5 -> (AudioUnit D5 -> AudioUnit ch) -> AudioUnit ch
dup5_ s DupRes f = f DupRes

dup5_ s x f = Dup5 (Just s) x f

-- | A wave shaper.
-- | Used to create distortion/overdrive.
-- |
-- | - s: A unique identifier for the float array with the wave shape. This should match an identifier passed to `runInBrowser`.
-- | - o: An oversampling factor (None TwoX FourX)
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

-- | A periodic oscillator.
-- | Used to create an oscillator based on complex numbers that describe the overtones. See [PeriodicWave](https://developer.mozilla.org/en-US/docs/Web/API/PeriodicWave) for more information
-- |
-- | - s: A unique identifier for the periodic wave to use. This should match an identifier passed to `runInBrowser`.
-- | - f: The fundamental frequency
periodicOsc ::
  String ->
  Number ->
  AudioUnit D1
periodicOsc handle n = PeriodicOsc Nothing (ap_ n) handle

periodicOsc_ ::
  String ->
  String ->
  Number ->
  AudioUnit D1
periodicOsc_ s handle n = PeriodicOsc (Just s) (ap_ n) handle

periodicOscT ::
  String ->
  AudioParameter Number ->
  AudioUnit D1
periodicOscT handle n = PeriodicOsc Nothing n handle

periodicOscT_ ::
  String ->
  String ->
  AudioParameter Number ->
  AudioUnit D1
periodicOscT_ s handle n = PeriodicOsc (Just s) n handle

-- | A sine wave oscillator.
-- |
-- | - f: The fundamental frequency
sinOsc :: Number -> AudioUnit D1
sinOsc n = SinOsc Nothing (ap_ n)

sinOsc_ :: String -> Number -> AudioUnit D1
sinOsc_ s n = SinOsc (Just s) (ap_ n)

sinOscT :: AudioParameter Number -> AudioUnit D1
sinOscT n = SinOsc Nothing n

sinOscT_ :: String -> AudioParameter Number -> AudioUnit D1
sinOscT_ s n = SinOsc (Just s) n

-- | A sawtooth oscillator.
-- |
-- | - f: The fundamental frequency
sawtoothOsc :: Number -> AudioUnit D1
sawtoothOsc n = SawtoothOsc Nothing (ap_ n)

sawtoothOsc_ :: String -> Number -> AudioUnit D1
sawtoothOsc_ s n = SawtoothOsc (Just s) (ap_ n)

sawtoothOscT :: AudioParameter Number -> AudioUnit D1
sawtoothOscT n = SawtoothOsc Nothing n

sawtoothOscT_ :: String -> AudioParameter Number -> AudioUnit D1
sawtoothOscT_ s n = SawtoothOsc (Just s) n

-- | A triangle oscillator.
-- |
-- | - f: The fundamental frequency
triangleOsc :: Number -> AudioUnit D1
triangleOsc n = TriangleOsc Nothing (ap_ n)

triangleOsc_ :: String -> Number -> AudioUnit D1
triangleOsc_ s n = TriangleOsc (Just s) (ap_ n)

triangleOscT :: AudioParameter Number -> AudioUnit D1
triangleOscT n = TriangleOsc Nothing n

triangleOscT_ :: String -> AudioParameter Number -> AudioUnit D1
triangleOscT_ s n = TriangleOsc (Just s) n

-- | A square oscillator.
-- |
-- | - f: The fundamental frequency
squareOsc :: Number -> AudioUnit D1
squareOsc n = SquareOsc Nothing (ap_ n)

squareOsc_ :: String -> Number -> AudioUnit D1
squareOsc_ s n = SquareOsc (Just s) (ap_ n)

squareOscT :: AudioParameter Number -> AudioUnit D1
squareOscT n = SquareOsc Nothing n

squareOscT_ :: String -> AudioParameter Number -> AudioUnit D1
squareOscT_ s n = SquareOsc (Just s) n

-- | A splitter for a mono sound.
-- | Effectively a no-op, same as dup1.
split1 :: forall ch. Pos ch => AudioUnit D1 -> (Vec D1 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split1 (SplitRes i) f = f (fill $ const (SplitRes i))

split1 x f = Split1 Nothing x f

split1_ :: forall ch. Pos ch => String -> AudioUnit D1 -> (Vec D1 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split1_ s (SplitRes i) f = f (fill $ const (SplitRes i))

split1_ s x f = Split1 (Just s) x f

-- | A splitter for a stereo sound.
split2 :: forall ch. Pos ch => AudioUnit D2 -> (Vec D2 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split2 (SplitRes i) f = f (fill $ const (SplitRes i))

split2 x f = Split2 Nothing x f

split2_ :: forall ch. Pos ch => String -> AudioUnit D2 -> (Vec D2 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split2_ s (SplitRes i) f = f (fill $ const (SplitRes i))

split2_ s x f = Split2 (Just s) x f

-- | A splitter for a three-channel sound.
split3 :: forall ch. Pos ch => AudioUnit D3 -> (Vec D3 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split3 (SplitRes i) f = f (fill $ const (SplitRes i))

split3 x f = Split3 Nothing x f

split3_ :: forall ch. Pos ch => String -> AudioUnit D3 -> (Vec D3 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split3_ s (SplitRes i) f = f (fill $ const (SplitRes i))

split3_ s x f = Split3 (Just s) x f

-- | A splitter for a four-channel sound.
split4 :: forall ch. Pos ch => AudioUnit D4 -> (Vec D4 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split4 (SplitRes i) f = f (fill $ const (SplitRes i))

split4 x f = Split4 Nothing x f

split4_ :: forall ch. Pos ch => String -> AudioUnit D4 -> (Vec D4 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split4_ s (SplitRes i) f = f (fill $ const (SplitRes i))

split4_ s x f = Split4 (Just s) x f

-- | A splitter for a five-channel sound.
split5 :: forall ch. Pos ch => AudioUnit D5 -> (Vec D5 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split5 (SplitRes i) f = f (fill $ const (SplitRes i))

split5 x f = Split5 Nothing x f

split5_ :: forall ch. Pos ch => String -> AudioUnit D5 -> (Vec D5 (AudioUnit D1) -> AudioUnit ch) -> AudioUnit ch
split5_ s (SplitRes i) f = f (fill $ const (SplitRes i))

split5_ s x f = Split5 (Just s) x f

-- | A stereo panner.
-- |
-- | n: The place in the stero field to place the sound. -1.0 is all the way left, 1.0 is all the way right.
panner :: Number -> AudioUnit D2 -> AudioUnit D2
panner n = StereoPanner Nothing (ap_ n)

panner_ :: String -> Number -> AudioUnit D2 -> AudioUnit D2
panner_ s n = StereoPanner (Just s) (ap_ n)

pannerT :: AudioParameter Number -> AudioUnit D2 -> AudioUnit D2
pannerT n = StereoPanner Nothing n

pannerT_ :: String -> AudioParameter Number -> AudioUnit D2 -> AudioUnit D2
pannerT_ s n = StereoPanner (Just s) n

pannerMono :: Number -> AudioUnit D1 -> AudioUnit D2
pannerMono n = panner n <<< unsafeCoerce

pannerMono_ :: String -> Number -> AudioUnit D1 -> AudioUnit D2
pannerMono_ s n = panner_ s n <<< unsafeCoerce

pannerMonoT :: AudioParameter Number -> AudioUnit D1 -> AudioUnit D2
pannerMonoT n = pannerT n <<< unsafeCoerce

pannerMonoT_ :: String -> AudioParameter Number -> AudioUnit D1 -> AudioUnit D2
pannerMonoT_ s n = pannerT_ s n <<< unsafeCoerce

-- | A spatial panner.
-- |
-- | See [Panner](https://developer.mozilla.org/en-US/docs/Web/API/PannerNode) in the WebAudio API.
pannerVarsAsAudioParams :: PannerVars' -> PannerVars
pannerVarsAsAudioParams n =
  { coneInnerAngle: (ap_ n.coneInnerAngle)
  , coneOuterAngle: (ap_ n.coneOuterAngle)
  , coneOuterGain: (ap_ n.coneOuterGain)
  , distanceModel: n.distanceModel
  , maxDistance: (ap_ n.maxDistance)
  , orientationX: (ap_ n.orientationX)
  , orientationY: (ap_ n.orientationY)
  , orientationZ: (ap_ n.orientationZ)
  , panningModel: n.panningModel
  , positionX: (ap_ n.positionX)
  , positionY: (ap_ n.positionY)
  , positionZ: (ap_ n.positionZ)
  , refDistance: (ap_ n.refDistance)
  , rolloffFactor: (ap_ n.rolloffFactor)
  }

spatialPanner :: PannerVars' -> AudioUnit D2 -> AudioUnit D2
spatialPanner = Panner Nothing <<< pannerVarsAsAudioParams

spatialPanner_ :: String -> PannerVars' -> AudioUnit D2 -> AudioUnit D2
spatialPanner_ s = Panner (Just s) <<< pannerVarsAsAudioParams

spatialPannerT :: PannerVars -> AudioUnit D2 -> AudioUnit D2
spatialPannerT = Panner Nothing

spatialPannerT_ :: String -> PannerVars -> AudioUnit D2 -> AudioUnit D2
spatialPannerT_ s = Panner (Just s)

spatialPannerMono :: PannerVars' -> AudioUnit D1 -> AudioUnit D2
spatialPannerMono v = spatialPanner v <<< unsafeCoerce

spatialPannerMono_ :: String -> PannerVars' -> AudioUnit D1 -> AudioUnit D2
spatialPannerMono_ s v = spatialPanner_ s v <<< unsafeCoerce

spatialPannerMonoT :: PannerVars -> AudioUnit D1 -> AudioUnit D2
spatialPannerMonoT v = spatialPannerT v <<< unsafeCoerce

spatialPannerMonoT_ :: String -> PannerVars -> AudioUnit D1 -> AudioUnit D2
spatialPannerMonoT_ s v = spatialPannerT_ s v <<< unsafeCoerce

-- | Send sound to the speaker.
-- |
-- | If you want sound to be played from your speaker, you _must_ use this function
-- | Otherwise, the audio graph will render but no sound will come out.
speaker :: forall ch. Pos ch => NonEmpty List (AudioUnit ch) -> AudioUnit ch
speaker = Speaker Nothing

speaker' :: forall ch. Pos ch => AudioUnit ch -> AudioUnit ch
speaker' = Speaker Nothing <<< NE.singleton

speaker_ :: forall ch. Pos ch => String -> NonEmpty List (AudioUnit ch) -> AudioUnit ch
speaker_ = Speaker <<< Just

-- | A merger of mono audio channels.
-- |
-- | Accepts a vector of mono audio and produces the merged result.
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

-- | Multiply signals
mul :: forall ch. Pos ch => NonEmpty List (AudioUnit ch) -> AudioUnit ch
mul = Mul Nothing

mul_ :: forall ch. Pos ch => String -> NonEmpty List (AudioUnit ch) -> AudioUnit ch
mul_ s = Mul (Just s)

-- | Add several signals.
add :: forall ch. Pos ch => NonEmpty List (AudioUnit ch) -> AudioUnit ch
add = Add Nothing

add_ :: forall ch. Pos ch => String -> NonEmpty List (AudioUnit ch) -> AudioUnit ch
add_ s = Add (Just s)

-- | A constant signal.
-- |
-- | - n : The constant number.
constant :: Number -> AudioUnit D1
constant n = Constant Nothing (ap_ n)

constant_ :: String -> Number -> AudioUnit D1
constant_ s n = Constant (Just s) (ap_ n)

constantT :: AudioParameter Number -> AudioUnit D1
constantT n = Constant Nothing n

constantT_ :: String -> AudioParameter Number -> AudioUnit D1
constantT_ s n = Constant (Just s) n

-- | A delayed signal.
-- |
-- | - n : The number of seconds to delay.
delay :: forall ch. Pos ch => Number -> AudioUnit ch -> AudioUnit ch
delay n = Delay Nothing (ap_ n)

delay_ :: forall ch. Pos ch => String -> Number -> AudioUnit ch -> AudioUnit ch
delay_ s n = Delay (Just s) (ap_ n)

delayT :: forall ch. Pos ch => AudioParameter Number -> AudioUnit ch -> AudioUnit ch
delayT n = Delay Nothing n

delayT_ :: forall ch. Pos ch => String -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
delayT_ s n = Delay (Just s) n

-- | A volume control.
-- |
-- | - n : The amount of volume attenuation or augmentation. 0.0 is off, 1.0 is unit. The same as `mul`.
gain :: forall ch. Pos ch => Number -> NonEmpty List (AudioUnit ch) -> AudioUnit ch
gain n = Gain Nothing (ap_ n)

gain_ :: forall ch. Pos ch => String -> Number -> NonEmpty List (AudioUnit ch) -> AudioUnit ch
gain_ s n = Gain (Just s) (ap_ n)

gainT :: forall ch. Pos ch => AudioParameter Number -> NonEmpty List (AudioUnit ch) -> AudioUnit ch
gainT n = Gain Nothing n

gainT_ :: forall ch. Pos ch => String -> AudioParameter Number -> NonEmpty List (AudioUnit ch) -> AudioUnit ch
gainT_ s n = Gain (Just s) n

-- | A variant of gain that accepts a single audio unit instead of a non-empty list.
gain' :: forall ch. Pos ch => Number -> AudioUnit ch -> AudioUnit ch
gain' n = Gain Nothing (ap_ n) <<< NE.singleton

gainT' :: forall ch. Pos ch => AudioParameter Number -> AudioUnit ch -> AudioUnit ch
gainT' n = Gain Nothing n <<< NE.singleton

gain_' :: forall ch. Pos ch => String -> Number -> AudioUnit ch -> AudioUnit ch
gain_' s n = Gain (Just s) (ap_ n) <<< NE.singleton

gainT_' :: forall ch. Pos ch => String -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch
gainT_' s n = Gain (Just s) n <<< NE.singleton

class AggregatorsToGraphInternal (aggregators :: RowList) (graph :: RowList) | aggregators -> graph

instance aggregatorsToGraphInternalNil :: AggregatorsToGraphInternal Nil Nil

instance aggregatorsToGraphInternalCons ::
  ( AggregatorsToGraphInternal t newT
    ) =>
  AggregatorsToGraphInternal (Cons k (Tuple (AudioGraphAggregator) (SLProxy v)) t) (Cons k (SLProxy v) newT)

instance aggregatorsToGraph ::
  ( RowToList aggregators ag
  , AggregatorsToGraphInternal ag gl
  , ListToRow gl graph
  ) =>
  AggregatorsToGraph aggregators graph

class AggregatorsToGraph (aggregators :: # Type) (graph :: # Type) | aggregators -> graph

class ProcessorsToGraphInternal (processors :: RowList) (graph :: RowList) | processors -> graph

instance processorsToGraphInternalNil :: ProcessorsToGraphInternal Nil Nil

instance processorsToGraphInternalCons ::
  ( ProcessorsToGraphInternal t newT
    ) =>
  ProcessorsToGraphInternal (Cons k (Tuple (AudioGraphProcessor) (SProxy v)) t) (Cons k (SLProxy (v :/ SNil)) newT)

class ProcessorsToGraph (processors :: # Type) (graph :: # Type) | processors -> graph

instance porocessorsToGraph ::
  ( RowToList processors ps
  , ProcessorsToGraphInternal ps gl
  , ListToRow gl graph
  ) =>
  ProcessorsToGraph processors graph

class GeneratorsToGraphInternal (generators :: RowList) (graph :: RowList) | generators -> graph

instance generatorsToGraphInternalNil :: GeneratorsToGraphInternal Nil Nil

instance generatorsToGraphInternalCons ::
  ( GeneratorsToGraphInternal t newT
    ) =>
  GeneratorsToGraphInternal (Cons k (AudioUnit ch) t) (Cons k (SLProxy SNil) newT)

class GeneratorsToGraph (generators :: # Type) (graph :: # Type) | generators -> graph

instance generatorsToGraph ::
  ( RowToList generators gn
  , GeneratorsToGraphInternal gn gl
  , ListToRow gl graph
  ) =>
  GeneratorsToGraph generators graph

class LookUpProcessorsInternal (rowList :: RowList) (graph :: RowList) (processors :: # Type) | rowList graph -> processors

class LookupProcessors (audioGraph :: # Type) (processors :: # Type) | audioGraph -> processors

instance lookUpProcessorsInternalNil :: (ListToRow rl r) => LookUpProcessorsInternal rl Nil r
else instance lookUpProcessorsInternalCons :: (ListToRow (Cons k v t) r) => LookUpProcessorsInternal (Cons k v t) g r
else instance lookUpProcessorsInternalYes :: LookUpProcessorsInternal x (Cons "processors" (Record v) t) v
else instance lookUpProcessorsInternalNo ::
  ( LookUpProcessorsInternal Nil t v
    ) =>
  LookUpProcessorsInternal Nil (Cons x y t) v

instance lookupProcessors ::
  ( RowToList audioGraph ag
  , LookUpProcessorsInternal Nil ag processors
  ) =>
  LookupProcessors audioGraph processors

class LookUpAggregatorsInternal (rowList :: RowList) (graph :: RowList) (aggregators :: # Type) | rowList graph -> aggregators

class LookupAggregators (audioGraph :: # Type) (aggregators :: # Type) | audioGraph -> aggregators

instance lookUpAggregatorsInternalNil :: (ListToRow rl r) => LookUpAggregatorsInternal rl Nil r
else instance lookUpAggregatorsInternalCons :: (ListToRow (Cons k v t) r) => LookUpAggregatorsInternal (Cons k v t) g r
else instance lookUpAggregatorsInternalYes :: LookUpAggregatorsInternal x (Cons "aggregators" (Record v) t) v
else instance lookUpAggregatorsInternalNo ::
  ( LookUpAggregatorsInternal Nil t v
    ) =>
  LookUpAggregatorsInternal Nil (Cons x y t) v

instance lookupAggregators ::
  ( RowToList audioGraph ag
  , LookUpAggregatorsInternal Nil ag aggregators
  ) =>
  LookupAggregators audioGraph aggregators

class LookUpGeneratorsInternal (rowList :: RowList) (graph :: RowList) (generators :: # Type) | rowList graph -> generators

class LookupGenerators (audioGraph :: # Type) (generators :: # Type) | audioGraph -> generators

instance lookUpGeneratorsInternalNil :: (ListToRow rl r) => LookUpGeneratorsInternal rl Nil r
else instance lookUpGeneratorsInternalCons :: (ListToRow (Cons k v t) r) => LookUpGeneratorsInternal (Cons k v t) g r
else instance lookUpGeneratorsInternalYes :: LookUpGeneratorsInternal x (Cons "generators" (Record v) t) v
else instance lookUpGeneratorsInternalNo ::
  ( LookUpGeneratorsInternal Nil t v
    ) =>
  LookUpGeneratorsInternal Nil (Cons x y t) v

instance lookupGenerators ::
  ( RowToList audioGraph ag
  , LookUpGeneratorsInternal Nil ag generators
  ) =>
  LookupGenerators audioGraph generators

class AudioGraphToGraph (audioGraph :: # Type) (graph :: # Type) | audioGraph -> graph

instance audioGraphToGraph ::
  ( LookupGenerators audioGraph generators
  , LookupProcessors audioGraph processors
  , LookupAggregators audioGraph aggregators
  , GeneratorsToGraph generators genGraph
  , ProcessorsToGraph processors procGraph
  , AggregatorsToGraph aggregators accGraph
  , Union genGraph procGraph step0
  , Union step0 accGraph graph
  ) =>
  AudioGraphToGraph audioGraph graph

class RecordHomogeneousInAudioUnits (gate :: Boolean) (generators :: Type) ch

instance recordHomogeneousInAudioUnitsFalse :: RecordHomogeneousInAudioUnits False g ch

instance recordHomogeneousInAudioUnitsF :: Homogeneous g (AudioUnit ch) => RecordHomogeneousInAudioUnits True (Record g) ch

class RecordNotEmptyInternal (gate :: Boolean) (generators :: Type) (b :: Boolean) | gate generators -> b

instance recordNotEmptyInternalFalse :: RecordNotEmptyInternal False g False
else instance recordNotEmptyInternalTrue :: (RowToList g (Cons k v t)) => RecordNotEmptyInternal True (Record g) True
else instance recordNotEmptyInternalF :: RecordNotEmptyInternal True g False

class HasOneGeneratorInternal (gate :: Boolean) (graph :: RowList) ch (b :: Boolean) | gate graph -> b

instance hasOneGeneratorInternalNil :: HasOneGeneratorInternal b Nil ch b
else instance hasOneGeneratorInternalTrue :: HasOneGeneratorInternal True g ch True
else instance hasOneGeneratorInternalCons ::
  ( Compare "generators" k kgen
  , IsEq kgen isGeneratorRecord
  , RecordNotEmptyInternal isGeneratorRecord v bx
  , RecordHomogeneousInAudioUnits isGeneratorRecord v ch
  , HasOneGeneratorInternal bx t ch b
  ) =>
  HasOneGeneratorInternal False (Cons k v t) ch b

class HasOneGenerator (graph :: # Type) ch (b :: Boolean) | graph ch -> b

instance hasOneGenerator ::
  ( RowToList graph gl
  , HasOneGeneratorInternal False gl ch b
  ) =>
  HasOneGenerator graph ch b

instance isValidAudioGraph ::
  ( HasOneGenerator audioGraph ch hasOneGenerator
  , AudioGraphToGraph audioGraph graphUnflipped
  , FlipDirection graphUnflipped graph
  , HasOrphanNodes graphUnflipped hasOrphanNodes
  , HasDuplicateNodes graph hasDuplicateNodes
  , HasDuplicateEdges graph hasDuplicateEdges
  , HasUniqueTerminus graph hasUniqueTerminus
  , IsConnected graph isConnected
  , Not hasOrphanNodes noOrphanNodes
  , Not hasDuplicateNodes noDuplicateNodes
  , Not hasDuplicateEdges noDuplicateEdges
  , And hasOneGenerator hasUniqueTerminus step0
  , And step0 isConnected step1
  , And step1 noOrphanNodes step2
  , And step2 noDuplicateNodes step3
  , And step3 noDuplicateEdges b
  ) =>
  IsValidAudioGraph audioGraph ch b

class IsValidAudioGraph (graph :: # Type) ch (b :: Boolean) | graph -> b

class ValidAudioGraph (graph :: # Type) ch

instance validAudioGraph :: (IsValidAudioGraph graph ch True) => ValidAudioGraph graph ch
else instance validAudioGraphFail ::
  (Fail (Text "Graph is not a valid audio graph"), IsValidAudioGraph graph ch False) =>
  ValidAudioGraph
    graph
    ch

class AsProcessor v where
  asProcessor :: v -> Maybe (Tuple AudioGraphProcessor String)

instance asProcessorJust :: (IsSymbol k) => AsProcessor (Tuple (AudioGraphProcessor) (SProxy k)) where
  asProcessor (Tuple a b) = Just (Tuple a $ reflectSymbol b)
else instance asProcessorOther :: AsProcessor x where
  asProcessor _ = Nothing

class AsProcessorObject (iter :: RowList) (processors :: # Type) where
  asProcessorObject :: RLProxy iter -> Record processors -> Object (Tuple (AudioGraphProcessor) String)

instance asProcessorObjectCons ::
  (IsSymbol k, AsProcessorObject tail graph, AsProcessor v) =>
  AsProcessorObject (Cons k v tail) graph where
  asProcessorObject _ g = let asStr = reflectSymbol (SProxy :: SProxy k) in O.union (maybe O.empty (O.singleton asStr) $ asProcessor ((unsafeGet asStr g) :: v)) (asProcessorObject (RLProxy :: RLProxy tail) g)
else instance asProcessorObjectNil :: AsProcessorObject Nil g where
  asProcessorObject _ g = O.empty

class AudioGraphProcessors (iter :: RowList) (graph :: # Type) where
  processors :: RLProxy iter -> Record graph -> O.Object (Tuple (AudioGraphProcessor) String)

instance audioGraphProcessorsNil :: AudioGraphProcessors Nil graph where
  processors _ g = O.empty
else instance audioGraphProcessorsCons ::
  (RowToList g gl, AsProcessorObject gl g, AudioGraphProcessors tail graph) =>
  AudioGraphProcessors (Cons "processors" (Record g) tail) graph where
  processors _ g = O.union (asProcessorObject (RLProxy :: RLProxy gl) ((unsafeGet "processors" g) :: (Record g))) (processors (RLProxy :: RLProxy tail) g)
else instance audioGraphProcessorsGiveUp0 ::
  (IsSymbol k, AudioGraphProcessors tail graph) =>
  AudioGraphProcessors (Cons k v tail) graph where
  processors _ g = processors (RLProxy :: RLProxy tail) g

---------------------
----------------
-------
class AudioGraphGenerators (iter :: RowList) (graph :: # Type) ch where
  generators :: RLProxy iter -> Record graph -> O.Object (AudioUnit ch)

instance audioGraphGeneratorsNil :: AudioGraphGenerators Nil graph ch where
  generators _ g = O.empty
else instance audioGraphGeneratorsCons ::
  ( RowToList genrec gl
    ) =>
  AudioGraphGenerators (Cons "generators" (Record genrec) tail) graph ch where
  generators _ g = (unsafeGet "generators" g) :: (Object (AudioUnit ch))
else instance audioGraphGeneratorsGiveUp0 ::
  (IsSymbol k, AudioGraphGenerators tail graph ch) =>
  AudioGraphGenerators (Cons k v tail) graph ch where
  generators _ g = generators (RLProxy :: RLProxy tail) g

-----------------
---------
class ReflectSymbols (sl :: SList) where
  reflectSymbols :: SLProxy sl -> List String

instance reflectSymbolsNil :: ReflectSymbols SNil where
  reflectSymbols _ = Nil
else instance reflectSymbolsCons :: (IsSymbol h, ReflectSymbols t) => ReflectSymbols (h :/ t) where
  reflectSymbols _ = reflectSymbol (SProxy :: SProxy h) : reflectSymbols (SLProxy :: SLProxy t)
else instance reflectSymbolsBad :: ReflectSymbols x where
  reflectSymbols _ = Nil

class AsAggregator v where
  asAggregator :: v -> Maybe (Tuple AudioGraphAggregator (Set String))

instance asAggregatorJust :: (ReflectSymbols k) => AsAggregator (Tuple (AudioGraphAggregator) (SLProxy k)) where
  asAggregator (Tuple a b) = Just (Tuple a (DS.fromFoldable $ reflectSymbols b))
else instance asAggregatorOther :: AsAggregator x where
  asAggregator _ = Nothing

class AsAggregatorObject (iter :: RowList) (aggregators :: # Type) where
  asAggregatorObject :: RLProxy iter -> Record aggregators -> Object (Tuple (AudioGraphAggregator) (Set String))

instance asAggregatorObjectCons ::
  (IsSymbol k, AsAggregatorObject tail graph, AsAggregator v) =>
  AsAggregatorObject (Cons k v tail) graph where
  asAggregatorObject _ g = let asStr = reflectSymbol (SProxy :: SProxy k) in O.union (maybe O.empty (O.singleton asStr) $ asAggregator ((unsafeGet asStr g) :: v)) (asAggregatorObject (RLProxy :: RLProxy tail) g)
else instance asAggregatorObjectNil :: AsAggregatorObject Nil g where
  asAggregatorObject _ g = O.empty

class AudioGraphAggregators (iter :: RowList) (graph :: # Type) where
  aggregators :: RLProxy iter -> Record graph -> O.Object (Tuple (AudioGraphAggregator) (Set String))

instance audioGraphAggregatorsNil :: AudioGraphAggregators Nil graph where
  aggregators _ g = O.empty
else instance audioGraphAggregatorsCons ::
  (RowToList g gl, AsAggregatorObject gl g, AudioGraphAggregators tail graph) =>
  AudioGraphAggregators (Cons "aggregators" (Record g) tail) graph where
  aggregators _ g = O.union (asAggregatorObject (RLProxy :: RLProxy gl) ((unsafeGet "aggregators" g) :: (Record g))) (aggregators (RLProxy :: RLProxy tail) g)
else instance audioGraphAggregatorsGiveUp0 ::
  (IsSymbol k, AudioGraphAggregators tail graph) =>
  AudioGraphAggregators (Cons k v tail) graph where
  aggregators _ g = aggregators (RLProxy :: RLProxy tail) g

class AudioGraphToObject (graph :: # Type) ch where
  toObject :: Record graph -> AudioGraph ch

instance audioGraphToObject ::
  ( ValidAudioGraph graph ch
  , RowToList graph gl
  , AudioGraphGenerators gl graph ch
  , AudioGraphProcessors gl graph
  , AudioGraphAggregators gl graph
  ) =>
  AudioGraphToObject graph ch where
  toObject g =
    { generators: generators (RLProxy :: RLProxy gl) g
    , processors: processors (RLProxy :: RLProxy gl) g
    , aggregators: aggregators (RLProxy :: RLProxy gl) g
    }

graph ::
  forall (graph :: # Type) ch.
  Pos ch =>
  AudioGraphToObject graph ch =>
  Record graph ->
  AudioUnit ch
graph g = Graph Nothing (toObject g)

graph_ ::
  forall (graph :: # Type) ch.
  Pos ch =>
  AudioGraphToObject graph ch =>
  String ->
  Record graph ->
  AudioUnit ch
graph_ s g = Graph (Just s) (toObject g)

--------------------------------------------
--------------------------------------------
--------------------------------------------
g'audioWorkletAggregator ::
  String ->
  Object Number ->
  AudioGraphAggregator
g'audioWorkletAggregator handle n = GAudioWorkletAggregator Nothing handle (map ap_ n)

g'audioWorkletAggregator_ ::
  String ->
  String ->
  Object Number ->
  AudioGraphAggregator
g'audioWorkletAggregator_ s handle n = GAudioWorkletAggregator (Just s) handle (map ap_ n)

g'audioWorkletAggregatorT ::
  String ->
  Object (AudioParameter Number) ->
  AudioGraphAggregator
g'audioWorkletAggregatorT handle n = GAudioWorkletAggregator Nothing handle n

g'audioWorkletAggregatorT_ ::
  String ->
  String ->
  Object (AudioParameter Number) ->
  AudioGraphAggregator
g'audioWorkletAggregatorT_ s handle n = GAudioWorkletAggregator (Just s) handle n

g'audioWorkletProcessor ::
  String ->
  Object Number ->
  AudioGraphProcessor
g'audioWorkletProcessor handle n = GAudioWorkletProcessor Nothing handle (map ap_ n)

g'audioWorkletProcessor_ ::
  String ->
  String ->
  Object Number ->
  AudioGraphProcessor
g'audioWorkletProcessor_ s handle n = GAudioWorkletProcessor (Just s) handle (map ap_ n)

g'audioWorkletProcessorT ::
  String ->
  Object (AudioParameter Number) ->
  AudioGraphProcessor
g'audioWorkletProcessorT handle n = GAudioWorkletProcessor Nothing handle n

g'audioWorkletProcessorT_ ::
  String ->
  String ->
  Object (AudioParameter Number) ->
  AudioGraphProcessor
g'audioWorkletProcessorT_ s handle n = GAudioWorkletProcessor (Just s) handle n

g'iirFilter ::
  forall len.
  Pos len =>
  LtEq len D20 =>
  Vec len Number ->
  Vec len Number ->
  AudioGraphProcessor
g'iirFilter a b = GIIRFilter Nothing (V.toArray a) (V.toArray b)

g'iirFilter_ ::
  forall len.
  Pos len =>
  LtEq len D20 =>
  String ->
  Vec len Number ->
  Vec len Number ->
  AudioGraphProcessor
g'iirFilter_ s a b = GIIRFilter (Just s) (V.toArray a) (V.toArray b)

g'lowpass :: Number -> Number -> AudioGraphProcessor
g'lowpass a b = GLowpass Nothing (ap_ a) (ap_ b)

g'lowpass_ :: String -> Number -> Number -> AudioGraphProcessor
g'lowpass_ s a b = GLowpass (Just s) (ap_ a) (ap_ b)

g'lowpassT :: AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'lowpassT a b = GLowpass Nothing a b

g'lowpassT_ :: String -> AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'lowpassT_ s a b = GLowpass (Just s) a b

g'highpass :: Number -> Number -> AudioGraphProcessor
g'highpass a b = GHighpass Nothing (ap_ a) (ap_ b)

g'highpass_ :: String -> Number -> Number -> AudioGraphProcessor
g'highpass_ s a b = GHighpass (Just s) (ap_ a) (ap_ b)

g'highpassT :: AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'highpassT a b = GHighpass Nothing a b

g'highpassT_ :: String -> AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'highpassT_ s a b = GHighpass (Just s) a b

g'bandpass :: Number -> Number -> AudioGraphProcessor
g'bandpass a b = GBandpass Nothing (ap_ a) (ap_ b)

g'bandpass_ :: String -> Number -> Number -> AudioGraphProcessor
g'bandpass_ s a b = GBandpass (Just s) (ap_ a) (ap_ b)

g'bandpassT :: AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'bandpassT a b = GBandpass Nothing a b

g'bandpassT_ :: String -> AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'bandpassT_ s a b = GBandpass (Just s) a b

g'lowshelf :: Number -> Number -> AudioGraphProcessor
g'lowshelf a b = GLowshelf Nothing (ap_ a) (ap_ b)

g'lowshelf_ :: String -> Number -> Number -> AudioGraphProcessor
g'lowshelf_ s a b = GLowshelf (Just s) (ap_ a) (ap_ b)

g'lowshelfT :: AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'lowshelfT a b = GLowshelf Nothing a b

g'lowshelfT_ :: String -> AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'lowshelfT_ s a b = GLowshelf (Just s) a b

g'highshelf :: Number -> Number -> AudioGraphProcessor
g'highshelf a b = GHighshelf Nothing (ap_ a) (ap_ b)

g'highshelf_ :: String -> Number -> Number -> AudioGraphProcessor
g'highshelf_ s a b = GHighshelf (Just s) (ap_ a) (ap_ b)

g'highshelfT :: AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'highshelfT a b = GHighshelf Nothing a b

g'highshelfT_ :: String -> AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'highshelfT_ s a b = GHighshelf (Just s) a b

g'peaking :: Number -> Number -> Number -> AudioGraphProcessor
g'peaking a b c = GPeaking Nothing (ap_ a) (ap_ b) (ap_ c)

g'peaking_ :: String -> Number -> Number -> Number -> AudioGraphProcessor
g'peaking_ s a b c = GPeaking (Just s) (ap_ a) (ap_ b) (ap_ c)

g'peakingT :: AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'peakingT a b c = GPeaking Nothing a b c

g'peakingT_ :: String -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'peakingT_ s a b c = GPeaking (Just s) a b c

g'notch :: Number -> Number -> AudioGraphProcessor
g'notch a b = GNotch Nothing (ap_ a) (ap_ b)

g'notch_ :: String -> Number -> Number -> AudioGraphProcessor
g'notch_ s a b = GNotch (Just s) (ap_ a) (ap_ b)

g'notchT :: AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'notchT a b = GNotch Nothing a b

g'notchT_ :: String -> AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'notchT_ s a b = GNotch (Just s) a b

g'allpass :: Number -> Number -> AudioGraphProcessor
g'allpass a b = GAllpass Nothing (ap_ a) (ap_ b)

g'allpass_ :: String -> Number -> Number -> AudioGraphProcessor
g'allpass_ s a b = GAllpass (Just s) (ap_ a) (ap_ b)

g'allpassT :: AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'allpassT a b = GAllpass Nothing a b

g'allpassT_ :: String -> AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'allpassT_ s a b = GAllpass (Just s) a b

g'convolver ::
  forall ch.
  Pos ch =>
  String ->
  AudioGraphProcessor
g'convolver handle = GConvolver Nothing handle

g'convolver_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  AudioGraphProcessor
g'convolver_ s handle = GConvolver (Just s) handle

g'dynamicsCompressor ::
  forall ch.
  Pos ch =>
  Number -> Number -> Number -> Number -> Number -> AudioGraphProcessor
g'dynamicsCompressor a b c d e = GDynamicsCompressor Nothing (ap_ a) (ap_ b) (ap_ c) (ap_ d) (ap_ e)

g'dynamicsCompressor_ ::
  forall ch.
  Pos ch =>
  String -> Number -> Number -> Number -> Number -> Number -> AudioGraphProcessor
g'dynamicsCompressor_ s a b c d e = GDynamicsCompressor (Just s) (ap_ a) (ap_ b) (ap_ c) (ap_ d) (ap_ e)

g'dynamicsCompressorT ::
  forall ch.
  Pos ch =>
  AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'dynamicsCompressorT a b c d e = GDynamicsCompressor Nothing a b c d e

g'dynamicsCompressorT_ ::
  forall ch.
  Pos ch =>
  String ->
  AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioParameter Number -> AudioGraphProcessor
g'dynamicsCompressorT_ s a b c d e = GDynamicsCompressor (Just s) a b c d e

g'waveShaper ::
  forall ch.
  Pos ch =>
  String ->
  Oversample ->
  AudioGraphProcessor
g'waveShaper handle = GWaveShaper Nothing handle

g'waveShaper_ ::
  forall ch.
  Pos ch =>
  String ->
  String ->
  Oversample ->
  AudioGraphProcessor
g'waveShaper_ s handle = GWaveShaper (Just s) handle

g'panner :: Number -> AudioGraphProcessor
g'panner n = GStereoPanner Nothing (ap_ n)

g'panner_ :: String -> Number -> AudioGraphProcessor
g'panner_ s n = GStereoPanner (Just s) (ap_ n)

g'pannerT :: AudioParameter Number -> AudioGraphProcessor
g'pannerT n = GStereoPanner Nothing n

g'pannerT_ :: String -> AudioParameter Number -> AudioGraphProcessor
g'pannerT_ s n = GStereoPanner (Just s) n

g'spatialPanner :: PannerVars' -> AudioGraphProcessor
g'spatialPanner = GPanner Nothing <<< pannerVarsAsAudioParams

g'spatialPanner_ :: String -> PannerVars' -> AudioGraphProcessor
g'spatialPanner_ s = GPanner (Just s) <<< pannerVarsAsAudioParams

g'spatialPannerT :: PannerVars -> AudioGraphProcessor
g'spatialPannerT = GPanner Nothing

g'spatialPannerT_ :: String -> PannerVars -> AudioGraphProcessor
g'spatialPannerT_ s = GPanner (Just s)

g'mul :: AudioGraphAggregator
g'mul = GMul Nothing

g'mul_ :: String -> AudioGraphAggregator
g'mul_ s = GMul (Just s)

g'add :: AudioGraphAggregator
g'add = GAdd Nothing

g'add_ :: String -> AudioGraphAggregator
g'add_ s = GAdd (Just s)

g'delay :: Number -> AudioGraphProcessor
g'delay n = GDelay Nothing (ap_ n)

g'delay_ :: String -> Number -> AudioGraphProcessor
g'delay_ s n = GDelay (Just s) (ap_ n)

g'delayT :: AudioParameter Number -> AudioGraphProcessor
g'delayT n = GDelay Nothing n

g'delayT_ :: String -> AudioParameter Number -> AudioGraphProcessor
g'delayT_ s n = GDelay (Just s) n

g'gain :: Number -> AudioGraphAggregator
g'gain n = GGain Nothing (ap_ n)

g'gain_ :: String -> Number -> AudioGraphAggregator
g'gain_ s n = GGain (Just s) (ap_ n)

g'gainT :: AudioParameter Number -> AudioGraphAggregator
g'gainT n = GGain Nothing n

g'gainT_ :: String -> AudioParameter Number -> AudioGraphAggregator
g'gainT_ s n = GGain (Just s) n

--------------------------------------------
--------------------------------------------
--------------------------------------------
--------------------------------------------
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

ucomp (AudioWorkletGenerator' n0 _) (AudioWorkletGenerator' n1 _) = n0 == n1

ucomp (AudioWorkletProcessor' n0 _) (AudioWorkletProcessor' n1 _) = n0 == n1

ucomp (AudioWorkletAggregator' n0 _) (AudioWorkletAggregator' n1 _) = n0 == n1

ucomp (Play' s0 _) (Play' s1 _) = s0 == s1

ucomp (PlayBuf' s0 _ _) (PlayBuf' s1 _ _) = s0 == s1

ucomp (LoopBuf' s0 _ _ _) (LoopBuf' s1 _ _ _) = s0 == s1

ucomp (Lowpass' _ _) (Lowpass' _ _) = true

ucomp (IIRFilter' a b) (IIRFilter' x y) = a == x && b == y

ucomp (Highpass' _ _) (Highpass' _ _) = true

ucomp (Bandpass' _ _) (Bandpass' _ _) = true

ucomp (Lowshelf' _ _) (Lowshelf' _ _) = true

ucomp (Highshelf' _ _) (Highshelf' _ _) = true

ucomp (Peaking' _ _ _) (Peaking' _ _ _) = true

ucomp (Notch' _ _) (Notch' _ _) = true

ucomp (Allpass' _ _) (Allpass' _ _) = true

ucomp (Convolver' s0) (Convolver' s1) = s0 == s1

ucomp (DynamicsCompressor' _ _ _ _ _) (DynamicsCompressor' _ _ _ _ _) = true

ucomp (SawtoothOsc' _) (SawtoothOsc' _) = true

ucomp (TriangleOsc' _) (TriangleOsc' _) = true

ucomp (PeriodicOsc' _ s0) (PeriodicOsc' _ s1) = s0 == s1

ucomp (WaveShaper' s0 _) (WaveShaper' s1 _) = s0 == s1

ucomp (Dup') (Dup') = true

ucomp (SinOsc' _) (SinOsc' _) = true

ucomp (SquareOsc' _) (SquareOsc' _) = true

ucomp (Splitter' _) (Splitter' _) = true

ucomp (StereoPanner' _) (StereoPanner' _) = true

ucomp (Panner' x) (Panner' y) = x.distanceModel == y.distanceModel && x.panningModel == y.panningModel

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
        , name: Nothing
        , status: Off
        , next: DS.empty
        , chan: t.chan
        , head: (-42) -- ugh, hackish
        , prev: DS.empty
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

nextDegree :: Map Int Int -> Array Int
nextDegree m = (A.fromFoldable <<< M.keys) $ M.filter (_ == 1) m

glpMIN = 1 :: Int

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

-- | the base time to set at
-- | instructions
-- | audio context
-- | audio stream (ie microphone)
-- | audio sources
-- | audio units
-- | audio units
foreign import touchAudio ::
  forall microphone track buffer floatArray periodicWave.
  FFIPredicates ->
  Number ->
  Array Instruction ->
  AudioContext ->
  AudioInfo (Object microphone) (Object track) (Object buffer) (Object floatArray) (Object periodicWave) ->
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
    , instructionSet :: Array Instruction
    }

-- "Assembly like" instruction
-- for sequential programming with audio units
-- treating them as pointers.
data Instruction
  = Stop Int
  | DisconnectFrom Int Int -- id id
  | ConnectTo Int Int (Maybe (Tuple Int Int)) -- id id channelConnections
  | Shuffle (Array (Tuple Int Int)) -- id id, shuffles the map
  | NewUnit Int AudioUnit'' (Maybe Int) (Maybe String) (Maybe Number) (Maybe Number) (Maybe (Tuple (Array Number) (Array Number))) -- new audio unit, maybe with channel info, maybe with a source, maybe with a start time, maybe with an offset, maybe with FF and FB info for IIR filter
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
  | SetCustomParam Int String Number Number -- for audio worklet nodes
  | SetConeInnerAngle Int Number
  | SetConeOuterAngle Int Number
  | SetConeOuterGain Int Number
  | SetDistanceModel Int String
  | SetMaxDistance Int Number
  | SetOrientationX Int Number Number
  | SetOrientationY Int Number Number
  | SetOrientationZ Int Number Number
  | SetPanningModel Int String
  | SetPositionX Int Number Number
  | SetPositionY Int Number Number
  | SetPositionZ Int Number Number
  | SetRefDistance Int Number
  | SetRolloffFactor Int Number

isStop_ :: Instruction -> Boolean
isStop_ (Stop _) = true

isStop_ _ = false

isDisconnectFrom_ :: Instruction -> Boolean
isDisconnectFrom_ (DisconnectFrom _ _) = true

isDisconnectFrom_ _ = false

isConnectTo_ :: Instruction -> Boolean
isConnectTo_ (ConnectTo _ _ _) = true

isConnectTo_ _ = false

isShuffle_ :: Instruction -> Boolean
isShuffle_ (Shuffle _) = true

isShuffle_ _ = false

isNewUnit_ :: Instruction -> Boolean
isNewUnit_ (NewUnit _ _ _ _ _ _ _) = true

isNewUnit_ _ = false

isSetFrequency_ :: Instruction -> Boolean
isSetFrequency_ (SetFrequency _ _ _) = true

isSetFrequency_ _ = false

isSetThreshold_ :: Instruction -> Boolean
isSetThreshold_ (SetThreshold _ _ _) = true

isSetThreshold_ _ = false

isSetKnee_ :: Instruction -> Boolean
isSetKnee_ (SetKnee _ _ _) = true

isSetKnee_ _ = false

isSetRatio_ :: Instruction -> Boolean
isSetRatio_ (SetRatio _ _ _) = true

isSetRatio_ _ = false

isSetAttack_ :: Instruction -> Boolean
isSetAttack_ (SetAttack _ _ _) = true

isSetAttack_ _ = false

isSetRelease_ :: Instruction -> Boolean
isSetRelease_ (SetRelease _ _ _) = true

isSetRelease_ _ = false

isSetBuffer_ :: Instruction -> Boolean
isSetBuffer_ (SetBuffer _ _ _) = true

isSetBuffer_ _ = false

isSetQ_ :: Instruction -> Boolean
isSetQ_ (SetQ _ _ _) = true

isSetQ_ _ = false

isSetPlaybackRate_ :: Instruction -> Boolean
isSetPlaybackRate_ (SetPlaybackRate _ _ _) = true

isSetPlaybackRate_ _ = false

isSetPeriodicWave_ :: Instruction -> Boolean
isSetPeriodicWave_ (SetPeriodicWave _ _ _) = true

isSetPeriodicWave_ _ = false

isSetCurve_ :: Instruction -> Boolean
isSetCurve_ (SetCurve _ _) = true

isSetCurve_ _ = false

isSetOversample_ :: Instruction -> Boolean
isSetOversample_ (SetOversample _ _) = true

isSetOversample_ _ = false

isSetLoopStart_ :: Instruction -> Boolean
isSetLoopStart_ (SetLoopStart _ _) = true

isSetLoopStart_ _ = false

isSetLoopEnd_ :: Instruction -> Boolean
isSetLoopEnd_ (SetLoopEnd _ _) = true

isSetLoopEnd_ _ = false

isSetPan_ :: Instruction -> Boolean
isSetPan_ (SetPan _ _ _) = true

isSetPan_ _ = false

isSetGain_ :: Instruction -> Boolean
isSetGain_ (SetGain _ _ _) = true

isSetGain_ _ = false

isSetDelay_ :: Instruction -> Boolean
isSetDelay_ (SetDelay _ _ _) = true

isSetDelay_ _ = false

isSetOffset_ :: Instruction -> Boolean
isSetOffset_ (SetOffset _ _ _) = true

isSetOffset_ _ = false

isSetCustomParam_ :: Instruction -> Boolean
isSetCustomParam_ (SetCustomParam _ _ _ _) = true

isSetCustomParam_ _ = false

isSetConeInnerAngle_ :: Instruction -> Boolean
isSetConeInnerAngle_ (SetConeInnerAngle _ _) = true

isSetConeInnerAngle_ _ = false

isSetConeOuterAngle_ :: Instruction -> Boolean
isSetConeOuterAngle_ (SetConeOuterAngle _ _) = true

isSetConeOuterAngle_ _ = false

isSetConeOuterGain_ :: Instruction -> Boolean
isSetConeOuterGain_ (SetConeOuterGain _ _) = true

isSetConeOuterGain_ _ = false

isSetDistanceModel_ :: Instruction -> Boolean
isSetDistanceModel_ (SetDistanceModel _ _) = true

isSetDistanceModel_ _ = false

isSetMaxDistance_ :: Instruction -> Boolean
isSetMaxDistance_ (SetMaxDistance _ _) = true

isSetMaxDistance_ _ = false

isSetOrientationX_ :: Instruction -> Boolean
isSetOrientationX_ (SetOrientationX _ _ _) = true

isSetOrientationX_ _ = false

isSetOrientationY_ :: Instruction -> Boolean
isSetOrientationY_ (SetOrientationY _ _ _) = true

isSetOrientationY_ _ = false

isSetOrientationZ_ :: Instruction -> Boolean
isSetOrientationZ_ (SetOrientationZ _ _ _) = true

isSetOrientationZ_ _ = false

isSetPanningModel_ :: Instruction -> Boolean
isSetPanningModel_ (SetPanningModel _ _) = true

isSetPanningModel_ _ = false

isSetPositionX_ :: Instruction -> Boolean
isSetPositionX_ (SetPositionX _ _ _) = true

isSetPositionX_ _ = false

isSetPositionY_ :: Instruction -> Boolean
isSetPositionY_ (SetPositionY _ _ _) = true

isSetPositionY_ _ = false

isSetPositionZ_ :: Instruction -> Boolean
isSetPositionZ_ (SetPositionZ _ _ _) = true

isSetPositionZ_ _ = false

isSetRefDistance_ :: Instruction -> Boolean
isSetRefDistance_ (SetRefDistance _ _) = true

isSetRefDistance_ _ = false

isSetRolloffFactor_ :: Instruction -> Boolean
isSetRolloffFactor_ (SetRolloffFactor _ _) = true

isSetRolloffFactor_ _ = false

type FFIPredicates
  = { justly :: forall a. a -> Maybe a
    , tupply :: forall a b. a -> b -> Tuple a b
    , isNothing :: forall a. Maybe a -> Boolean
    , isMicrophone :: (AudioUnit'' -> Boolean)
    , isAudioWorkletGenerator :: (AudioUnit'' -> Boolean)
    , isAudioWorkletProcessor :: (AudioUnit'' -> Boolean)
    , isAudioWorkletAggregator :: (AudioUnit'' -> Boolean)
    , isPlay :: (AudioUnit'' -> Boolean)
    , isPlayBuf :: (AudioUnit'' -> Boolean)
    , isLoopBuf :: (AudioUnit'' -> Boolean)
    , isIIRFilter :: (AudioUnit'' -> Boolean)
    , isLowpass :: (AudioUnit'' -> Boolean)
    , isHighpass :: (AudioUnit'' -> Boolean)
    , isBandpass :: (AudioUnit'' -> Boolean)
    , isLowshelf :: (AudioUnit'' -> Boolean)
    , isHighshelf :: (AudioUnit'' -> Boolean)
    , isPeaking :: (AudioUnit'' -> Boolean)
    , isNotch :: (AudioUnit'' -> Boolean)
    , isAllpass :: (AudioUnit'' -> Boolean)
    , isConvolver :: (AudioUnit'' -> Boolean)
    , isDynamicsCompressor :: (AudioUnit'' -> Boolean)
    , isSawtoothOsc :: (AudioUnit'' -> Boolean)
    , isTriangleOsc :: (AudioUnit'' -> Boolean)
    , isPeriodicOsc :: (AudioUnit'' -> Boolean)
    , isWaveShaper :: (AudioUnit'' -> Boolean)
    , isDup :: (AudioUnit'' -> Boolean)
    , isSinOsc :: (AudioUnit'' -> Boolean)
    , isSquareOsc :: (AudioUnit'' -> Boolean)
    , isSplitter :: (AudioUnit'' -> Boolean)
    , isStereoPanner :: (AudioUnit'' -> Boolean)
    , isPanner :: (AudioUnit'' -> Boolean)
    , isMul :: (AudioUnit'' -> Boolean)
    , isAdd :: (AudioUnit'' -> Boolean)
    , isSwap :: (AudioUnit'' -> Boolean)
    , isMerger :: (AudioUnit'' -> Boolean)
    , isConstant :: (AudioUnit'' -> Boolean)
    , isDelay :: (AudioUnit'' -> Boolean)
    , isGain :: (AudioUnit'' -> Boolean)
    , isSpeaker :: (AudioUnit'' -> Boolean)
    , isNoSound :: (AudioUnit'' -> Boolean)
    , isSplitRes :: (AudioUnit'' -> Boolean)
    , isDupRes :: (AudioUnit'' -> Boolean)
    , isStop :: (Instruction -> Boolean)
    , isDisconnectFrom :: (Instruction -> Boolean)
    , isConnectTo :: (Instruction -> Boolean)
    , isShuffle :: (Instruction -> Boolean)
    , isNewUnit :: (Instruction -> Boolean)
    , isSetFrequency :: (Instruction -> Boolean)
    , isSetThreshold :: (Instruction -> Boolean)
    , isSetKnee :: (Instruction -> Boolean)
    , isSetRatio :: (Instruction -> Boolean)
    , isSetAttack :: (Instruction -> Boolean)
    , isSetRelease :: (Instruction -> Boolean)
    , isSetBuffer :: (Instruction -> Boolean)
    , isSetQ :: (Instruction -> Boolean)
    , isSetPlaybackRate :: (Instruction -> Boolean)
    , isSetPeriodicWave :: (Instruction -> Boolean)
    , isSetCurve :: (Instruction -> Boolean)
    , isSetOversample :: (Instruction -> Boolean)
    , isSetLoopStart :: (Instruction -> Boolean)
    , isSetLoopEnd :: (Instruction -> Boolean)
    , isSetPan :: (Instruction -> Boolean)
    , isSetGain :: (Instruction -> Boolean)
    , isSetDelay :: (Instruction -> Boolean)
    , isSetOffset :: (Instruction -> Boolean)
    , isSetCustomParam :: (Instruction -> Boolean)
    , isSetConeInnerAngle :: (Instruction -> Boolean)
    , isSetConeOuterAngle :: (Instruction -> Boolean)
    , isSetConeOuterGain :: (Instruction -> Boolean)
    , isSetDistanceModel :: (Instruction -> Boolean)
    , isSetMaxDistance :: (Instruction -> Boolean)
    , isSetOrientationX :: (Instruction -> Boolean)
    , isSetOrientationY :: (Instruction -> Boolean)
    , isSetOrientationZ :: (Instruction -> Boolean)
    , isSetPanningModel :: (Instruction -> Boolean)
    , isSetPositionX :: (Instruction -> Boolean)
    , isSetPositionY :: (Instruction -> Boolean)
    , isSetPositionZ :: (Instruction -> Boolean)
    , isSetRefDistance :: (Instruction -> Boolean)
    , isSetRolloffFactor :: (Instruction -> Boolean)
    }

toFFI =
  { justly: Just
  , tupply: Tuple
  , isNothing: isNothing
  , isMicrophone: isMicrophone_
  , isAudioWorkletGenerator: isAudioWorkletGenerator_
  , isAudioWorkletProcessor: isAudioWorkletProcessor_
  , isAudioWorkletAggregator: isAudioWorkletAggregator_
  , isPlay: isPlay_
  , isPlayBuf: isPlayBuf_
  , isLoopBuf: isLoopBuf_
  , isIIRFilter: isIIRFilter_
  , isLowpass: isLowpass_
  , isHighpass: isHighpass_
  , isBandpass: isBandpass_
  , isLowshelf: isLowshelf_
  , isHighshelf: isHighshelf_
  , isPeaking: isPeaking_
  , isNotch: isNotch_
  , isAllpass: isAllpass_
  , isConvolver: isConvolver_
  , isDynamicsCompressor: isDynamicsCompressor_
  , isSawtoothOsc: isSawtoothOsc_
  , isTriangleOsc: isTriangleOsc_
  , isPeriodicOsc: isPeriodicOsc_
  , isWaveShaper: isWaveShaper_
  , isDup: isDup_
  , isSinOsc: isSinOsc_
  , isSquareOsc: isSquareOsc_
  , isSplitter: isSplitter_
  , isStereoPanner: isStereoPanner_
  , isPanner: isPanner_
  , isMul: isMul_
  , isAdd: isAdd_
  , isSwap: isSwap_
  , isMerger: isMerger_
  , isConstant: isConstant_
  , isDelay: isDelay_
  , isGain: isGain_
  , isSpeaker: isSpeaker_
  , isNoSound: isNoSound_
  , isSplitRes: isSplitRes_
  , isDupRes: isDupRes_
  , isStop: isStop_
  , isDisconnectFrom: isDisconnectFrom_
  , isConnectTo: isConnectTo_
  , isShuffle: isShuffle_
  , isNewUnit: isNewUnit_
  , isSetFrequency: isSetFrequency_
  , isSetThreshold: isSetThreshold_
  , isSetKnee: isSetKnee_
  , isSetRatio: isSetRatio_
  , isSetAttack: isSetAttack_
  , isSetRelease: isSetRelease_
  , isSetBuffer: isSetBuffer_
  , isSetQ: isSetQ_
  , isSetPlaybackRate: isSetPlaybackRate_
  , isSetPeriodicWave: isSetPeriodicWave_
  , isSetCurve: isSetCurve_
  , isSetOversample: isSetOversample_
  , isSetLoopStart: isSetLoopStart_
  , isSetLoopEnd: isSetLoopEnd_
  , isSetPan: isSetPan_
  , isSetGain: isSetGain_
  , isSetDelay: isSetDelay_
  , isSetOffset: isSetOffset_
  , isSetCustomParam: isSetCustomParam_
  , isSetConeInnerAngle: isSetConeInnerAngle_
  , isSetConeOuterAngle: isSetConeOuterAngle_
  , isSetConeOuterGain: isSetConeOuterGain_
  , isSetDistanceModel: isSetDistanceModel_
  , isSetMaxDistance: isSetMaxDistance_
  , isSetOrientationX: isSetOrientationX_
  , isSetOrientationY: isSetOrientationY_
  , isSetOrientationZ: isSetOrientationZ_
  , isSetPanningModel: isSetPanningModel_
  , isSetPositionX: isSetPositionX_
  , isSetPositionY: isSetPositionY_
  , isSetPositionZ: isSetPositionZ_
  , isSetRefDistance: isSetRefDistance_
  , isSetRolloffFactor: isSetRolloffFactor_
  } ::
    FFIPredicates

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

sourceConstructor (AudioWorkletGenerator' s _) = Just s

sourceConstructor (AudioWorkletProcessor' s _) = Just s

sourceConstructor (AudioWorkletAggregator' s _) = Just s

sourceConstructor (PlayBuf' s _ _) = Just s

sourceConstructor (LoopBuf' s _ _ _) = Just s

sourceConstructor (PeriodicOsc' _ s) = Just s

sourceConstructor (WaveShaper' s _) = Just s

sourceConstructor (Convolver' s) = Just s

sourceConstructor _ = Nothing

offsetConstructor :: AudioUnit' -> Maybe Number
offsetConstructor (PlayBuf' _ _ o) = Just (apP o)

offsetConstructor _ = Nothing

iirCoefConstructor :: AudioUnit' -> Maybe (Tuple (Array Number) (Array Number))
iirCoefConstructor (IIRFilter' x y) = Just (Tuple x y)

iirCoefConstructor _ = Nothing

startConstructor :: AudioUnit' -> Maybe Number
startConstructor (Play' n timingHack) = Just timingHack

startConstructor (PlayBuf' _ n _) = Just (apT n)

startConstructor (LoopBuf' _ n _ _) = Just (apT n)

startConstructor (SawtoothOsc' n) = Just (apT n)

startConstructor (TriangleOsc' n) = Just (apT n)

startConstructor (PeriodicOsc' n _) = Just (apT n)

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

describeConnection :: Reconcilable -> Reconcilable -> Map Int Int -> Array (Tuple Int Int)
describeConnection start end passage =
  (A.fromFoldable <<< M.keys)
    ( M.filter
        ( \(Tuple f s) ->
            fromMaybe false
              ((not <<< member s <<< _.next) <$> (M.lookup f end.flat))
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
                                  $ DL.fromFoldable au.next
                            )
                            (M.values start.flat)
                        )
                )
            )
        )
    )

isGen :: AudioUnit' -> Boolean
isGen (Microphone') = true

-- NB: play is not a gen as there is no stop method on the source element
-- this is a bad, as it will keep playing in the background after
-- disconnected from the speaker
-- find a way to disconnect
isGen (PlayBuf' _ _ _) = true

isGen (LoopBuf' _ _ _ _) = true

isGen (SawtoothOsc' _) = true

isGen (TriangleOsc' _) = true

isGen (PeriodicOsc' _ _) = true

isGen (SinOsc' _) = true

isGen (SquareOsc' _) = true

isGen (Constant' _) = true

isGen _ = false

scp :: Int -> Object (AudioParameter Number) -> Object (AudioParameter Number) -> Array Instruction
scp i n nx =
  join
    $ map
        ( \(Tuple k0 v0) ->
            if ( fromMaybe true
                $ map (napeq v0) (O.lookup k0 nx)
            ) then
              [ SetCustomParam i k0 (apP v0) (apT v0) ]
            else
              []
        )
        (O.toUnfoldable n)

reconciliationToInstructionSet :: Reconciled' -> Reconciled
reconciliationToInstructionSet { prev, cur, reconciliation } =
  { prev
  , cur
  , reconciliation
  , instructionSet: stop <> disconnect <> (pure shuffle) <> new <> connect <> set
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
  stop :: Array Instruction
  stop =
    map (Stop <<< fst)
      ( A.filter
          (maybe false (isGen <<< _.au) <<< flip M.lookup prev.flat <<< fst)
          $ statusChange (Just Off) (Just On)
      )

  -- shuffle instructions represent the new array that we will make out of the old
  shuffle :: Instruction
  shuffle = Shuffle $ statusChange (Just On) (Just On)

  -- new units that were not in the old array
  new :: Array Instruction
  new =
    ( A.catMaybes
        ( map
            ( \i ->
                map
                  ( \ptr ->
                      (NewUnit i $ au'' ptr.au) (channelConstructor ptr.au)
                        (sourceConstructor ptr.au)
                        (startConstructor ptr.au)
                        (offsetConstructor ptr.au)
                        (iirCoefConstructor ptr.au)
                  )
                  $ M.lookup i cur.flat
            )
            ( A.catMaybes
                ( map
                    ( \k ->
                        M.lookup k reconciliationAsMap
                    )
                    $ (A.fromFoldable <<< M.keys) (M.filter (\i -> i.status == Off) prev.flat)
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

  connect :: Array Instruction
  connect =
    let
      conn = describeConnection cur prev reversedAsMap
    in
      (map (uncurry $ uncurry ConnectTo) $ map (\i -> Tuple i (harmonizeCurrChannels i)) conn)

  setFQFilter i a b x y =
    (if napeq a x then [ SetFrequency i (apP a) (apT a) ] else [])
      <> (if napeq b y then [ SetQ i (apP b) (apT b) ] else [])

  setFilter i a b c x y z =
    (if napeq a x then [ SetFrequency i (apP a) (apT a) ] else [])
      <> (if napeq b y then [ SetQ i (apP b) (apT b) ] else [])
      <> (if napeq c z then [ SetGain i (apP c) (apT c) ] else [])

  setFGFilter i a c x z =
    (if napeq a x then [ SetFrequency i (apP a) (apT a) ] else [])
      <> (if napeq c z then [ SetGain i (apP c) (apT c) ] else [])

  set' i (AudioWorkletGenerator' _ n) (AudioWorkletGenerator' _ nx) = scp i n nx

  set' i (AudioWorkletProcessor' _ n) (AudioWorkletProcessor' _ nx) = scp i n nx

  set' i (AudioWorkletAggregator' _ n) (AudioWorkletAggregator' _ nx) = scp i n nx

  set' i (PlayBuf' _ n _) (PlayBuf' _ nx _) = if napeq n nx then [ SetPlaybackRate i (apP n) (apT n) ] else []

  set' i (LoopBuf' _ n s e) (LoopBuf' _ nx sx ex) =
    (if napeq n nx then [ SetPlaybackRate i (apP n) (apT n) ] else [])
      <> (if s /= sx then [ SetLoopStart i s ] else [])
      <> (if e /= ex then [ SetLoopEnd i e ] else [])

  set' i (Lowpass' a b) (Lowpass' x y) = setFQFilter i a b x y

  set' i (Highpass' a b) (Highpass' x y) = setFQFilter i a b x y

  set' i (Bandpass' a b) (Bandpass' x y) = setFQFilter i a b x y

  set' i (Allpass' a b) (Allpass' x y) = setFQFilter i a b x y

  set' i (Highshelf' a c) (Highshelf' x z) = setFGFilter i a c x z

  set' i (Lowshelf' a c) (Lowshelf' x z) = setFGFilter i a c x z

  set' i (Peaking' a b c) (Peaking' x y z) = setFilter i a b c x y z

  set' i (Notch' a b) (Notch' x y) = setFQFilter i a b x y

  set' i (DynamicsCompressor' a b c d e) (DynamicsCompressor' v w x y z) =
    (if napeq a v then [ SetThreshold i (apP a) (apT a) ] else [])
      <> (if napeq b w then [ SetKnee i (apP b) (apT b) ] else [])
      <> (if napeq c x then [ SetRatio i (apP c) (apT c) ] else [])
      <> (if napeq d y then [ SetAttack i (apP d) (apT d) ] else [])
      <> (if napeq e z then [ SetRelease i (apP e) (apT e) ] else [])

  set' i (SinOsc' n) (SinOsc' nx) = if napeq n nx then [ SetFrequency i (apP n) (apT n) ] else []

  set' i (SquareOsc' n) (SquareOsc' nx) = if napeq n nx then [ SetFrequency i (apP n) (apT n) ] else []

  set' i (SawtoothOsc' n) (SawtoothOsc' nx) = if napeq n nx then [ SetFrequency i (apP n) (apT n) ] else []

  set' i (TriangleOsc' n) (TriangleOsc' nx) = if napeq n nx then [ SetFrequency i (apP n) (apT n) ] else []

  set' i (PeriodicOsc' n _) (PeriodicOsc' nx _) = if napeq n nx then [ SetFrequency i (apP n) (apT n) ] else []

  set' i (WaveShaper' _ o) (WaveShaper' _ ox) =
    if o /= ox then
      [ SetOversample i
          $ os2s o
      ]
    else
      []

  set' i (StereoPanner' n) (StereoPanner' nx) = if napeq n nx then [ SetPan i (apP n) (apT n) ] else []

  set' i (Panner' n) (Panner' nx) =
    ( ( if napeq n.coneInnerAngle nx.coneInnerAngle then
          [ SetConeInnerAngle i (apP n.coneInnerAngle)
          ]
        else
          []
      )
        <> ( if napeq n.coneOuterAngle nx.coneOuterAngle then
              [ SetConeOuterAngle i (apP n.coneOuterAngle)
              ]
            else
              []
          )
        <> ( if napeq n.coneOuterGain nx.coneOuterGain then
              [ SetConeOuterGain i (apP n.coneOuterGain)
              ]
            else
              []
          )
        <> ( if n.distanceModel /= nx.distanceModel then
              [ SetDistanceModel i (dm2str n.distanceModel)
              ]
            else
              []
          )
        <> ( if napeq n.maxDistance nx.maxDistance then
              [ SetMaxDistance i (apP n.maxDistance)
              ]
            else
              []
          )
        <> ( if napeq n.orientationX nx.orientationX then
              [ SetOrientationX i (apP n.orientationX) (apT n.orientationX)
              ]
            else
              []
          )
        <> ( if napeq n.orientationY nx.orientationY then
              [ SetOrientationY i (apP n.orientationY) (apT n.orientationY)
              ]
            else
              []
          )
        <> ( if napeq n.orientationZ nx.orientationZ then
              [ SetOrientationZ i (apP n.orientationZ) (apT n.orientationZ)
              ]
            else
              []
          )
        <> ( if n.panningModel /= nx.panningModel then
              [ SetPanningModel i (pm2str n.panningModel)
              ]
            else
              []
          )
        <> ( if napeq n.positionX nx.positionX then
              [ SetPositionX i (apP n.positionX) (apT n.positionX)
              ]
            else
              []
          )
        <> ( if napeq n.positionY nx.positionY then
              [ SetPositionY i (apP n.positionY) (apT n.positionY)
              ]
            else
              []
          )
        <> ( if napeq n.positionZ nx.positionZ then
              [ SetPositionZ i (apP n.positionZ) (apT n.positionZ)
              ]
            else
              []
          )
        <> ( if napeq n.refDistance nx.refDistance then
              [ SetRefDistance i (apP n.refDistance)
              ]
            else
              []
          )
        <> ( if napeq n.rolloffFactor nx.rolloffFactor then
              [ SetRolloffFactor i (apP n.rolloffFactor)
              ]
            else
              []
          )
    )

  set' i (Constant' n) (Constant' nx) = if napeq n nx then [ SetOffset i (apP n) (apT n) ] else []

  set' i (Delay' n) (Delay' nx) = if napeq n nx then [ SetDelay i (apP n) (apT n) ] else []

  set' i (Gain' n) (Gain' nx) = if napeq n nx then [ SetGain i (apP n) (apT n) ] else []

  set' i _ _ = []

  set :: Array Instruction
  set =
    ( join
        ( map
            ( \v ->
                set' v.ptr v.au
                  (fromMaybe v.au $ (map _.au $ M.lookup v.ptr reversedAsMap >>= flip M.lookup prev.flat))
            )
            (A.filter (\{ status } -> status == On) (A.fromFoldable $ M.values cur.flat))
        )
    )

type AudioInfo microphones tracks buffers floatArrays periodicWaves
  = { microphones :: microphones
    , tracks :: tracks
    , buffers :: buffers
    , floatArrays :: floatArrays
    , periodicWaves :: periodicWaves
    }

-- the reason canvas elements are effectful is because,
-- unlike audio elements in html,
-- canvases can be killed off based on a rendering function, ie in halogen
-- we want to be able to throw if the canvas does not exist
type VisualInfo
  = { canvases :: Object (Effect CanvasElement)
    }

foreign import getAudioClockTime :: AudioContext -> Effect Number

type CanvasInfo'
  = { w :: Number, h :: Number, boundingClientRect :: Rectangle }

newtype CanvasInfo
  = CanvasInfo CanvasInfo'

dummyCanvasInfo :: CanvasInfo'
dummyCanvasInfo = { w: 0.0, h: 0.0, boundingClientRect: { width: 0.0, height: 0.0, x: 0.0, y: 0.0 } }

type RunInBrowserAudioUnit ch env
  = RunInBrowser (Number -> Behavior (AudioUnit ch)) Unit env

type RunInBrowserAudioUnit_ ch env
  = RunInBrowser_ (Number -> Behavior (AudioUnit ch)) Unit env

type RunInBrowserIAudioUnit accumulator ch env
  = RunInBrowser (accumulator -> Number -> Behavior (IAudioUnit ch accumulator)) accumulator
      env

type RunInBrowserIAudioUnit_ accumulator ch env
  = RunInBrowser_ (accumulator -> Number -> Behavior (IAudioUnit ch accumulator)) accumulator
      env

type RunInBrowserAV accumulator ch env
  = RunInBrowser (accumulator -> CanvasInfo -> Number -> Behavior (AV ch accumulator)) accumulator
      env

type RunInBrowserAV_ accumulator ch env
  = RunInBrowser_ (accumulator -> CanvasInfo -> Number -> Behavior (AV ch accumulator)) accumulator
      env

type RunInBrowserIAnimation accumulator ch env
  = RunInBrowser (accumulator -> CanvasInfo -> Number -> Behavior (IAnimation accumulator)) accumulator
      env

type RunInBrowserIAnimation_ accumulator ch env
  = RunInBrowser_ (accumulator -> CanvasInfo -> Number -> Behavior (IAnimation accumulator)) accumulator
      env

type RunInBrowser callback accumulator env
  = forall microphone track buffer floatArray periodicWave.
    callback ->
    accumulator ->
    AudioContext ->
    EngineInfo ->
    AudioInfo (Object microphone) (Object track) (Object buffer) (Object floatArray) (Object periodicWave) ->
    VisualInfo ->
    Exporter env ->
    Effect (Effect Unit)

type RunInBrowser_ callback accumulator env
  = forall microphone track buffer floatArray periodicWave.
    Effect callback ->
    accumulator ->
    AudioContext ->
    EngineInfo ->
    AudioInfo (Object microphone) (Object track) (Object buffer) (Object floatArray) (Object periodicWave) ->
    VisualInfo ->
    Exporter env ->
    Effect (Effect Unit)

type Exporter env
  = { acquire :: Aff env
    , use :: env -> BuildingBlocks -> Aff Unit
    , release :: env -> Aff Unit
    }

defaultExporter :: Exporter Unit
defaultExporter =
  { acquire: pure unit
  , use: \_ _ -> pure unit
  , release: \_ -> pure unit
  }

type EngineInfo
  = { msBetweenSamples :: Int
    , msBetweenPings :: Int
    , fastforwardLowerBound :: Number
    , rewindUpperBound :: Number
    , initialOffset :: Number
    , doWebAudio :: Boolean
    }

class RunnableMedia callback accumulator env where
  runInBrowser ::
    forall microphone track buffer floatArray periodicWave.
    callback ->
    accumulator ->
    AudioContext ->
    EngineInfo ->
    AudioInfo (Object microphone) (Object track) (Object buffer) (Object floatArray) (Object periodicWave) ->
    VisualInfo ->
    Exporter env ->
    Effect (Effect Unit)

data AV ch accumulator
  = AV (Maybe (AudioUnit ch)) (Maybe Drawing) accumulator

type BuildingBlocks
  = { id :: Int
    , timeStamp :: Number
    , audio :: Maybe (Array Instruction)
    , canvas :: Maybe Drawing
    }

data Time'AudioInstructions
  = Time'AudioInstructions Number (Array Instruction)

data Time'Drawing
  = Time'Drawing Number Drawing

data Animation
  = Animation Drawing

data IAnimation accumulator
  = IAnimation Drawing accumulator

data IAudioUnit ch accumulator
  = IAudioUnit (AudioUnit ch) accumulator

getFirstCanvas :: Object (Effect CanvasElement) -> Maybe (Effect CanvasElement)
getFirstCanvas = map snd <<< A.head <<< O.toUnfoldable

instance soundscapeRunnableMedia :: Pos ch => RunnableMedia (Number -> ABehavior Event (AudioUnit ch)) accumulator env where
  runInBrowser f a ac ei ai vi ex = runInBrowser ((\z wh s -> map (\x -> AV (Just x) Nothing unit) (f s)) :: (Unit -> CanvasInfo -> Number -> ABehavior Event (AV ch Unit))) unit ac ei ai vi ex

instance iSoundscapeRunnableMedia :: Pos ch => RunnableMedia (accumulator -> Number -> ABehavior Event (IAudioUnit ch accumulator)) accumulator env where
  runInBrowser f a ac ei ai vi ex = runInBrowser ((\z wh s -> map (\(IAudioUnit xa xz) -> AV (Just xa) Nothing xz) (f z s)) :: (accumulator -> CanvasInfo -> Number -> ABehavior Event (AV ch accumulator))) a ac ei ai vi ex

instance animationRunnable :: RunnableMedia (CanvasInfo -> Number -> ABehavior Event Animation) accumulator env where
  runInBrowser f a ac ei ai vi ex =
    runInBrowser
      ((\z wh s -> map (\(Animation x) -> (AV (Nothing :: Maybe (AudioUnit D1)) (Just x) z)) (f wh s)) :: (Unit -> CanvasInfo -> Number -> ABehavior Event (AV D1 Unit)))
      unit
      ac
      ei
      ai
      vi
      ex

instance iAnimationRunnable :: RunnableMedia (accumulator -> CanvasInfo -> Number -> ABehavior Event (IAnimation accumulator)) accumulator env where
  runInBrowser f a ac ei ai vi ex =
    runInBrowser
      ( \z wh s ->
          map
            ( \(IAnimation xv xz) ->
                ( AV (Nothing :: Maybe (AudioUnit D1))
                    (Just xv)
                    xz
                )
            )
            (f z wh s)
      )
      a
      ac
      ei
      ai
      vi
      ex

instance avRunnableMedia :: Pos ch => RunnableMedia (accumulator -> CanvasInfo -> Number -> ABehavior Event (AV ch accumulator)) accumulator env where
  runInBrowser scene accumulator ctx engineInfo audioInfo visualInfo exporter = do
    let
      __contract = toNumber $ engineInfo.msBetweenSamples
    __accumulator <- new accumulator
    __totalFromStart <- new 0.0
    ciRef <- new 0
    __exporterQueueRef <- (launchAff $ pure unit) >>= new
    __totalTillProgram <- new 0.0
    __totalProgram <- new 0.0
    __totalPostProgram <- new 0.0
    reconRef <-
      new
        { grouped: M.empty
        , flat: M.empty
        }
    let
      tOffset = engineInfo.initialOffset
    clock <- new 0
    units <- new ([] :: Array Foreign)
    audioClockStart <- getAudioClockTime ctx
    fiber <- launchAff exporter.acquire
    bam <-
      subscribe
        (interval engineInfo.msBetweenPings)
        ( const do
            ct <- read clock
            write (ct + engineInfo.msBetweenSamples) clock
            acc_ <- getAudioClockTime ctx
            curIt <- read ciRef
            write (curIt + 1) ciRef
            clockNow_ <- read clock
            let
              startingPosWRT =
                ( ((toNumber clockNow_ + tOffset) / 1000.0)
                    - (acc_ - audioClockStart)
                )
            if (startingPosWRT > engineInfo.rewindUpperBound) then
              -- reset the clock
              ( do
                  let
                    newV = (clockNow_ - engineInfo.msBetweenSamples)
                  -- log $ "Rewinding " <> show clockNow_ <> " " <> show newV <> " " <> show startingPosWRT
                  write newV clock
              )
            else do
              if (startingPosWRT < engineInfo.fastforwardLowerBound) then
                ( do
                    let
                      newV = clockNow_ + engineInfo.msBetweenSamples
                    log $ "Fastforwarding " <> show clockNow_ <> " " <> show newV <> " " <> show startingPosWRT
                    write newV clock
                )
              else
                pure unit
              __startTime <- map getTime now
              _accNow <- read __accumulator
              let
                __cvsNow = getFirstCanvas visualInfo.canvases
              canvasInfo <-
                maybe (pure dummyCanvasInfo)
                  ( \_cvsNow -> do
                      __r <-
                        try do
                          __cvs <- _cvsNow
                          w <- getCanvasWidth __cvs
                          h <- getCanvasHeight __cvs
                          boundingClientRect <- getBoundingClientRect __cvs
                          pure $ { w, h, boundingClientRect }
                      either (const $ pure dummyCanvasInfo) pure __r
                  )
                  __cvsNow
              let
                timeInSeconds = toNumber ct / 1000.0
              let
                behavior = scene _accNow (CanvasInfo canvasInfo) timeInSeconds
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
                              ( \cvs__ -> do
                                  _ <-
                                    try do
                                      cvs <- cvs__
                                      canvasCtx <- getContext2D cvs
                                      clearRect canvasCtx
                                        { height: canvasInfo.h
                                        , width: canvasInfo.w
                                        , x: 0.0
                                        , y: 0.0
                                        }
                                      render canvasCtx viz
                                  pure unit
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
                                , i: instr.instructionSet
                                }
                            exporterQueueRef <- read __exporterQueueRef
                            launchAff
                              ( do
                                  env <- joinFiber fiber
                                  _ <- joinFiber exporterQueueRef
                                  exporter.use
                                    env
                                    { id: curIt
                                    , timeStamp: timeInSeconds
                                    , audio: (ava >>= (const $ Just instructions.i))
                                    , canvas: avv
                                    }
                              )
                              >>= flip write __exporterQueueRef
                            uts <- read units
                            uts' <-
                              if engineInfo.doWebAudio then
                                touchAudio
                                  toFFI
                                  (audioClockStart + ((toNumber instructions.t + tOffset) / 1000.0))
                                  instructions.i
                                  ctx
                                  audioInfo
                                  uts
                              else
                                pure []
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
    pure
      ( do
          bam
          launchAff_
            ( do
                env <- joinFiber fiber
                exporter.release env
            )
      )

-- | The main executor loop in the browser
-- | Accepts an effectful scene
runInBrowser_ ::
  forall accumulator microphone track buffer floatArray periodicWave callback env.
  RunnableMedia callback accumulator env =>
  Effect callback ->
  accumulator ->
  AudioContext ->
  EngineInfo ->
  AudioInfo (Object microphone) (Object track) (Object buffer) (Object floatArray) (Object periodicWave) ->
  VisualInfo ->
  Exporter env ->
  Effect (Effect Unit)
runInBrowser_ scene' accumulator ctx engineInfo audioInfo visualInfo exporter = do
  scene <- scene'
  runInBrowser scene accumulator ctx engineInfo audioInfo visualInfo exporter
