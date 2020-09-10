module FRP.Behavior.Audio
  ( SampleFrame
  , AudioProcessor
  , makeAudioWorkletProcessor
  , audioIO
  , audioIOInterleaved
  , IdxContext
  ) where

import Prelude
import Data.Array (head, index, length, mapWithIndex, range, replicate, snoc, zipWith, (!!))
import Data.Int (floor, toNumber)
import Data.Maybe (fromMaybe)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Ref (modify_, new, read, write)
import FRP.Behavior (Behavior, behavior, sample_)
import FRP.Event (Event, makeEvent, subscribe)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Type.Row.Homogeneous (class Homogeneous)

-- hack because cpp ffi doesn't have whileE yet
whileEImpl :: forall a. Effect Boolean -> Effect a -> Effect Unit
whileEImpl b m = do
  truthy <- b
  if truthy then
    ( do
        void m
        whileEImpl b m
    )
  else
    pure unit

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

soundify ::
  Event Unit ->
  Behavior SampleFrame ->
  (SampleFrame -> Effect Unit) ->
  Effect (Effect Unit)
soundify e scene render = subscribe (sample_ scene e) render

-- | Create an event which fires for every audio frame.
-- | Note that this does not have a canceler.  Cancellation from audio
-- | processing should be handled in the `process()` function of the worklet.
audioDriver :: Effect Boolean -> Event Unit
audioDriver incr =
  makeEvent \k -> do
    whileEImpl incr (k unit)
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
    soundify

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
    soundify _driver chain \frame -> do
      modify_ (flip (zipWith (zipWith snoc)) frame) output
      pure unit
  bam
  read output

-- | The inner audio loop
-- | The same as audioIO, but it keeps the sink interleaved.
-- | For web audio, this avoids additional memory copying.
-- | Note that the outer array enclosing the samples is facultative here,
-- | as interleaved audio is just channels and does not have an extra dimension.
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
    soundify _driver chain \frame -> do
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
