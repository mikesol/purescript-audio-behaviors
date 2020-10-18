module FRP.Behavior.Audio.Example.Readme where

-- tests everything in the readme to make sure it works!
import Prelude
import Color (rgb)
import Data.Array (span, last, head, range)
import Data.Int (toNumber)
import Data.List ((:), List(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty ((:|))
import Data.Set (isEmpty)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1, D2)
import Data.Vec ((+>), empty)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AV(..), AudioContext, AudioInfo, AudioParameter(..), AudioUnit, CanvasInfo(..), EngineInfo, Exporter, IAudioUnit(..), VisualInfo, defaultExporter, dup1, g'add, g'bandpass, g'delay, g'gain, gain', gainT', graph, merger, microphone, panner, play, runInBrowser_, sinOsc, speaker, speaker')
import FRP.Behavior.Mouse (buttons, position)
import FRP.Event.Mouse (Mouse, getMouse)
import Foreign.Object (Object)
import Graphics.Drawing (circle, fillColor, filled)
import Math (pi, sin)
import Record.Extra (SLProxy(..), SNil)
import Type.Data.Graph (type (:/))

scene0 :: Number -> Behavior (AudioUnit D1)
scene0 = const $ pure (speaker' $ (gain' 0.5 $ sinOsc 440.0))

scene1 :: Number -> Behavior (AudioUnit D1)
scene1 =
  const
    $ pure
        ( speaker
            $ ( (gain' 0.2 $ sinOsc 110.0)
                  :| (gain' 0.1 $ sinOsc 220.0)
                  : microphone
                  : Nil
              )
        )

scene2 :: Number -> Behavior (AudioUnit D1)
scene2 =
  const
    $ pure
        ( speaker
            $ ( (gain' 0.2 $ sinOsc 110.0)
                  :| (gain' 0.1 $ sinOsc 220.0)
                  : (gain' 0.5 $ (play "forest"))
                  : microphone
                  : Nil
              )
        )

scene3 :: Number -> Behavior (AudioUnit D2)
scene3 =
  const
    $ pure
        ( dup1
            ( (gain' 0.2 $ sinOsc 110.0)
                + (gain' 0.1 $ sinOsc 220.0)
                + microphone
            ) \mono ->
            speaker
              $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                    :| (gain' 0.5 $ (play "forest"))
                    : Nil
                )
        )

scene4 :: Number -> Behavior (AudioUnit D2)
scene4 time =
  let
    rad = pi * time
  in
    pure
      $ dup1
          ( (gain' 0.2 $ sinOsc (110.0 + (10.0 * sin (0.2 * rad))))
              + (gain' 0.1 $ sinOsc 220.0)
              + microphone
          ) \mono ->
          speaker
            $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                  :| (gain' 0.5 $ (play "forest"))
                  : Nil
              )

scene5 :: Mouse -> Number -> Behavior (AudioUnit D2)
scene5 mouse time = f time <$> click
  where
  f s cl =
    let
      rad = pi * s
    in
      dup1
        ( (gain' 0.2 $ sinOsc (110.0 + (10.0 * sin (0.2 * rad))))
            + (gain' 0.1 $ sinOsc (220.0 + (if cl then 50.0 else 0.0)))
            + microphone
        ) \mono ->
        speaker
          $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                :| (gain' 0.5 $ (play "forest"))
                : Nil
            )

  click :: Behavior Boolean
  click = map (not <<< isEmpty) $ buttons mouse

-- a piecewise function that creates an attack/release/sustain envelope
-- at a periodicity of every 0.9 seconds
pwf :: Array (Tuple Number Number)
pwf =
  join
    $ map
        ( \i ->
            map
              ( \(Tuple f s) ->
                  Tuple (f + 0.11 * toNumber i) s
              )
              [ Tuple 0.0 0.0, Tuple 0.02 0.7, Tuple 0.06 0.2 ]
        )
        (range 0 400)

kr = 20.0 / 1000.0 :: Number -- the control rate in seconds, or 66.66667 Hz

scene6 :: Mouse -> Number -> Behavior (AudioUnit D2)
scene6 mouse time = f time <$> click
  where
  split s = span ((s >= _) <<< fst) pwf

  gn s =
    let
      ht = split s

      left = fromMaybe (Tuple 0.0 0.0) $ last ht.init

      right = fromMaybe (Tuple 201.0 0.0) $ head ht.rest
    in
      -- if we are in a control cycle with a peak or trough
      -- we lock to that
      -- otherwise, we interpolate
      if (fst right - s) < kr then
        AudioParameter { param: (snd right), timeOffset: (fst right - s) }
      else
        let
          m = (snd right - snd left) / (fst right - fst left)

          b = (snd right - (m * fst right))
        in
          AudioParameter { param: (m * s + b), timeOffset: 0.0 }

  f s cl =
    let
      rad = pi * s
    in
      dup1
        ( (gain' 0.2 $ sinOsc (110.0 + (3.0 * sin (0.5 * rad))))
            + (gain' 0.1 (gainT' (gn s) $ sinOsc 440.0))
            + (gain' 0.1 $ sinOsc (220.0 + (if cl then 50.0 else 0.0)))
            + microphone
        ) \mono ->
        speaker
          $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                :| (gain' 0.5 $ (play "forest"))
                : Nil
            )

  click :: Behavior Boolean
  click = map (not <<< isEmpty) $ buttons mouse

initialOnset = { onset: Nothing } :: { onset :: Maybe Number }

scene7 ::
  Mouse ->
  { onset :: Maybe Number } ->
  Number ->
  Behavior (IAudioUnit D2 { onset :: Maybe Number })
scene7 mouse acc@{ onset } time = f time <$> click
  where
  split s = span ((s >= _) <<< fst) pwf

  gn s =
    let
      ht = split s

      left = fromMaybe (Tuple 0.0 0.0) $ last ht.init

      right = fromMaybe (Tuple 201.0 0.0) $ head ht.rest
    in
      -- if we are in a control cycle with a peak or trough
      -- we lock to that
      -- otherwise, we interpolate
      if (fst right - s) < kr then
        AudioParameter { param: (snd right), timeOffset: (fst right - s) }
      else
        let
          m = (snd right - snd left) / (fst right - fst left)

          b = (snd right - (m * fst right))
        in
          AudioParameter { param: (m * s + b), timeOffset: 0.0 }

  f s cl =
    IAudioUnit
      ( dup1
          ( (gain' 0.2 $ sinOsc (110.0 + (3.0 * sin (0.5 * rad))))
              + (gain' 0.1 (gainT' (gn s) $ sinOsc 440.0))
              + (gain' 0.1 $ sinOsc (220.0 + (if cl then (50.0 + maybe 0.0 (\t -> 10.0 * (s - t)) stTime) else 0.0)))
              + microphone
          ) \mono ->
          speaker
            $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                  :| (gain' 0.5 $ (play "forest"))
                  : Nil
              )
      )
      (acc { onset = stTime })
    where
    rad = pi * s

    stTime = case Tuple onset cl of
      (Tuple Nothing true) -> Just s
      (Tuple (Just y) true) -> Just y
      (Tuple _ false) -> Nothing

  click :: Behavior Boolean
  click = map (not <<< isEmpty) $ buttons mouse

scene7_1 ::
  Mouse ->
  { onset :: Maybe Number } ->
  Number ->
  Behavior (IAudioUnit D2 { onset :: Maybe Number })
scene7_1 mouse acc@{ onset } time = f time <$> click
  where
  split s = span ((s >= _) <<< fst) pwf

  gn s =
    let
      ht = split s

      left = fromMaybe (Tuple 0.0 0.0) $ last ht.init

      right = fromMaybe (Tuple 201.0 0.0) $ head ht.rest
    in
      -- if we are in a control cycle with a peak or trough
      -- we lock to that
      -- otherwise, we interpolate
      if (fst right - s) < kr then
        AudioParameter { param: (snd right), timeOffset: (fst right - s) }
      else
        let
          m = (snd right - snd left) / (fst right - fst left)

          b = (snd right - (m * fst right))
        in
          AudioParameter { param: (m * s + b), timeOffset: 0.0 }

  f s cl =
    IAudioUnit
      ( dup1
          ( (gain' 0.2 $ sinOsc (110.0 + (3.0 * sin (0.5 * rad))))
              + (gain' 0.1 (gainT' (gn s) $ sinOsc 440.0))
              + (gain' 0.1 $ sinOsc (220.0 + (if cl then (50.0 + maybe 0.0 (\t -> 10.0 * (s - t)) stTime) else 0.0)))
              + ( graph
                    { aggregators:
                        { out: Tuple g'add (SLProxy :: SLProxy ("combine" :/ SNil))
                        , combine: Tuple g'add (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                        , gain: Tuple (g'gain 0.9) (SLProxy :: SLProxy ("del" :/ SNil))
                        }
                    , processors:
                        { del: Tuple (g'delay 0.2) (SProxy :: SProxy "filt")
                        , filt: Tuple (g'bandpass 440.0 1.0) (SProxy :: SProxy "combine")
                        }
                    , generators:
                        { mic: microphone
                        }
                    }
                )
          ) \mono ->
          speaker
            $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                  :| (gain' 0.5 $ (play "forest"))
                  : Nil
              )
      )
      (acc { onset = stTime })
    where
    rad = pi * s

    stTime = case Tuple onset cl of
      (Tuple Nothing true) -> Just s
      (Tuple (Just y) true) -> Just y
      (Tuple _ false) -> Nothing

  click :: Behavior Boolean
  click = map (not <<< isEmpty) $ buttons mouse

scene8 ::
  Mouse ->
  { onset :: Maybe Number } ->
  CanvasInfo ->
  Number ->
  Behavior (AV D2 { onset :: Maybe Number })
scene8 mouse acc@{ onset } (CanvasInfo { w, h, boundingClientRect: { x, y } }) time = f time <$> click <*> pos
  where
  split s = span ((s >= _) <<< fst) pwf

  gn s =
    let
      ht = split s

      left = fromMaybe (Tuple 0.0 0.0) $ last ht.init

      right = fromMaybe (Tuple 201.0 0.0) $ head ht.rest
    in
      -- if we are in a control cycle with a peak or trough
      -- we lock to that
      -- otherwise, we interpolate
      if (fst right - s) < kr then
        AudioParameter { param: (snd right), timeOffset: (fst right - s) }
      else
        let
          m = (snd right - snd left) / (fst right - fst left)

          b = (snd right - (m * fst right))
        in
          AudioParameter { param: (m * s + b), timeOffset: 0.0 }

  f s cl ps =
    AV
      ( Just
          $ dup1
              ( (gain' 0.2 $ sinOsc (110.0 + (3.0 * sin (0.5 * rad))))
                  + (gain' 0.1 (gainT' (gn s) $ sinOsc 440.0))
                  + (gain' 0.1 $ sinOsc (220.0 + (if cl then (50.0 + maybe 0.0 (\t -> 10.0 * (s - t)) stTime) else 0.0)))
                  + ( graph
                        { aggregators:
                            { out: Tuple g'add (SLProxy :: SLProxy ("combine" :/ SNil))
                            , combine: Tuple g'add (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                            , gain: Tuple (g'gain 0.9) (SLProxy :: SLProxy ("del" :/ SNil))
                            }
                        , processors:
                            { del: Tuple (g'delay 0.2) (SProxy :: SProxy "filt")
                            , filt: Tuple (g'bandpass 440.0 1.0) (SProxy :: SProxy "combine")
                            }
                        , generators:
                            { mic: microphone
                            }
                        }
                    )
              ) \mono ->
              speaker
                $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                      :| (gain' 0.5 $ (play "forest"))
                      : Nil
                  )
      )
      ( Just
          $ filled
              (fillColor (rgb 0 0 0))
              ( circle
                  (if cl then toNumber ps.x - x else w / 2.0)
                  (if cl then toNumber ps.y - y else h / 2.0)
                  (if cl then 25.0 else 5.0)
              )
      )
      (acc { onset = stTime })
    where
    rad = pi * s

    stTime = case Tuple onset cl of
      (Tuple Nothing true) -> Just s
      (Tuple (Just y') true) -> Just y'
      (Tuple _ false) -> Nothing

  click :: Behavior Boolean
  click = map (not <<< isEmpty) $ buttons mouse

  pos :: Behavior { x :: Int, y :: Int }
  pos = map (fromMaybe { x: 0, y: 0 }) (position mouse)

run ::
  forall microphone track buffer floatArray periodicWave.
  { onset :: Maybe Number } ->
  AudioContext ->
  EngineInfo ->
  AudioInfo (Object microphone) (Object track) (Object buffer) (Object floatArray) (Object periodicWave) ->
  VisualInfo ->
  Exporter Unit ->
  Effect (Effect Unit)
run =
  runInBrowser_ do
    mouse <- getMouse
    pure (scene8 mouse)

exporter = defaultExporter :: Exporter Unit

main :: Effect Unit
main = pure unit
