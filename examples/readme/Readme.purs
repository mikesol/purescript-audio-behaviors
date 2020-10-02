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
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1, D2)
import Data.Vec ((+>), empty)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AV(..), AudioParameter(..), AudioUnit, CanvasInfo(..), IAudioUnit(..), RunInBrowser, RunInBrowserIAudioUnit_, RunInBrowser_, RunInBrowserAV_, dup1, gain', gainT', merger, microphone, panner, play, runInBrowser, runInBrowser_, sinOsc, speaker, speaker')
import FRP.Behavior.Mouse (buttons)
import FRP.Event.Mouse (Mouse, getMouse)
import Graphics.Drawing (circle, fillColor, filled)
import Math (pi, sin)

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
  forall a.
  Mouse ->
  { onset :: Maybe Number | a } ->
  Number ->
  Behavior (IAudioUnit D2 { onset :: Maybe Number | a })
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

scene8 ::
  forall a.
  Mouse ->
  { onset :: Maybe Number | a } ->
  CanvasInfo ->
  Number ->
  Behavior (AV D2 { onset :: Maybe Number | a })
scene8 mouse acc@{ onset } (CanvasInfo { w, h }) time = f time <$> click
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
    AV
      ( Just
          $ dup1
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
      ( Just
          $ filled
              (fillColor (rgb 0 0 0))
              (circle (w / 2.0) (h / 2.0) (if cl then 25.0 else 5.0))
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

run =
  ( runInBrowser_ ::
      forall a.
      RunInBrowserAV_ { onset :: Maybe Number | a } D2
  )
    ( do
        mouse <- getMouse
        pure (scene8 mouse)
    )

main :: Effect Unit
main = pure unit
