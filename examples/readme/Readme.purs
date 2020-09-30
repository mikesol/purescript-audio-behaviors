module FRP.Behavior.Audio.Example.Readme where

-- tests everything in the readme to make sure it works!
import Prelude
import Data.Array (span, last, head, range)
import Data.Int (toNumber)
import Data.List ((:), List(..))
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.Set (isEmpty)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1, D2)
import Data.Vec ((+>), empty)
import Effect (Effect)
import FRP.Behavior (Behavior, integral', fixB, switcher)
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, Instruction, dup1, gain', gainT', merger, microphone, panner, play, runInBrowser, runInBrowser_, sinOsc, speaker, speaker')
import FRP.Behavior.Audio as Aud
import FRP.Behavior.Mouse (buttons)
import FRP.Behavior.Time as Time
import FRP.Event.Mouse (Mouse, getMouse, down)
import Foreign (Foreign)
import Math (pi, sin)

scene0 :: forall a. a -> Number -> Behavior (AudioUnit D1)
scene0 _ _ = pure (speaker' $ (gain' 0.5 $ sinOsc 440.0))

scene1 :: forall a. a -> Number -> Behavior (AudioUnit D1)
scene1 _ _ =
  pure
    ( speaker
        $ ( (gain' 0.2 $ sinOsc 110.0)
              :| (gain' 0.1 $ sinOsc 220.0)
              : microphone
              : Nil
          )
    )

scene2 :: forall a. a -> Number -> Behavior (AudioUnit D1)
scene2 _ _ =
  pure
    ( speaker
        $ ( (gain' 0.2 $ sinOsc 110.0)
              :| (gain' 0.1 $ sinOsc 220.0)
              : (gain' 0.5 $ (play "forest"))
              : microphone
              : Nil
          )
    )

scene3 :: forall a. a -> Number -> Behavior (AudioUnit D2)
scene3 _ _ =
  pure
    $ dup1
        ( (gain' 0.2 $ sinOsc 110.0)
            + (gain' 0.1 $ sinOsc 220.0)
            + microphone
        ) \mono ->
        speaker
          $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                :| (gain' 0.5 $ (play "forest"))
                : Nil
            )

scene4 :: forall a. a -> Number -> Behavior (AudioUnit D2)
scene4 _ time =
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

scene5 :: forall a. Mouse -> a -> Number -> Behavior (AudioUnit D2)
scene5 mouse _ time = f time <$> click
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
  click = map isEmpty $ buttons mouse

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

scene6 :: forall a. Mouse -> a -> Number -> Behavior (AudioUnit D2)
scene6 mouse _ time = f time <$> click
  where
  split s = span ((s >= _) <<< fst) pwf

  gn s =
    let
      ht = split s
    in
      let
        left = fromMaybe (Tuple 0.0 0.0) $ last ht.init
      in
        let
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
            in
              let
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
  click = map isEmpty $ buttons mouse

run =
  runInBrowser_
    ( do
        mouse <- getMouse
        pure (scene6 mouse)
    )

main :: Effect Unit
main = pure unit
