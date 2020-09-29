module FRP.Behavior.Audio.Example.Readme where

-- tests everything in the readme to make sure it works!
import Prelude
import Data.Int (toNumber)
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Set (isEmpty)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Array (span, last, head, range)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import FRP.Behavior.Time as Time
import Data.Typelevel.Num (D1, D2)
import Effect (Effect)
import Data.Vec ((+>), empty)
import FRP.Behavior (Behavior, integral', fixB, switcher)
import FRP.Behavior.Mouse (buttons)
import FRP.Event.Mouse (Mouse, getMouse, down)
import FRP.Behavior.Audio (AudioUnit, Instruction, AudioParameter(..), merger, panner, gain', gainT', microphone, play, runInBrowser, sinOsc, speaker, dup1, speaker')
import FRP.Behavior.Audio as Aud
import Foreign (Foreign)
import Math (pi, sin)

scene0 :: Behavior Number -> Behavior (AudioUnit D1)
scene0 _ = pure (speaker' $ (gain' 0.5 $ sinOsc 440.0))

scene1 :: Behavior Number -> Behavior (AudioUnit D1)
scene1 _ =
  pure
    ( speaker
        $ ( (gain' 0.2 $ sinOsc 110.0)
              :| (gain' 0.1 $ sinOsc 220.0)
              : microphone
              : Nil
          )
    )

scene2 :: Behavior Number -> Behavior (AudioUnit D1)
scene2 _ =
  pure
    ( speaker
        $ ( (gain' 0.2 $ sinOsc 110.0)
              :| (gain' 0.1 $ sinOsc 220.0)
              : (gain' 0.5 $ (play "forest"))
              : microphone
              : Nil
          )
    )

scene3 :: Behavior Number -> Behavior (AudioUnit D2)
scene3 _ =
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

scene4 :: Behavior Number -> Behavior (AudioUnit D2)
scene4 time = f <$> time
  where
  f s =
    let
      rad = pi * s
    in
      dup1
        ( (gain' 0.2 $ sinOsc (110.0 + (10.0 * sin (0.2 * rad))))
            + (gain' 0.1 $ sinOsc 220.0)
            + microphone
        ) \mono ->
        speaker
          $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                :| (gain' 0.5 $ (play "forest"))
                : Nil
            )

scene5 :: Mouse -> Behavior Number -> Behavior (AudioUnit D2)
scene5 mouse time = f <$> time <*> swell
  where
  f s sw =
    let
      rad = pi * s
    in
      dup1
        ( (gain' 0.2 $ sinOsc (110.0 + (10.0 * sin (0.2 * rad))))
            + (gain' 0.1 $ sinOsc (220.0 + sw))
            + microphone
        ) \mono ->
        speaker
          $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                :| (gain' 0.5 $ (play "forest"))
                : Nil
            )

  -- `swell` is an interactive function of time defined by a differential equation:
  --
  -- d^2s/dt^2
  --   | mouse down = ⍺ - βs
  --   | mouse up   = ɣ - δs - ε ds/dt
  --
  -- So the function exhibits either decay or growth depending on if
  -- the mouse is pressed or not.
  --
  -- We can solve the differential equation by integration using `solve2'`.
  swell :: Behavior Number
  swell =
    fixB 2.0 \b ->
      integral' 2.0 (unwrap <$> Time.seconds)
        let
          db =
            fixB 10.0 \db_ ->
              integral' 10.0 (unwrap <$> Time.seconds) (ft <$> buttons mouse <*> b <*> db_)
        in
          switcher db (down $> db)
    where
    ft bs s ds
      | isEmpty bs = -8.0 * (s - 1.0) - ds * 2.0
      | otherwise = 2.0 * (4.0 - s)

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

kr = 15.0 / 1000.0 :: Number -- the control rate in seconds, or 66.66667 Hz

scene6 :: Mouse -> Behavior Number -> Behavior (AudioUnit D2)
scene6 mouse time = f <$> time <*> swell
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

  f s sw =
    let
      rad = pi * s
    in
      dup1
        ( (gain' 0.2 $ sinOsc (110.0 + (3.0 * sin (0.5 * rad))))
            + (gain' 0.1 (gainT' (gn s) $ sinOsc 440.0))
            + (gain' 0.1 $ sinOsc (220.0 + sw))
            + microphone
        ) \mono ->
        speaker
          $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                :| (gain' 0.5 $ (play "forest"))
                : Nil
            )

  -- `swell` is an interactive function of time defined by a differential equation:
  --
  -- d^2s/dt^2
  --   | mouse down = ⍺ - βs
  --   | mouse up   = ɣ - δs - ε ds/dt
  --
  -- So the function exhibits either decay or growth depending on if
  -- the mouse is pressed or not.
  --
  -- We can solve the differential equation by integration using `solve2'`.
  swell :: Behavior Number
  swell =
    fixB 2.0 \b ->
      integral' 2.0 (unwrap <$> Time.seconds)
        let
          db =
            fixB 10.0 \db_ ->
              integral' 10.0 (unwrap <$> Time.seconds) (ft <$> buttons mouse <*> b <*> db_)
        in
          switcher db (down $> db)
    where
    ft bs s ds
      | isEmpty bs = -8.0 * (s - 1.0) - ds * 2.0
      | otherwise = 2.0 * (4.0 - s)

-- there's a bug here
-- by including swell, it throws the audio graph off...
scene6' :: Mouse -> Behavior Number -> Behavior (AudioUnit D1)
scene6' mouse time = f <$> time <*> swell
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
          right = fromMaybe (Tuple 101.0 0.0) $ head ht.rest
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

  f s sw =
    let
      rad = pi * s
    in
      speaker
        ( (gain' 0.1 (gainT' (gn s) $ sinOsc 440.0))
            :| (gain' 0.5 $ (play "forest"))
            -- : (gain' 0.1 $ sinOsc (220.0 + sw))
            
            : (gain' 0.2 $ sinOsc (110.0 + (3.0 * sin (0.5 * rad))))
            : Nil
        )

  swell :: Behavior Number
  swell =
    fixB 2.0 \b ->
      integral' 2.0 (unwrap <$> Time.seconds)
        let
          db =
            fixB 10.0 \db_ ->
              integral' 10.0 (unwrap <$> Time.seconds) (ft <$> buttons mouse <*> b <*> db_)
        in
          switcher db (down $> db)
    where
    ft bs s ds
      | isEmpty bs = -8.0 * (s - 1.0) - ds * 2.0
      | otherwise = 2.0 * (4.0 - s)

type Sources
  = { forest :: Foreign
    }

run ::
  Int ->
  Int ->
  Foreign ->
  Foreign ->
  Sources ->
  Array Foreign ->
  (Number -> Array Instruction -> Foreign -> Foreign -> Sources -> Array Foreign -> Effect (Array Foreign)) ->
  Effect (Effect Unit)
run a b c d e f g = do
  mouse <- getMouse
  runInBrowser (scene6' mouse) a b c d e f g

touchAudio :: Number -> Array Instruction → Foreign → Foreign → Sources → Array Foreign → Effect (Array Foreign)
touchAudio = Aud.touchAudio

makeWorkers :: Int -> Effect (Array Foreign)
makeWorkers = Aud.makeWorkers

main :: Effect Unit
main = pure unit
