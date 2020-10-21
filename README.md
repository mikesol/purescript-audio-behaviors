# purescript-audio-behaviors

[`purescript-behaviors`](https://github.com/paf31/purescript-behaviors) for web audio.

## Demo

Check out [klank.dev](https://klank.dev), where you can use this library interactively in the browser.

## Installation

```bash
spago install
```

## Build

```bash
spago build
```

## Main idea

This library uses the [behaviors pattern](https://wiki.haskell.org/Functional_Reactive_Programming) pioneered by Conal Elliott and Paul Hudak. You describe the way audio should behave at a given time, and the function is sampled at regular intervals to build the audio graph.

For example, consider the following behavior, taken from [`HelloWorld.purs`](./examples/hello-world/HelloWorld.purs):

[Try me on klank.dev](https://link.klank.dev/CBv8TPJxVWJikohT9)

```haskell
scene ::  Number -> Behavior (AudioUnit D1)
scene time = let
      rad = pi * time
    in
      pure $ speaker
         ( (gain' 0.1 $ sinOsc (440.0 + (10.0 * sin (2.3 * rad))))
              :| (gain' 0.25 $ sinOsc (235.0 + (10.0 * sin (1.7 * rad))))
              : (gain' 0.2 $ sinOsc (337.0 + (10.0 * sin rad)))
              : (gain' 0.1 $ sinOsc (530.0 + (19.0 * (5.0 * sin rad))))
              : Nil
          )
```

Here, there are four sine wave oscillators whose frequencies modulate subtly based on time, creating an eerie Theremin effect. Under the hood, this library samples the function to know what the frequencies should be played at any given time and makes sure they are rendered to the speaker.

## Building a scene

The main unit of work in `purescript-audio-behaviors` is the **scene**. A scene, like the one above, is a function of time, where the input time comes from the audio clock at regular intervals.

In this section, we'll build a scene from the ground up. In doing so, we'll accomplish several things:

1. Getting a static sound to play.
1. Adding sound via the microphone.
1. Adding playback from an `audio` tag.
1. Going from mono to stereo.
1. Getting the sound to change as a function of time.
1. Getting the sound to change as a function of a mouse input event.
1. Making sure that certain sounds occur at a precise time.
1. Remembering when events happened.
1. Working with feedback.
1. Adding visuals.

### Getting a static sound to play

Let's start with a sine wave at A440 playing at a volume of `0.5` (where `1.0` is the loudest volume).

[Try me on klank.dev](https://link.klank.dev/TYgGyYwjdh5mWg9D9)

```haskell
scene :: Number -> Behavior (AudioUnit D1)
scene = const $ pure (speaker' $ (gain' 0.5 $ sinOsc 440.0))
```

Note that, because this function does not depend on time, we can ignore the input.

### Adding sound via the microphone

Let's add our voice to the mix! We'll put it above a nice low drone.

[Try me on klank.dev](https://link.klank.dev/qCFPu6fK7h4BGiUx9)

```haskell
scene :: Number -> Behavior (AudioUnit D1)
scene =
  const
    $ pure
        ( speaker
            $ ( (gain' 0.2 $ sinOsc 110.0)
                  :| (gain' 0.1 $ sinOsc 220.0)
                  : microphone
                  : Nil
              )
        )
```

Make sure to wear headphones to avoid feedback!

### Adding playback from an audio tag

Let's add some soothing jungle sounds to the mix. We use the function `play` to add an audio element. This function assumes that you provide an audio element with the appropriate tag to the toplevel `runInBrowser` function. In this case, the tag is `"forest"`.

[Try me on klank.dev](https://link.klank.dev/mkpLh13aNu1duSEa9)

```haskell
-- assuming we have passed in an object
-- with { forest: new Audio("my-recording.mp3") }
-- to `runInBrowser`
scene :: Number -> Behavior (AudioUnit D1)
scene =
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
```

### Going from mono to stereo

To go from mono to stereo, there is a class of functions called `dupX`, `splitX` and `merger`. In the example below, we use `dup1` to duplicate a mono sound and then `merge` it into two stereo tracks.

If you want to make two separate audio units, then you can use a normal let block. If, on the other hand, you want to use the same underlying unit, use `dupX`. When in doubt, use `dupX`, as you'll rarely need to duplicate an identical audio source.

[Try me on klank.dev](https://link.klank.dev/wUrdhc4Q2dn5v3TNA)

```haskell
scene :: Number -> Behavior (AudioUnit D2)
scene =
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
```

### Getting the sound to change as a function of time

Up until this point, our audio hasn't reacted to many behaviors. Let's fix that! One behavior to react to is the passage of time. Let's add a slow undulation to the lowest pitch in the drone that is based on the passage of time

[Try me on klank.dev](https://link.klank.dev/7R891WVcSnTcZT5G6)

```haskell
scene :: Number -> Behavior (AudioUnit D2)
scene time =
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
```

### Getting the sound to change as a function of a mouse input event

The next snippet of code uses the mouse to modulate the pitch of the higher note by roughly a major third.

[Try me on klank.dev](https://link.klank.dev/bkvxvHTFQeKeyMZK6)

```haskell
scene :: Mouse -> Number -> Behavior (AudioUnit D2)
scene mouse time = f time <$> click
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
```

### Making sure that certain sounds occur at a precise time

Great audio is all about timing, but so far, we have been locked to scheduling events at multiples of the control rate. The most commonly used control rate for this library is 50Hz, which is way too slow to quantize complex rhythmic events.

To fix the control rate problem, parameters that can change in time like _frequency_ or _gain_ have an optional second parameter that specifies the offset, in seconds, from the current quantized value in the control rate.

For example, let's say the control rate is `66Hz` and you want a sound to trigger at _exactly_ `0.25` seconds. At this rate, the closest quantized value to `0.25` is `0.2424242424`, or `16/66`. That means that, when `time` is `0.24242424`, we will add an offset of `0.00757576` to the value to make sure that it happens "exactly" at `0.25` seconds. "Exactly" is in quoation marks because floating point arrithmentic will provoke a rounding error of around `0.000000000001`, but this is far smaller than the audio sample rate, so we will not hear it.

Let's add a small metronome on the inside of our sound. We will have it beat every `0.9` seconds, and we use the function `gainT'` instead of `gain` to accept an `AudioParameter`.

[Try me on klank.dev](https://link.klank.dev/yRmb7EC1zGLqQANn8)

```haskell
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

kr = 20.0 / 1000.0 :: Number -- the control rate in seconds, or 50 Hz

scene :: Mouse -> Number -> Behavior (AudioUnit D2)
scene mouse time = f time <$> click
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
```

### Remembering when events happened

Sometimes, you don't just want to react to an event like a mouse click. You want to remember when the event happened in time. For example, imagine that we modulate a pitch whenever a button is clicked, like in the example below. When you click the mouse, the pitch should continue slowly rising until the mouse button is released.

To accomplish this, or anything where memory needs to be retained, the scene accepts an arbitrary accumulator as its first parameter. You can think of it as a [fold](https://pursuit.purescript.org/packages/purescript-foldable-traversable/4.1.1/docs/Data.Foldable#v:fold) over time.

To make the accumulator useful, the scene should return the accumulator as well. The constructor `IAudioUnit` allows for this: it accepts an audio unit as well as an accumulator.

[Try me on klank.dev](https://link.klank.dev/mw6KayjtZnDWcqFU7)

```haskell
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

kr = 20.0 / 1000.0 :: Number -- the control rate in seconds, or 50 Hz

initialOnset = { onset: Nothing } :: { onset :: Maybe Number }

scene ::
  forall a.
  Mouse ->
  { onset :: Maybe Number | a } ->
  Number ->
  Behavior (IAudioUnit D2 { onset :: Maybe Number | a })
scene mouse acc@{ onset } time = f time <$> click
  where
  split s = span ((s >= _) <<< fst) pwf

  gn s =
    let
      ht = split s

      left = fromMaybe (Tuple 0.0 0.0) $ last ht.init

      right = fromMaybe (Tuple 201.0 0.0) $ head ht.rest
    in
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
```

Because the accumulator object is global for an entire audio graph, it's a good idea to use row polymorphism in the accumulator object. While using keys like `onset` is fine for small projects, if you're a library developer, you'll want to make sure to use keys more like namespaces. That is, you'll want to make sure that they do not conflict with other vendors' keys and with users' keys. A good practice is to use something like `{ myLibrary :: { param1 :: Number } | a }`.

#### Working with feedback

Our microphone has been pretty boring up until now. Let's create a feedback loop to spice things up.

A feedback loop is created when one uses the processed output of an audio node as an input to itself. One classic physical feedback loop is echo between two walls: the delayed audio bounces back and forth, causing really interesting and surprising effects.

Because audio functions like `gain` consume other audio functions like `sinOsc`, there is no way to create a loop by composing these functions. Instead, to create a feedback loop, we need to use the `graph` function to create an audio graph.

An audio graph is a row with three keys: `accumulators`, `processors` and `generators`. `generators` can be any function that creates audio (including `graph` itself). `processors` are unary audio operators like filters and convolution. All of the audio functions that do this, like `highpass` and `waveShaper`, have graph analogues with `g'` prepended, ie `g'highpass` and `g'waveShaper`. `aggregators` are _n_-ary audio operators like `g'add`, `g'mul` and `g'gain` (gain is just addition composed with multiplication of a constant, and the special `gain` function does this in an efficient way).

The audio graph must respect certain rules: it must be fully connected, it must have a unique terminal node, it must have at least one generator, it must have no orphan nodes, it must not have duplicate edges between nodes, etc. Violating any of these rules will result in a type error at compile-time.

The graph structure is represented using _incoming_ edges, so processors have only one incoming edge whereas accumulators have an arbitrary number of incoming edges, as we see below. Play it and you'll hear an echo effect!

[Try me on klank.dev](https://link.klank.dev/soK6Z2guc4XRuRNm8)

```haskell
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

kr = 20.0 / 1000.0 :: Number -- the control rate in seconds, or 50 Hz

initialOnset = { onset: Nothing } :: { onset :: Maybe Number }

scene ::
  forall a.
  Mouse ->
  { onset :: Maybe Number | a } ->
  Number ->
  Behavior (IAudioUnit D2 { onset :: Maybe Number | a })
scene mouse acc@{ onset } time = f time <$> click
  where
  split s = span ((s >= _) <<< fst) pwf

  gn s =
    let
      ht = split s

      left = fromMaybe (Tuple 0.0 0.0) $ last ht.init

      right = fromMaybe (Tuple 201.0 0.0) $ head ht.rest
    in
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
```

#### Adding visuals

Let's add a little dot that gets bigger when we click. We'll do that using the `AV` constructor that accepts a [Drawing](https://pursuit.purescript.org/packages/purescript-drawing/4.0.0/docs/Graphics.Drawing#t:Drawing).

[Try me on klank.dev](https://klank.dev/?k&ec&url=https://klank-share.s3.eu-west-1.amazonaws.com/K16029054664258658.purs)

```haskell
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

kr = 20.0 / 1000.0 :: Number -- the control rate in seconds, or 50 Hz

initialOnset = { onset: Nothing } :: { onset :: Maybe Number }

scene ::
  forall a.
  Mouse ->
  { onset :: Maybe Number | a } ->
  CanvasInfo ->
  Number ->
  Behavior (AV D2 { onset :: Maybe Number | a })
scene mouse acc@{ onset } (CanvasInfo { w, h }) time = f time <$> click
  where
  split s = span ((s >= _) <<< fst) pwf

  gn s =
    let
      ht = split s

      left = fromMaybe (Tuple 0.0 0.0) $ last ht.init

      right = fromMaybe (Tuple 201.0 0.0) $ head ht.rest
    in
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
```

### Conclusion

We started with a simple sound and built all the way up to a complex, precisely-timed stereo structure with feedback that responds to mouse events both visually and sonically. These examples also exist in [Readme.purs](./examples/readme/Readme.purs).

From here, the only thing left is to make some noise! There are many more audio units in the library, such as filters, compressors and convolvers. Almost the whole [Web Audio API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API) is exposed.

To see a list of exported audio units, you can check out [`Audio.purs`](./src/FRP/Behavior/Audio.purs). In a future version of this, we will refactor things so that all of the audio units are in one package.

## MIDI

The file [src/FRP/Behavior/MIDI.purs](./src/FRP/Behavior/MIDI.purs) exposes one function - `midi` - that can be used in conjunction with `getMidi` [src/FRP/Event/MIDI.purs](./src/FRP/Event/MIDI.purs) to incorporate [realtime MIDI data](https://twitter.com/stronglynormal/status/1316756584786276352) into the audio graph. For an example of how this is done, check out [examples/midi-in](./examples/midi-in).

## Interacting with the browser

In simple setups, you'll interact with the browser in a `<script>` tag to create resources like buffers and float arrays. This is how it is done in most of the [./examples](./examples) directory. However, sometimes you'll be creating a webpage using PureScript, in which case you may need to create a browser-specific resource like an audio buffer for a `playBuf` in PureScript.

To this end, there are several helper functions that allow you to interact directly with the browser. The advantage of these functions is that they link into the `purescript-audio-behaviors` type system. However, as they are just assigning types to opaque blobs from the browser, you can also use your own FFI functions and cast the results to types understood by this library.

```haskell
-- creates a new audio context
-- necessary for some of the functions below and for `runInBrowser`
makeAudioContext :: Effect AudioContext

-- decode audio data from a Uri, ie a link to a wav file
decodeAudioDataFromUri :: AudioContext -> String -> Effect (Promise BrowserAudioBuffer)

-- decode audio data from a base 64 encoded string, passed directly as an argument
decodeAudioDataFromBase64EncodedString :: AudioContext -> String -> Effect (Promise BrowserAudioBuffer)

-- make an audio track
-- the advantage of audio tracks over audio buffers is that
-- they are streamed, so you don't need to wait for them to be downloaded
-- to start playing
makeAudioTrack :: String -> Effect BrowserAudioTrack

-- make an audio buffer
-- best for things like creating drum machines, impulse responses, granular synthesis etc
-- basically anything short
-- for anything that resembles streaming, use makeAudioTrack
makeAudioBuffer :: AudioContext -> AudioBuffer -> Effect BrowserAudioBuffer

-- makes a 32-bit float array
-- useful when creating wave shapers (this is what adds the distortion)
makeFloatArray :: Array Number -> Effect BrowserFloatArray

-- makes a periodic wave
-- this is what is used for the periodicOsc unit
makePeriodicWave ::
  forall len.
  Pos len =>
  AudioContext ->
  Vec len Number ->
  Vec len Number ->
  Effect BrowserPeriodicWave
```

## Advanced usage

Here are some tips for advanced usage of `purescript-audio-behaviors`.

### Exporting

`purescript-audio-behaviors` translates scenes to a sort of "assembly" language that is passed to an audio rendering function. This language has primitives like `NewUnit` for a new audio unit, `ConnectTo`, to connect one unit to another one, etc. These instructions are sent to an exporter for further downstream processing. Examples of downstream actions could be:

- printing all processing information to a log, ie `console.log`
- sending to a server for dispatching to MIDI devices or SuperCollider
- tweeting your audio graph every 20ms (I don't know why you'd do this... but you could!)

The library provides a `defaultExporter` that is a no-op. To override this, pass an exporter with type `Exporter` to the `runInBrowser` function. [Here is an example on klank.dev of an exporter that prints to `console.log`](https://link.klank.dev/6N3KnLpyiZJJm14m7).

### Named units

As you build larger and larger audio structures, you may notice some stuttering in your application.

One way to mitigate this is to give your audio units names. Named audio units speed up computation and result in less-glitchy corner cases when the audio graph changes radically.

Giving names is a bit tedious, so the recommendation is to build audio without names first and then, once you're satisfied with your scene, to give everything names. Here is how you attribute names to audio units:

```haskell
sinOsc_ "My name" 440.0
```

Notice the trailing underscore after the function. That's all you need. If you add a trailing underscore, you'll get a function that can accept a name.

If you are building an audio function that is supposed to be reused (ie your own filter, etc), named audio is a great idea, as it will speed up computation everywhere the function is used.

```haskell
myAwesomeFilter t = highpass_ ("myAwesomeFilter_" <> t) 1000.0 0.3 0.5
```

### Tweaking engine parameters

The `runInBrowser` function takes an `EngineInfo` parameter that specifies how the audio and animation should be rendered.

```haskell
type EngineInfo
  = { msBetweenSamples :: Int
    , msBetweenPings :: Int
    , fastforwardLowerBound :: Number
    , rewindUpperBound :: Number
    , initialOffset :: Number
    }
```

- `msBetweenSamples` - The number of milliseconds between samples of the audio behavior. This is the effective control rate. The lower the rate, the more rhythmic precision, and the higher the rate, the less likely there will be jank. Try `20`.
- `msBetweenPings` - The number of milliseconds between pings to the sampling engine. The lower the rate, the less chance that you will miss a sampling deadline but the higher chance your page will lock up. This _must_ be less than `msBetweenSamples`. Try 15.
- `fastforwardLowerBound` - The number of seconds below which the audio engine will skip a frame. The lower this is, the less likely there will be a skip, but the more likely the skip will sound jarring if it happens. Try `0.025`.
- `rewindUpperBound` - The number of seconds of look-ahead. For uses that have no interactive component other than starting and stopping the sound (meaning no mouse, no MIDI Keyboard, etc) this can be large (ie `1.0` or even higher). For apps with an interactive component, you want this as low as possible, ie `0.06` or even lower. Note that this should be _at least_ twice `msBetweenSamples`.
- `initialOffset` - The number of seconds to wait before playing. JavaScript does a lot of memory allocation when a klank starts playing, which sometimes results in jank. Try something between `0.1` and `0.4`.
- `doWebAudio` - Should we render all of this stuff towith the web audio API? If true, then yes, otherwise the Web Audio API won't be called. Useful when you want to use an exporter without making sound in the browser.

## Bundling on your site

To see how to bundle this library on your site, please visit the [examples](./examples) directory.

To compile the JS for the hello world example, issue the following command:

```bash
spago -x examples.dhall bundle-app \
  --main FRP.Behavior.Audio.Example.HelloWorld \
  --to examples/hello-world/index.js
```

Other examples will work the same way, with the directory and module name changing.

You will also need to copy all of the files from the top-level `custom-units` folter into your project folder. With a correct setup, the hello-world directory should look like this:

```bash
examples/
  hello-world/
    HelloWorld.purs  # incldued in the git distro
    index.html # incldued in the git distro
    index.js # the generated js from spago bundle-app
    ps-aud-mul.js # plus any other files from the custom-units folder
```

From there, you can run `python -m http.server` in the directory of the example and it will serve all of the files. Visit `http://localhost:8000` in Firefox to interact with the page.
