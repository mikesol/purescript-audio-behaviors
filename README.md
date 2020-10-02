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
1. Adding visuals.

### Getting a static sound to play

Let's start with a sine wave at A440 playing at a volume of `0.5` (where `1.0` is the loudest volume).

[Listen on klank.dev](https://klank.dev/?gist=1d5345f85a05b84644941843709f6d6a)

```haskell
scene :: Number -> Behavior (AudioUnit D1)
scene = const $ pure (speaker' $ (gain' 0.5 $ sinOsc 440.0))
```

Note that, because this function does not depend on time, we can ignore the input.

### Adding sound via the microphone

Let's add our voice to the mix! We'll put it above a nice low drone.

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

[Listen on klank.dev](https://klank.dev/?gist=c28a01d3bb382d8a9d24dbb636835a21)

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

Sometimes, you don't just want to react to an event like a mouse click. You want to remember when the event happened in time. For example, imagine that we modulate a pitch whenever a button is clicked, like in the example below. When the pitch modulates, it should continue slowly rising until the button is released.

To accomplish this, or anything where memory needs to be retained, the scene accepts an arbitrary accumulator as its first parameter. You can think of it as a [fold](https://pursuit.purescript.org/packages/purescript-foldable-traversable/4.1.1/docs/Data.Foldable#v:fold) over time.

To make the accumulator useful, the scene should return the accumulator as well. The constructor `IAudioUnit` allows for this: it accepts an audio unit as well as an accumulator.

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

#### Adding visuals

Let's add a little dot that gets bigger when we click. We'll do that using the `AV` constructor that accepts a [Drawing](https://pursuit.purescript.org/packages/purescript-drawing/4.0.0/docs/Graphics.Drawing#t:Drawing).

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
```

### Conclusion

We started with a simple sound and built all the way up to a complex, precisely-timed stereo structure that responds to mouse events both visually and sonically. These examples also exist in [Readme.purs](./examples/readme/Readme.purs).

From here, the only thing left is to make some noise! There are many more audio units in the library, such as filters, compressors and convolvers. Almost the whole [Web Audio API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API) is exposed.

To see a list of exported audio units, you can check out [`Audio.purs`](./src/FRP/Behavior/Audio.purs). In a future version of this, we will refactor things so that all of the audio units are in one package.

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

-- make a periodic wave for the periodic osc
makePeriodicWave :: AudioContext -> Array Number -> Array Number -> Effect BrowserPeriodicWave

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

### AudioContext

## Advanced usage

Here are some tips for advanced usage of `purescript-audio-behaviors`.

### Debugging our scene

`purescript-audio-behaviors` translates scenes to a sort of "assembly" language that is passed to an audio rendering function. This language has primitives like `NewUnit` for a new audio unit, `ConnectTo`, to connect one unit to another one, etc. When debugging, the recommendation is to print these instructions to the console using `console.log`. Then, you will see exactly how the audio graph is updating in realtime.

Another useful way to debug is unit tests. Most behaviors can be refactored as pure functions with the `Behavior` as a top-level applicative control structure, and you can sample them at various times to make sure the audio graph is consistent with your expectations.

### Function signatures

Depending on how you set up your scene, you may need to give a hint to the PureScript compiler about its type when passing it to `runInBrowser` or `runInBrowser_`. There are several type hints shipped with the library:

- `RunInBrowserIAudioUnit`
- `RunInBrowserIAudioUnit_`
- `RunInBrowserIAnimation`
- `RunInBrowserIAnimation_`
- `RunInBrowserAV`
- `RunInBrowserAV_`

Examples of how these are used can be found in [README.purs](./examples/readme/Readme.purs).

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

### Tweaking parameters

Under the hood, `purescript-audio-behaviors` tries _really hard_ to guarantee that, no matter how laggy or janky the browser is, audio is rendered at a consistent rate so that there is no stutter. There are several parameters that influence this, and they all have tradeoffs.

All of the parameters are passed to the function `runInBrowser` in the class `RunnableMedia`, which has the following signature:

```haskell
class RunnableMedia callback accumulator where
  runInBrowser ::
    forall microphone track buffer floatArray periodicWave.
    callback -> -- scene
    accumulator -> -- initial accumulator
    Int -> -- audio clock rate
    Int -> -- driver rate
    AudioContext -> -- audioContext
    AudioInfo (Object microphone) (Object track) (Object buffer) (Object floatArray) (Object periodicWave) -> -- audio info
    VisualInfo -> -- visual info
    Effect (Effect Unit)
```

A lot of this is boilerplate, and you can see examples of how to hook this up in the [examples](./examples) directory. The two bits to understand here are:

- audio clock rate
- driver rate

The _audio clock rate_ represents the number of milliseconds between control-rate pings to the scene. For example, if you set this to `20`, the scene will get polled every `0.02` seconds. When we use `20.0` as the `kr` in the examples above, we're referring to this rate. In general, for most applications, somewhere between `10` and `20` is the sweet spot. Too low and you'll skip frames (jank), too high and you'll start hearing the quantization.

The _driver rate_ represents the number of milliseconds between pings to the entire reactive system. You can think of this as the motor behind the FRP. This should _always be_ less than the audio clock rate. The closer to the audio clock rate, the more likely there will be dropped frames because the system doesn't poll fast enough to make the audio deadline. The closer to `0`, the less responsive your UI will be. In general, `5ms` less than the _audio clock rate_ is a safe bet. So if your audio clock rate is `20`, this should be `15`.

## Bundling on your site

To see how to bundle this library on your site, please visit the [examples](./examples) directory.

To compile the JS for the hello world example, issue the following command:

```bash
spago -x examples.dhall bundle-app \
  --main FRP.Behavior.Audio.Example.HelloWorld \
  --to examples/hello-world/index.js
```

Other examples will work the same way, with the directory and module name changing.

You will also need to copy all of the files from the `custom-units` folter into your project folder. With a correct setup, the hello-world directory should look like this:

```bash
examples/
  hello-world/
    HelloWorld.purs  # incldued in the git distro
    index.html # incldued in the git distro
    index.js # the generated js from spago bundle-app
    ps-aud-mul.js # plus any other files from the custom-units folder
```
