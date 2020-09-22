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
scene :: Behavior Number -> Behavior (AudioUnit D1)
scene time = f <$> time
  where
  f s =
    let
      rad = pi * s
    in
      speaker
        $ ( (gain' 0.1 $ sinOsc (440.0 + (10.0 * sin (2.3 * rad))))
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

### Getting a static sound to play

Let's start with a sine wave at A440 playing at a volume of `0.5` (where `1.0` is the loudest volume).

```haskell
scene :: Behavior Number -> Behavior (AudioUnit D1)
scene _ = pure (speaker' $ (gain' 0.5 $ sinOsc 440.0))
```

Note that, because this function does not depend on time, we can ignore the input.

### Adding sound via the microphone

Let's add our voice to the mix! We'll put it above a nice low drone.

```haskell
scene :: Behavior Number -> Behavior (AudioUnit D1)
scene _ =
  pure
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

```haskell
-- assuming we have passed in an object
-- with { forest: new Audio("my-recording.mp3") }
-- to `runInBrowser`
scene :: Behavior Number -> Behavior (AudioUnit D1)
scene _ =
  pure
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
scene :: Behavior Number -> Behavior (AudioUnit D2)
scene _ =
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
```

### Getting the sound to change as a function of time

Up until this point, our audio hasn't reacted to many behaviors. Let's fix that! One behavior to react to is the passage of time. Let's add a slow undulation to the lowest pitch in the drone that is based on the passage of time

```haskell
scene :: Behavior Number -> Behavior (AudioUnit D2)
scene time = f <$> time
  where
  f s =
    let
      rad = pi * s
    in
      dup1
        ( (gain' 0.2 $ sinOsc (110.0 + (3.0 * sin (0.5 * rad))))
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

The next snippet of code is taken right out of the original `purescript-behaviors` library by [Phil Freeman](https://github.com/paf31). Let's create a "swelling" effect on our upper sine wave when we click the mouse.

```haskell
scene5 :: Mouse -> Behavior Number -> Behavior (AudioUnit D2)
scene5 mouse time = f <$> time <*> swell
  where
  f s sw =
    let
      rad = pi * s
    in
      dup1
        ( (gain' 0.2 $ sinOsc (110.0 + (3.0 * sin (0.5 * rad))))
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
  swell :: Behavior Number
  swell =
    fixB 2.0 \b ->
      integral' 2.0 (unwrap <$> Time.seconds)
        let
          db =
            fixB 10.0 \db_ ->
              integral'
                10.0
                (unwrap <$> Time.seconds)
                (f <$> buttons mouse <*> b <*> db_)
        in
          switcher db (down $> db)
    where
    f bs s ds
      | isEmpty bs = -8.0 * (s - 1.0) - ds * 2.0
      | otherwise = 2.0 * (4.0 - s)
```

The functions [`integral'`](https://pursuit.purescript.org/packages/purescript-behaviors/7.0.0/docs/FRP.Behavior#v:integral'), [`fixB`](https://pursuit.purescript.org/packages/purescript-behaviors/7.0.0/docs/FRP.Behavior#v:fixB), and [`switcher`](https://pursuit.purescript.org/packages/purescript-behaviors/7.0.0/docs/FRP.Behavior#v:switcher) all come from [`purescript-behaviors`](https://pursuit.purescript.org/packages/purescript-behaviors/7.0.0).

### Making sure that certain sounds occur at a precise time

Great audio is all about timing, but so far, we have been locked to scheduling events at multiples of the control rate. The _de facto_ control rate for this library is ~66Hz, which is way too slow to quantize complex rhythmic events.

To fix the control rate problem, parameters that can change in time like _frequency_ or _gain_ have an optional second parameter that specifies the offset, in seconds, from the current quantized value in the control rate.

For example, let's say the control rate is `66Hz` and you want a sound to trigger at _exactly_ `0.25` seconds. At this rate, the closest quantized value to `0.25` is `0.2424242424`, or `16/66`. That means that, when `time` is `0.24242424`, we will add an offset of `0.00757576` to the value to make sure that it happens "exactly" at `0.25` seconds. "Exactly" is in quoation marks because floating point arrithmentic will provoke a rounding error of around `0.000000000001`, but this is far smaller than the audio sample rate, so we will not hear it.

To finish our tutorial, let's add a small metronome on the inside of our sound. We will have it beat every `0.9` seconds.

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
                  Tuple (f + 0.9 * toNumber i) s
              )
              [ Tuple 0.0 0.0, Tuple 0.1 0.9, Tuple 0.3 0.3 ]
        )
        (range 0 100)

-- the control rate in seconds, or 66.66667 Hz
-- as we can set our own control rate (see Advanced usage below)
-- we know what this is
-- in klang.dev, the control rate is 1000.0 / 15
kr = 1000.0 / 15.0 :: Number

scene :: Mouse -> Behavior Number -> Behavior (AudioUnit D2)
scene mouse time = f <$> time <*> swell
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
          -- if in a control cycle with a peak or trough
          -- then we lock to that
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
            + (gainT' (gn s) $ sinOsc (220.0 + sw))
            + (gain' 0.1 $ sinOsc (220.0 + sw))
            + microphone
        ) \mono ->
        speaker
          $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                :| (gain' 0.5 $ (play "forest"))
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

```

### Conclusion

We've built from a simple sound all the way up to a complex, precisely-timed stereo structure that responds to mouse events. There are many more audio units in the library, such as filters, compressors and convolvers. Almost the whole [Web Audio API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API) is exposed.

To see a list of exported audio units, you can check out [`Audio.purs`](./src/FRP/Behavior/Audio.purs). In a future version of this, we will refactor things so that all of the audio units are in one package.

## Advanced usage

Here are some tips for advanced usage of `purescript-audio-behaviors`.

### Debugging our scene

### Named units

As you build larger and larger audio structures, you may notice some stuttering in your application. The more units that exist, the more work the library has to do to keep track of them, and it can result in throttling a rendering frame.

One way to mitigate this significantly (like 10x significantly) is to give your audio units names. The more named audio units, the faster the computation will go.

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

All of the parameters are passed to the function `runInBrowser`, which has the following signature:

```haskell
runInBrowser ::
  forall ch (a :: # Type).
  Homogeneous a Foreign =>
  Pos ch =>
  (Behavior Number -> Behavior (AudioUnit ch)) -> -- the scene
  Int -> -- audio clock rate
  Int -> -- driver rate
  Foreign -> -- audio context
  Foreign -> -- microphone if one exists
  Record a -> -- buffers, sound files, and wavetables
  Array Foreign -> -- web workers with glpk
  (
    Number ->
    Array Instruction ->
    Foreign ->
    Foreign ->
    Record a ->
    Array Foreign ->
    Effect (Array Foreign)
  ) -> -- renderer
  Effect (Effect Unit)
```

A lot of this is boilerplate, and you can see examples of how to hook this up in the [examples](./examples) directory. The three bits to understand here are:

- audio clock rate
- driver rate
- web workers with glpk

The _audio clock rate_ represents how many milliseconds are between control-rate pings to the scene. For example, if you set this to `20`, the scene will get polled every `0.02` seconds. In general, for most applications, somewhere between `10` and `20` is the sweet spot. Too low and you'll skip frames (jank), too high and you'll start hearing the quantization.

The _driver rate_ represents how many milliseconds are between pings to the entire reactive system. You can think of this as the motor behind the FRP. This should _always be_ less than the audio clock rate. The closer to the audio clock rate, the more likely there will be dropped frames because the system doesn't poll fast enough to make the audio deadline. The closer to `0`, the less responsive your UI will be. In general, `5ms` less than the _audio clock rate_ is a safe bet. So if your audio clock rate is `15`, this should be `10`.

Lastly, _web workers with glpk_ is an array of web workers with an emscripten port of GLPK preloaded. Here, more is always better in theory, but the danger is that anything over 8ish runs the risk of providing no marginal benefit, as you will max out the number of concurrent threads on which you can run glpk. I tend to use 8 for everything.

## Bundling on your site

To see how to bundle this library on your site, please visit the [examples](./examples) directory.

To compile the JS for the hello world example, issue the following command:

```bash
spago -x examples.dhall bundle-app \
  --main FRP.Behavior.Audio.Example.HelloWorld \
  --to examples/hello-world/index.js
```

Other examples will work the same way, with the directory and module name changing.

This library relies on on `glpk.js`, which can be found [here](https://github.com/jvail/glpk.js). The files `glpk-worker.js` and `glpk-worker.wasm` need to be compiled by running `make all` copied to the directory of the project (ie [examples/hello-world](./examples/hello-world)). You'll need an [emscripten toolchain](https://emscripten.org/docs/getting_started/downloads.html#platform-notes-installation-instructions-sdk) to compile this. You will also need to copy all of the files from the `custom-units` folter into your project folder. With a correct setup, the hello-world directory should look like this:

```bash
examples/
  hello-world/
    glpk-worker.js # compiled glpk js
    glpk-worker.wasm # compiled glpk wasm
    HelloWorld.purs  # incldued in the git distro
    index.html # incldued in the git distro
    index.js # the generated js from spago bundle-app
    ps-aud-mul.js # plus any other files from the custom-units folder
```

Note that, due to a bug in `glpk-4.65`, there is a spurious console message that sometimes prints. The way around it is to apply [this patch](https://bugs.debian.org/cgi-bin/bugreport.cgi?att=1;bug=891465;filename=simplex-warning.patch;msg=5) to glpk before compiling it.
