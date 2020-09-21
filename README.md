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

This library uses the [behaviors pattern](https://wiki.haskell.org/Functional_Reactive_Programming) pioneered by Conal Elliott and Paul Hudak. You describe the way audio should behave at a given time, and a sampling function makes sure tha the audio is rendered correctly.

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

In this section, we'll build a scene from the ground up. In doing so, we'll accomplish four things:

1. Getting a static sound to play.
1. Adding sound via the microphone.
1. Adding playback from an `audio` tag.
1. Going from mono to stereo.
1. Getting the sound to change as a function of time.
1. Getting the sound to change as a function of a mouse input event.
1. Making sure that certain sounds occur at a precise time.

### Getting a static sound to play

Let's start with a sine wave at A440.

```haskell
scene :: Behavior Number -> Behavior (AudioUnit D1)
scene _ = pure (speaker' $ sinOsc 440.0)
```

Note that, because this function does not depend on time, we can ignore the input.

### Going from mono to stereo

Another thing to note is the `D1` in the signature. `purescript-audio-behaviors` uses types to describe channels. Here, D1 signifies that the output is mono. If you try to change it to anything else (ie D2 for stereo) it will break. That is because a sine wave oscillator is always mono. To make it stereo, you'd use the `dup` audio unit to duplicate the sound and the `merger` audio unit to merge the two channels into a single sound.

```haskell
scene :: Behavior Number -> Behavior (AudioUnit D2)
scene _ = let d = dup $ sinOsc 440.0 in pure (speaker $ (merger (d +> d +> empty)))
```

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
