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
2. Getting the sound to change as a function of time.
3. Getting the sound to change as a function of a keyboard input event.
4. Making sure that certain sounds occur at a precise time.

## Advanced usage

Here are some tips for advanced usage of `purescript-audio-behaviors`.

### Named units

As you build larger and larger audio structures, you may notice some stuttering in your application. Basically, the more units that exist, the more work the library has to do to keep track of them, and it can result in throttling a rendering frame.

One way to mitigate this significantly (like 10x significantly) is to give your audio units names. The more named audio units, the faster the computation will go.

Giving names is a bit tedious, so the recommendation is to build audio without names first and then, once you're satisfied with your scene, to give everything names. Here is how you attribute names to audio units:

```haskell

```

If you are building an audio function that is supposed to be reused (ie your own filter, etc), named audio is a great idea, as it will speed up computation everywhere the function is used.

```haskell

```

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
