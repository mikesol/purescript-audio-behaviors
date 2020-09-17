# purescript-audio-behaviors

[`purescript-behaviors`](https://github.com/paf31/purescript-behaviors) for web audio.

## Demo

[Here](https://klank-hello-world.surge.sh/). If it doesn't turn on when you click "turn on", reload and try again.

```haskell
scene :: Behavior (AudioUnit D1)
scene = f <$> (unwrap <$> seconds)
  where
  f s =
    let
      rad = pi * s
    in
      speaker
        $ ( (gain' 0.1 $ sinOsc (440.0 + (10.0 * sin (2.3 * rad))))
              :| (gain' 0.25 $ sinOsc (235.0 + (10.0 * sin (1.7 * rad))))
              : (gain' 0.2 $ sinOsc (337.0 + (10.0 * sin rad)))
              : Nil
          )
```

## Installation

```bash
spago install
```

## Build

```bash
spago build
```

## Example

A working example is in the [examples](./examples) directory.

To compile the JS for the hello world example, issue the following command:

```bash
spago -x examples.dhall bundle-app --main FRP.Behavior.Audio.Example.HelloWorld --to examples/hello-world/index.js
```

Other examples will work the same way, with the directory and module name changing.

This example relies on `glpk.js`, which can be found [here](https://github.com/jvail/glpk.js). The files `glpk-worker.js` and `glpk-worker.wasm` need to be compiled by running `make all` copied to the directory of the project (ie [examples/hello-world](./examples/hello-world)).  You'll need an [emscripten toolchain](https://emscripten.org/docs/getting_started/downloads.html#platform-notes-installation-instructions-sdk) to compile this.

Note that, due to a bug in `glpk-4.65`, there is a spurious console message that sometimes prints.  The way around it is to apply [this patch](https://bugs.debian.org/cgi-bin/bugreport.cgi?att=1;bug=891465;filename=simplex-warning.patch;msg=5) to glpk before compiling it.