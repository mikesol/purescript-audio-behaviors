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

## Bundling on your site

To see how to bundle this library on your site, please visit the [examples](./examples) directory.

To compile the JS for the hello world example, issue the following command:

```bash
spago -x examples.dhall bundle-app --main FRP.Behavior.Audio.Example.HelloWorld --to examples/hello-world/index.js
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
