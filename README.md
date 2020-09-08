# purescript-audio-behaviors

Somewhat broken, flaky, unready for production experiment in using [`purescript-behaviors`](https://github.com/paf31/purescript-behaviors) for web audio.

## Demo

[Here](https://spiritual-trade.surge.sh/). Kind of works in Firefox (a bit garbled). Chrome isn't fast enough yet.

## Installation

```bash
spago install
```

## Build

```bash
spago build
```

## Running

Test with Firefox at http://localhost:8000 by using the command below.

```bash
mkdir dist && cp client/index.html dist/ && spago -x client.dhall bundle-app --main Client.Main -t dist/index.js
cd dist
python -m http.server
```

## Wasm

Compiling to wasm is possible, if not painful. Here are the steps I took.

1. Install [`purescript-native`](https://github.com/andyarvanitis/purescript-native)
1. Clone my fork of the [`purescript-native-ffi`](https://github.com/mikesol/purescript-native-ffi) into a directory called ffi.
1. Make sure that you have [emscripten](https://emscripten.org/docs/getting_started/downloads.html#platform-notes-installation-instructions-sdk) installed and the toolchain is in your path.
1. Run `make release`. You may hit issues with `purescript-native` not compiling some files. Just edit these by hand - the fix is usually a one liner. For examples of issues I reported and how to resolve them, check [here](https://github.com/andyarvanitis/purescript-native/issues/57) and [here](https://github.com/andyarvanitis/purescript-native/issues/58).
1. Find the comment with the word `abovethisline` in `cpptest/purescript-worklet-processor.js` (it's at the beginning of the file, should be on the first line). Copy the entire contents of `cpptest/pure-script-kernel.wasmmodule.js` above this line.
1. `cd cpptest && python -m http.server`

Even with this, wasm is no better than JavaScript due to the intense amount of memory copying.

Note that `PureScriptKernel.cc` uses a hardcoded function from `cpptest/Main.purs` with some changes. If you want to change the function and recompile, you have to copy these changes from `Main.cpp` to `PureScriptKernel.cc`

Obviously this is not ideal and I'll polish the flow if I have time.
