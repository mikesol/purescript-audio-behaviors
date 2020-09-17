# Custom units

`purescript-audio-behaviors` works best using predefined audio units. It is also possible to roll your own audio units in JavaScript or C++ using this library.  I would recommend _not_ doing this.  I tried it, and it winds up being super slow.  Audio units need to be lightening fast, which means always using the most efficient operations.  FRP is simply not efficient on the sample level.

## Js

A working example of a FRP-based audio unit is in the [client](./client) directory.  It currently has three simultaneous delay lines.  Anything more than that will be really choppy.

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

## tl;dr

Don't use this library to create custom audio units. Make your custom units using non-reactive code, and then use this library for the reactive layer @ the control rate.