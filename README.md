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
