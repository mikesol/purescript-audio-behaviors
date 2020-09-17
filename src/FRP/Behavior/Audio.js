"use strict";

function copyAudio(inputs) {
  var out = new Array(inputs.length).fill(null);
  for (var i = 0; i < inputs.length; i++) {
    out[i] = new Array(inputs[i].length).fill(null);
    for (var j = 0; j < inputs[i].length; j++) {
      out[i][j] = Array.from(inputs[i][j]);
    }
  }
  return out;
}

function mergeAudio(retention, prev, inputs) {
  for (var i = 0; i < inputs.length; i++) {
    for (var j = 0; j < inputs[i].length; j++) {
      prev[i][j] = prev[i][j]
        .slice(Math.max(0, prev[i][j].length - retention))
        .concat(Array.from(inputs[i][j]));
    }
  }
}
/**
  String -> -- name
  Number -> -- retention
  (Effect Boolean -> Event Unit) -> -- driver
  (Effect Number -> Behavior Number) -> -- currentTime
  (Effect SampleFrame -> Behavior SampleFrame) -> -- sampleFrame
  (Effect r -> Behavior r) -> -- control params
  AudioProcessor r -> -- audio processor
  ( Event Unit ->
    Behavior SampleFrame ->
    (SampleFrame -> Effect Unit) ->
    Effect (Effect Unit)
  ) -> -- soundify
  Effect Unit
 */

exports._makeAudioWorkletProcessor = function (name) {
  return function (retention) {
    return function (defaults) {
      return function (driver) {
        return function (currentTime) {
          return function (sampleFrame) {
            return function (controlParams) {
              return function (processor) {
                return function (soundify) {
                  return function () {
                    class FRPWorkletProcessor extends AudioWorkletProcessor {
                      static get parameterDescriptors() {
                        var defaultKeys = Object.keys(defaults);
                        var out = new Array(defaultKeys.length).fill(null);
                        for (var i = 0; i < defaultKeys.length; i++) {
                          out[i] = {
                            name: defaultKeys[i],
                            defaultValue: defaults[defaultKeys[i]],
                          };
                        }
                        return out;
                      }
                      constructor() {
                        super();
                        this.sink = null;
                        this.curSample = 0;
                        this.defaultKeys = Object.keys(defaults);
                      }

                      process(inputs, outputs, parameters) {
                        if (inputs.length == 0) {
                          return true;
                        }
                        if (inputs[0].length == 0) {
                          return true;
                        }
                        if (this.sink == null) {
                          this.sink = copyAudio(inputs);
                        } else {
                          mergeAudio(
                            Math.floor(retention * sampleRate),
                            this.sink,
                            inputs
                          );
                        }
                        var sink = this.sink;
                        var futureSample =
                          this.currentSample + inputs[0][0].length;
                        var curpos = { i: 0 };
                        var curtime = { i: this.currentSample };
                        var curparam = { i: 0 };
                        var curout = { i: 0 };
                        var _driver = driver(function () {
                          return curpos.i++ < inputs[0][0].length;
                        });
                        var _behaviorCurrentTime = currentTime(function () {
                          var out = curtime.i / sampleRate;
                          curtime.i++;
                          return out;
                        });
                        var _secondsToBehaviorSampleFrame = function (
                          lookback
                        ) {
                          if (lookback < 0) {
                            lookback = 0;
                          }
                          var lookbackInSamples = Math.floor(
                            lookback * sampleRate
                          );
                          var curposInSink = {
                            i:
                              sink[0][0].length -
                              inputs[0][0].length -
                              lookbackInSamples,
                          };
                          return sampleFrame(function () {
                            var out = new Array(inputs.length).fill(
                              new Array(inputs[0].length).fill(0.0)
                            );
                            var cp = curposInSink.i++;
                            if (cp < 0) {
                              return out;
                            }
                            for (var i = 0; i < inputs.length; i++) {
                              for (var j = 0; j < inputs[0].length; j++) {
                                out[i][j] = sink[i][j][cp];
                              }
                            }
                            return out;
                          });
                        };
                        var _behaviorControlParams = controlParams(function () {
                          var cp = curparam.i;
                          curparam.i++;
                          var out = {};
                          for (var i = 0; i < defaultKeys.length; i++) {
                            var p = parameters[defaultKeys[i]];
                            if (p.length == 1) {
                              out[defaultKeys[i]] = p[0];
                            } else {
                              out[defaultKeys[i]] = p[cp];
                            }
                          }
                          return out;
                        });
                        var chain = processor(_behaviorCurrentTime)(
                          _secondsToBehaviorSampleFrame
                        )(_behaviorControlParams);
                        soundify(_driver)(chain)(function (frame) {
                          return function () {
                            for (var i = 0; i < outputs.length; i++) {
                              for (var j = 0; j < outputs[i].length; j++) {
                                outputs[i][j][curout.i] = frame[i][j];
                              }
                            }
                            curout.i++;
                          };
                        })(); // thunk to start processing
                        // audio processing code here.
                        this.currentSample = futureSample;
                        return true;
                      }
                    }

                    registerProcessor(name, FRPWorkletProcessor);
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};
exports.getGlpkImpl = function () {
  return require("glpk.js");
};

exports.touchAudio = function (instructions) {
  return function (context) {
    return function (stream) {
      return function (g) {
        return function () {
          var generators = g;
          for (var i = 0; i < instructions.length; i++) {
            var c = instructions[i];
            if (c.constructor.name == "DisconnectFrom") {
              generators[c.value0].disconnect(generators[c.value1]);
            } else if (c.constructor.name == "ConnectTo") {
              if (c.value2.constructor.name == "Nothing") {
                generators[c.value0].connect(generators[c.value1]);
              } else {
                generators[c.value0].connect(
                  generators[c.value1],
                  c.value2.value0.value0,
                  c.value2.value0.value1
                );
              }
            } else if (c.constructor.name == "Shuffle") {
              var old = generators;
              var generators = generators.slice(0);
              for (var j = 0; j < c.value0.length; j++) {
                generators[c.value0[j].value1] = old[c.value0[j].value0];
              }
            } else if (c.constructor.name == "NewUnit") {
              generators[c.value0] =
                c.value1.constructor.name == "Speaker$prime$prime"
                  ? context.destination
                  : c.value1.constructor.name == "Microphone$prime$prime"
                  ? context.createMediaStreamSource(stream)
                  : c.value1.constructor.name == "StereoPanner$prime$prime"
                  ? context.createStereoPanner()
                  : c.value1.constructor.name == "SinOsc$prime$prime"
                  ? context.createOscillator()
                  : c.value1.constructor.name == "SquareOsc$prime$prime"
                  ? context.createOscillator()
                  : c.value1.constructor.name == "Mul$prime$prime"
                  ? new AudioWorkletNode(context, "ps-aud-mul")
                  : c.value1.constructor.name == "Add$prime$prime"
                  ? context.createGain()
                  : c.value1.constructor.name == "Delay$prime$prime"
                  ? context.createDelay(10.0) // magic number for 10 seconds...make tweakable?
                  : c.value1.constructor.name == "Constant$prime$prime"
                  ? context.createConstantSource()
                  : c.value1.constructor.name == "Gain$prime$prime"
                  ? context.createGain()
                  : c.value1.constructor.name == "SplitRes$prime$prime"
                  ? context.createGain()
                  : c.value1.constructor.name == "Splitter$prime$prime"
                  ? context.createChannelSplitter(c.value2.value0)
                  : c.value1.constructor.name == "Merger$prime$prime"
                  ? context.createChannelMerger(c.value2.value0)
                  : null;
              if (c.value1.constructor.name == "SinOsc$prime$prime") {
                generators[c.value0].type = "sine";
                generators[c.value0].start();
              }
              if (c.value1.constructor.name == "SquareOsc$prime$prime") {
                generators[c.value0].type = "square";
                generators[c.value0].start();
              }
              if (c.value1.constructor.name == "SplitRes$prime$prime") {
                generators[c.value0].gain.setValueAtTime(
                  1.0,
                  context.currentTime
                );
              }
            } else if (c.constructor.name == "SetFrequency") {
              generators[c.value0].frequency.setValueAtTime(
                c.value1,
                context.currentTime
              );
            } else if (c.constructor.name == "SetPan") {
              generators[c.value0].pan.setValueAtTime(
                c.value1,
                context.currentTime
              );
            } else if (c.constructor.name == "SetGain") {
              generators[c.value0].gain.setValueAtTime(
                c.value1,
                context.currentTime
              );
            } else if (c.constructor.name == "SetDelay") {
              generators[c.value0].delayTime.setValueAtTime(
                c.value1,
                context.currentTime
              );
            } else if (c.constructor.name == "SetOffset") {
              generators[c.value0].offset.setValueAtTime(
                c.value1,
                context.currentTime
              );
            }
          }
          return generators;
        };
      };
    };
  };
};

exports.glpkWorkerImpl = function (worker) {
  return function (program) {
    return function () {
      return new Promise(function (resolve, reject) {
        worker.queue.push([resolve, reject]);
        worker.postMessage(program);
      });
    };
  };
};

exports.makeWorkers = function (n) {
  return function () {
    const workers = [];
    for (var i = 0; i < n; i++) {
      var w = new Worker("glpk-worker.js");
      workers.push(w);
      w.queue = [];
      w.onmessage = function (e) {
        if (e.data.initialized) {
          return;
        }
        var s = this.queue.shift();
        var o = e.data;
        return o.result.status !== 5 ? s[1]("Error") : s[0](o.result.vars);
      };
      w.onerror = function (err) {
        var s = this.queue.shift();
        s[1](err);
      };
    }
    return workers;
  };
};

exports._glpk = function (glpk) {
  return function (lp) {
    return function (left) {
      return function (right) {
        try {
          var o = glpk.solve(lp, glpk.GLP_MSG_OFF);
          return o.result.status !== 5 ? left : right(o.result.vars);
        } catch (e) {
          return left;
        }
      };
    };
  };
};
