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

exports.touchAudio = function (/**dictHomogeneous */) {
  return function (timeToSet) {
    return function (instructions) {
      return function (context) {
        return function (stream) {
          return function (sources) {
            return function (g) {
              return function () {
                // should never happen
                if (timeToSet < context.currentTime) {
                  console.warn(
                    "Programming error: we are setting in the past",
                    timeToSet,
                    context.currentTime
                  );
                  timeToSet = context.currentTime;
                }
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
                    var generators = new Array(c.value0.length);
                    for (var j = 0; j < c.value0.length; j++) {
                      generators[c.value0[j].value1] = old[c.value0[j].value0];
                    }
                  } else if (c.constructor.name == "NewUnit") {
                    generators[c.value0] =
                      c.value1.constructor.name == "Speaker$prime$prime"
                        ? context.destination
                        : c.value1.constructor.name == "Microphone$prime$prime"
                        ? context.createMediaStreamSource(stream)
                        : c.value1.constructor.name == "Play$prime$prime"
                        ? context.createMediaElementSource(
                            sources[c.value3.value0]
                          )
                        : c.value1.constructor.name == "PlayBuf$prime$prime"
                        ? context.createBufferSource()
                        : c.value1.constructor.name == "LoopBuf$prime$prime"
                        ? context.createBufferSource()
                        : c.value1.constructor.name ==
                          "PlayDynamicBuf$prime$prime"
                        ? context.createBufferSource()
                        : c.value1.constructor.name ==
                          "LoopDynamicBuf$prime$prime"
                        ? context.createBufferSource()
                        : c.value1.constructor.name == "Lowpass$prime$prime"
                        ? context.createBiquadFilter()
                        : c.value1.constructor.name == "Bandpass$prime$prime"
                        ? context.createBiquadFilter()
                        : c.value1.constructor.name == "Lowshelf$prime$prime"
                        ? context.createBiquadFilter()
                        : c.value1.constructor.name == "Highshelf$prime$prime"
                        ? context.createBiquadFilter()
                        : c.value1.constructor.name == "Notch$prime$prime"
                        ? context.createBiquadFilter()
                        : c.value1.constructor.name == "Allpass$prime$prime"
                        ? context.createBiquadFilter()
                        : c.value1.constructor.name == "Peaking$prime$prime"
                        ? context.createBiquadFilter()
                        : c.value1.constructor.name == "Highpass$prime$prime"
                        ? context.createBiquadFilter()
                        : c.value1.constructor.name ==
                          "DynamicConvolver$prime$prime"
                        ? context.createConvolver()
                        : c.value1.constructor.name == "Convolver$prime$prime"
                        ? context.createConvolver()
                        : c.value1.constructor.name ==
                          "DynamicsCompressor$prime$prime"
                        ? context.createDynamicsCompressor()
                        : c.value1.constructor.name == "SawtoothOsc$prime$prime"
                        ? context.createOscillator()
                        : c.value1.constructor.name == "TriangleOsc$prime$prime"
                        ? context.createOscillator()
                        : c.value1.constructor.name == "PeriodicOsc$prime$prime"
                        ? context.createOscillator()
                        : c.value1.constructor.name ==
                          "DynamicPeriodicOsc$prime$prime"
                        ? context.createOscillator()
                        : c.value1.constructor.name ==
                          "DynamicWaveShaper$prime$prime"
                        ? context.createWaveShaper()
                        : c.value1.constructor.name == "WaveShaper$prime$prime"
                        ? context.createWaveShaper()
                        : c.value1.constructor.name == "Dup$prime$prime"
                        ? context.createGain()
                        : c.value1.constructor.name ==
                          "StereoPanner$prime$prime"
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
                      generators[c.value0].start(timeToSet + c.value4.value0);
                    } else if (
                      c.value1.constructor.name == "LoopBuf$prime$prime"
                    ) {
                      generators[c.value0].loop = true;
                      generators[c.value0].buffer = sources[c.value3.value0];
                      generators[c.value0].start(timeToSet + c.value4.value0);
                    } else if (
                      c.value1.constructor.name == "WaveShaper$prime$prime"
                    ) {
                      generators[c.value0].curve = sources[c.value3.value0];
                    } else if (
                      c.value1.constructor.name == "Convolver$prime$prime"
                    ) {
                      generators[c.value0].buffer = sources[c.value3.value0];
                    } else if (
                      c.value1.constructor.name == "PlayBuf$prime$prime"
                    ) {
                      generators[c.value0].loop = false;
                      generators[c.value0].buffer = sources[c.value3.value0];
                      generators[c.value0].start(timeToSet + c.value4.value0);
                    } else if (
                      c.value1.constructor.name == "LoopDynamicBuf$prime$prime"
                    ) {
                      generators[c.value0].loop = true;
                      generators[c.value0].start(timeToSet + c.value4.value0);
                    } else if (
                      c.value1.constructor.name == "PlayDynamicBuf$prime$prime"
                    ) {
                      generators[c.value0].loop = false;
                      generators[c.value0].start(timeToSet + c.value4.value0);
                    } else if (
                      c.value1.constructor.name == "Play$prime$prime"
                    ) {
                      // todo - add delay somehow...
                      sources[c.value3.value0].play();
                    } else if (
                      c.value1.constructor.name == "Constant$prime$prime"
                    ) {
                      generators[c.value0].start(timeToSet + c.value4.value0);
                    } else if (
                      c.value1.constructor.name == "Lowpass$prime$prime"
                    ) {
                      generators[c.value0].type = "lowpass";
                    } else if (
                      c.value1.constructor.name == "Bandpass$prime$prime"
                    ) {
                      generators[c.value0].type = "bandpass";
                    } else if (
                      c.value1.constructor.name == "Lowshelf$prime$prime"
                    ) {
                      generators[c.value0].type = "lowshelf";
                    } else if (
                      c.value1.constructor.name == "Highshelf$prime$prime"
                    ) {
                      generators[c.value0].type = "highshelf";
                    } else if (
                      c.value1.constructor.name == "Notch$prime$prime"
                    ) {
                      generators[c.value0].type = "notch";
                    } else if (
                      c.value1.constructor.name == "Allpass$prime$prime"
                    ) {
                      generators[c.value0].type = "allpass";
                    } else if (
                      c.value1.constructor.name == "Peaking$prime$prime"
                    ) {
                      generators[c.value0].type = "peaking";
                    } else if (
                      c.value1.constructor.name == "Highpass$prime$prime"
                    ) {
                      generators[c.value0].type = "highpass";
                    } else if (
                      c.value1.constructor.name == "SquareOsc$prime$prime"
                    ) {
                      generators[c.value0].type = "square";
                      generators[c.value0].start(timeToSet + c.value4.value0);
                    } else if (
                      c.value1.constructor.name == "TriangleOsc$prime$prime"
                    ) {
                      generators[c.value0].type = "triangle";
                      generators[c.value0].start(timeToSet + c.value4.value0);
                    } else if (
                      c.value1.constructor.name == "SawtoothOsc$prime$prime"
                    ) {
                      generators[c.value0].type = "sawtooth";
                      generators[c.value0].start(timeToSet + c.value4.value0);
                    } else if (
                      c.value1.constructor.name == "PeriodicOsc$prime$prime"
                    ) {
                      generators[c.value0].type = "custom";
                      generators[c.value0].setPeriodicWave(
                        sources[c.value3.value0]
                      );
                      generators[c.value0].start(timeToSet + c.value4.value0);
                    } else if (
                      c.value1.constructor.name ==
                      "DynamicPeriodicOsc$prime$prime"
                    ) {
                      generators[c.value0].type = "custom";
                      generators[c.value0].start(timeToSet + c.value4.value0);
                    } else if (
                      c.value1.constructor.name == "SplitRes$prime$prime"
                    ) {
                      generators[c.value0].gain.setValueAtTime(1.0, timeToSet);
                    } else if (
                      c.value1.constructor.name == "DupRes$prime$prime"
                    ) {
                      generators[c.value0].gain.setValueAtTime(1.0, timeToSet);
                    }
                  } else if (c.constructor.name == "SetFrequency") {
                    generators[c.value0].frequency.setValueAtTime(
                      c.value1,
                      timeToSet + c.value2
                    );
                  } else if (c.constructor.name == "SetPan") {
                    generators[c.value0].pan.setValueAtTime(
                      c.value1,
                      timeToSet + c.value2
                    );
                  } else if (c.constructor.name == "SetGain") {
                    generators[c.value0].gain.setValueAtTime(
                      c.value1,
                      timeToSet + c.value2
                    );
                  } else if (c.constructor.name == "SetQ") {
                    generators[c.value0].Q.setValueAtTime(
                      c.value1,
                      timeToSet + c.value2
                    );
                  } else if (c.constructor.name == "SetBuffer") {
                    var myArrayBuffer = context.createBuffer(
                      c.value2.length,
                      c.value2[0].length,
                      c.value1
                    );
                    for (
                      var channel = 0;
                      channel < myArrayBuffer.numberOfChannels;
                      channel++
                    ) {
                      var nowBuffering = myArrayBuffer.getChannelData(channel);
                      for (var i = 0; i < myArrayBuffer.length; i++) {
                        nowBuffering[i] = c.value2[channel][i];
                      }
                    }
                    generators[c.value0].buffer = myArrayBuffer;
                  } else if (c.constructor.name == "SetDelay") {
                    generators[c.value0].delayTime.setValueAtTime(
                      c.value1,
                      timeToSet + c.value2
                    );
                  } else if (c.constructor.name == "SetOffset") {
                    generators[c.value0].offset.setValueAtTime(
                      c.value1,
                      timeToSet + c.value2
                    );
                  } else if (c.constructor.name == "SetLoopStart") {
                    generators[c.value0].loopStart.setValueAtTime(
                      c.value1,
                      timeToSet + c.value2
                    );
                  } else if (c.constructor.name == "SetLoopEnd") {
                    generators[c.value0].loopEnd.setValueAtTime(
                      c.value1,
                      timeToSet + c.value2
                    );
                  } else if (c.constructor.name == "SetOversample") {
                    generators[c.value0].oversample = c.value1;
                  } else if (c.constructor.name == "SetCurve") {
                    var curve = new Float32Array(c.value1.length);
                    for (var i = 0; i < c.value1.length; i++) {
                      curve[i] = c.value1[i];
                    }

                    generators[c.value0].curve = curve;
                  } else if (c.constructor.name == "SetPeriodicWave") {
                    var real = new Float32Array(c.value1.length);
                    var imag = new Float32Array(c.value2.length);
                    for (var i = 0; i < c.value1.length; i++) {
                      real[i] = c.value1[i];
                      imag[i] = c.value2[i];
                    }

                    var wave = context.createPeriodicWave(real, imag, {
                      disableNormalization: true,
                    });

                    generators[c.value0].setPeriodicWave(wave);
                  } else if (c.constructor.name == "SetPlaybackRate") {
                    generators[c.value0].playbackRate.setValueAtTime(
                      c.value1,
                      timeToSet + c.value2
                    );
                  } else if (c.constructor.name == "SetThreshold") {
                    generators[c.value0].threshold.setValueAtTime(
                      c.value1,
                      timeToSet + c.value2
                    );
                  } else if (c.constructor.name == "SetKnee") {
                    generators[c.value0].knee.setValueAtTime(
                      c.value1,
                      timeToSet + c.value2
                    );
                  } else if (c.constructor.name == "SetRatio") {
                    generators[c.value0].ratio.setValueAtTime(
                      c.value1,
                      timeToSet + c.value2
                    );
                  } else if (c.constructor.name == "SetAttack") {
                    generators[c.value0].attack.setValueAtTime(
                      c.value1,
                      timeToSet + c.value2
                    );
                  } else if (c.constructor.name == "SetRelease") {
                    generators[c.value0].release.setValueAtTime(
                      c.value1,
                      timeToSet + c.value2
                    );
                  } else if (c.constructor.name == "Stop") {
                    generators[c.value0].stop();
                  }
                }
                return generators;
              };
            };
          };
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

exports.getAudioClockTime = function (ctx) {
  return function () {
    return ctx.currentTime;
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
