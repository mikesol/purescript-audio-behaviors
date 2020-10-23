("use strict");

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
exports.makeAudioContext = function () {
  return new (window.AudioContext || window.webkitAudioContext)();
};

exports.makeAudioTrack = function (s) {
  return function () {
    var o = new Audio(s);
    o.crossOrigin = "anonymous";
    return o;
  };
};
exports.decodeAudioDataFromBase64EncodedString = function (ctx) {
  return function (s) {
    return function () {
      {
        function base64ToArrayBuffer(base64) {
          var binaryString = window.atob(base64);
          var len = binaryString.length;
          var bytes = new Uint8Array(len);
          for (var i = 0; i < len; i++) {
            bytes[i] = binaryString.charCodeAt(i);
          }
          return bytes.buffer;
        }
        return ctx.decodeAudioData(base64ToArrayBuffer(s));
      }
    };
  };
};
exports.decodeAudioDataFromUri = function (ctx) {
  return function (s) {
    return function () {
      {
        return fetch(s)
          .then(function (b) {
            return b.arrayBuffer();
          })
          .then(function (b) {
            return ctx.decodeAudioData(b);
          });
      }
    };
  };
};
exports.audioWorkletAddModule = function (ctx) {
  return function (s) {
    return function () {
      {
        return ctx.audioWorklet.addModule(s);
      }
    };
  };
};
exports.makeAudioBuffer = function (ctx) {
  return function (b) {
    return function () {
      var myArrayBuffer = ctx.createBuffer(
        b.value1.length,
        b.value1[0].length,
        b.value0
      );
      for (
        var channel = 0;
        channel < myArrayBuffer.numberOfChannels;
        channel++
      ) {
        var nowBuffering = myArrayBuffer.getChannelData(channel);
        for (var i = 0; i < myArrayBuffer.length; i++) {
          nowBuffering[i] = b.value1[channel][i];
        }
      }
      return myArrayBuffer;
    };
  };
};

exports.makePeriodicWaveImpl = function (ctx) {
  return function (real_) {
    return function (imag_) {
      return function () {
        var real = new Float32Array(real_.length);
        var imag = new Float32Array(imag_.length);
        for (var i = 0; i < real_.length; i++) {
          real[i] = real_[i];
        }
        for (var i = 0; i < imag_.length; i++) {
          imag[i] = imag_[i];
        }
        return ctx.createPeriodicWave(real, imag, {
          disableNormalization: true,
        });
      };
    };
  };
};

exports.makeFloatArray = function (fa) {
  return function () {
    var r = new Float32Array(fa.length);
    for (var i = 0; i < fa.length; i++) {
      r[i] = fa[i];
    }
    return r;
  };
};

exports.touchAudio = function (predicates) {
  return function (timeToSet) {
    return function (instructions) {
      return function (context) {
        return function (audioInfo) {
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
                if (predicates.isDisconnectFrom(c)) {
                  generators[c.value0].disconnect(generators[c.value1]);
                } else if (predicates.isConnectTo(c)) {
                  if (predicates.isNothing(c.value2)) {
                    generators[c.value0].connect(generators[c.value1]);
                  } else {
                    generators[c.value0].connect(
                      generators[c.value1],
                      c.value2.value0.value0,
                      c.value2.value0.value1
                    );
                  }
                } else if (predicates.isShuffle(c)) {
                  var old = generators;
                  var generators = new Array(c.value0.length);
                  for (var j = 0; j < c.value0.length; j++) {
                    generators[c.value0[j].value1] = old[c.value0[j].value0];
                  }
                } else if (predicates.isNewUnit(c)) {
                  generators[c.value0] = predicates.isSpeaker(c.value1)
                    ? context.destination
                    : predicates.isMicrophone(c.value1)
                    ? context.createMediaStreamSource(
                        audioInfo.microphones[
                          Object.keys(audioInfo.microphones)[0]
                        ]
                      )
                    : predicates.isPlay(c.value1)
                    ? context.createMediaElementSource(
                        audioInfo.tracks[c.value3.value0]
                      )
                    : predicates.isPlayBuf(c.value1)
                    ? context.createBufferSource()
                    : predicates.isLoopBuf(c.value1)
                    ? context.createBufferSource()
                    : predicates.isIIRFilter(c.value1)
                    ? context.createIIRFilter(
                        c.value6.value0.value0,
                        c.value6.value0.value1
                      )
                    : predicates.isLowpass(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isBandpass(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isLowshelf(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isHighshelf(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isNotch(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isAllpass(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isPeaking(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isHighpass(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isConvolver(c.value1)
                    ? context.createConvolver()
                    : predicates.isDynamicsCompressor(c.value1)
                    ? context.createDynamicsCompressor()
                    : predicates.isSawtoothOsc(c.value1)
                    ? context.createOscillator()
                    : predicates.isTriangleOsc(c.value1)
                    ? context.createOscillator()
                    : predicates.isPeriodicOsc(c.value1)
                    ? context.createOscillator()
                    : predicates.isWaveShaper(c.value1)
                    ? context.createWaveShaper()
                    : predicates.isDup(c.value1)
                    ? context.createGain()
                    : predicates.isStereoPanner(c.value1)
                    ? context.createStereoPanner()
                    : predicates.isPanner(c.value1)
                    ? context.createPanner()
                    : predicates.isSinOsc(c.value1)
                    ? context.createOscillator()
                    : predicates.isSquareOsc(c.value1)
                    ? context.createOscillator()
                    : predicates.isMul(c.value1)
                    ? (function () {
                        var nConnections = 0;
                        for (var j = 0; j < instructions.length; j++) {
                          // this hack is necessary because
                          // custom audio worklets need explicit
                          // channel assignments. maybe make explicit everywhere?
                          var d = instructions[j];
                          if (
                            predicates.isConnectTo(d) &&
                            d.value1 == c.value0
                          ) {
                            d.value2 = predicates.justly(
                              predicates.tupply(0)(nConnections)
                            );
                            nConnections += 1;
                          }
                        }
                        return new AudioWorkletNode(context, "ps-aud-mul", {
                          numberOfInputs: nConnections,
                          numberOfOutputs: 1,
                        });
                      })()
                    : predicates.isAudioWorkletGenerator(c.value1) ||
                      predicates.isAudioWorkletProcessor(c.value1) ||
                      predicates.isAudioWorkletAggregator(c.value1)
                    ? (function () {
                        var initialParams = {};
                        for (var j = 0; j < instructions.length; j++) {
                          var d = instructions[j];
                          if (
                            predicates.isSetCustomParam(d) &&
                            d.value0 == c.value0
                          ) {
                            initialParams[d.value1] = d.value2;
                          }
                        }
                        if (predicates.isAudioWorkletAggregator(c.value1)) {
                          var nConnections = 0;
                          for (var j = 0; j < instructions.length; j++) {
                            // this hack is necessary because
                            // custom audio worklets need explicit
                            // channel assignments. maybe make explicit everywhere?
                            var d = instructions[j];
                            if (
                              predicates.isConnectTo(d) &&
                              d.value1 == c.value0
                            ) {
                              d.value2 = predicates.justly(
                                predicates.tupply(0)(nConnections)
                              );
                              nConnections += 1;
                            }
                          }
                        }
                        return new AudioWorkletNode(context, c.value3.value0, {
                          numberOfInputs: predicates.isAudioWorkletGenerator(
                            c.value1
                          )
                            ? 0
                            : predicates.isAudioWorkletProcessor(c.value1)
                            ? 1
                            : 2,
                          numberOfOutputs: 1,
                          parameterData: initialParams,
                        });
                      })()
                    : predicates.isAdd(c.value1)
                    ? context.createGain()
                    : predicates.isDelay(c.value1)
                    ? context.createDelay(10.0) // magic number for 10 seconds...make tweakable?
                    : predicates.isConstant(c.value1)
                    ? context.createConstantSource()
                    : predicates.isGain(c.value1)
                    ? context.createGain()
                    : predicates.isSplitRes(c.value1)
                    ? context.createGain()
                    : predicates.isDupRes(c.value1)
                    ? context.createGain()
                    : predicates.isSplitter(c.value1)
                    ? context.createChannelSplitter(c.value2.value0)
                    : predicates.isMerger(c.value1)
                    ? context.createChannelMerger(c.value2.value0)
                    : null;
                  if (predicates.isSinOsc(c.value1)) {
                    generators[c.value0].type = "sine";
                    generators[c.value0].start(timeToSet + c.value4.value0);
                  } else if (predicates.isLoopBuf(c.value1)) {
                    generators[c.value0].loop = true;
                    generators[c.value0].buffer =
                      audioInfo.buffers[c.value3.value0];
                    generators[c.value0].start(timeToSet + c.value4.value0);
                  } else if (predicates.isWaveShaper(c.value1)) {
                    generators[c.value0].curve =
                      audioInfo.floatArrays[c.value3.value0];
                  } else if (predicates.isConvolver(c.value1)) {
                    generators[c.value0].buffer =
                      audioInfo.buffers[c.value3.value0];
                  } else if (predicates.isPlayBuf(c.value1)) {
                    generators[c.value0].loop = false;
                    generators[c.value0].buffer =
                      audioInfo.buffers[c.value3.value0];
                    generators[c.value0].start(
                      timeToSet + c.value4.value0,
                      c.value5.value0
                    );
                  } else if (predicates.isPlay(c.value1)) {
                    // todo - if the same element is resumed via play it won't
                    // work in the current setup
                    // this is because there is a 1-to-1 relationship between source
                    // and media element
                    // the current workaround is to create multiple media elements.
                    // todo - add delay somehow...
                    audioInfo.tracks[c.value3.value0].play();
                  } else if (predicates.isConstant(c.value1)) {
                    generators[c.value0].start(timeToSet + c.value4.value0);
                  } else if (predicates.isLowpass(c.value1)) {
                    generators[c.value0].type = "lowpass";
                  } else if (predicates.isBandpass(c.value1)) {
                    generators[c.value0].type = "bandpass";
                  } else if (predicates.isLowshelf(c.value1)) {
                    generators[c.value0].type = "lowshelf";
                  } else if (predicates.isHighshelf(c.value1)) {
                    generators[c.value0].type = "highshelf";
                  } else if (predicates.isNotch(c.value1)) {
                    generators[c.value0].type = "notch";
                  } else if (predicates.isAllpass(c.value1)) {
                    generators[c.value0].type = "allpass";
                  } else if (predicates.isPeaking(c.value1)) {
                    generators[c.value0].type = "peaking";
                  } else if (predicates.isHighpass(c.value1)) {
                    generators[c.value0].type = "highpass";
                  } else if (predicates.isSquareOsc(c.value1)) {
                    generators[c.value0].type = "square";
                    generators[c.value0].start(timeToSet + c.value4.value0);
                  } else if (predicates.isTriangleOsc(c.value1)) {
                    generators[c.value0].type = "triangle";
                    generators[c.value0].start(timeToSet + c.value4.value0);
                  } else if (predicates.isSawtoothOsc(c.value1)) {
                    generators[c.value0].type = "sawtooth";
                    generators[c.value0].start(timeToSet + c.value4.value0);
                  } else if (predicates.isPeriodicOsc(c.value1)) {
                    // generators[c.value0].type = "custom";
                    generators[c.value0].setPeriodicWave(
                      audioInfo.periodicWaves[c.value3.value0]
                    );
                    generators[c.value0].start(timeToSet + c.value4.value0);
                  } else if (predicates.isSplitRes(c.value1)) {
                    generators[c.value0].gain.linearRampToValueAtTime(
                      1.0,
                      timeToSet
                    );
                  } else if (predicates.isDupRes(c.value1)) {
                    generators[c.value0].gain.linearRampToValueAtTime(
                      1.0,
                      timeToSet
                    );
                  }
                } else if (predicates.isSetFrequency(c)) {
                  generators[c.value0].frequency.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetPan(c)) {
                  generators[c.value0].pan.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetGain(c)) {
                  generators[c.value0].gain.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetQ(c)) {
                  generators[c.value0].Q.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetBuffer(c)) {
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
                } else if (predicates.isSetDelay(c)) {
                  generators[c.value0].delayTime.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetOffset(c)) {
                  generators[c.value0].offset.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetLoopStart(c)) {
                  generators[c.value0].loopStart = c.value1;
                } else if (predicates.isSetLoopEnd(c)) {
                  generators[c.value0].loopEnd = c.value1;
                } else if (predicates.isSetOversample(c)) {
                  generators[c.value0].oversample = c.value1;
                } else if (predicates.isSetCurve(c)) {
                  var curve = new Float32Array(c.value1.length);
                  for (var i = 0; i < c.value1.length; i++) {
                    curve[i] = c.value1[i];
                  }

                  generators[c.value0].curve = curve;
                } else if (predicates.isSetPlaybackRate(c)) {
                  generators[c.value0].playbackRate.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetThreshold(c)) {
                  generators[c.value0].threshold.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetKnee(c)) {
                  generators[c.value0].knee.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetRatio(c)) {
                  generators[c.value0].ratio.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetAttack(c)) {
                  generators[c.value0].attack.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetRelease(c)) {
                  generators[c.value0].release.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetCustomParam(c)) {
                  generators[c.value0].parameters
                    .get(c.value1)
                    .linearRampToValueAtTime(c.value2, timeToSet + c.value3);
                } else if (predicates.isStop(c)) {
                  generators[c.value0].stop();
                } else if (predicates.isSetConeInnerAngle(c)) {
                  generators[c.value0].coneInnerAngle = c.value1;
                } else if (predicates.isSetConeOuterAngle(c)) {
                  generators[c.value0].coneOuterAngle = c.value1;
                } else if (predicates.isSetConeOuterGain(c)) {
                  generators[c.value0].coneOuterGain = c.value1;
                } else if (predicates.isSetDistanceModel(c)) {
                  generators[c.value0].distanceModel = c.value1;
                } else if (predicates.isSetMaxDistance(c)) {
                  generators[c.value0].maxDistance = c.value1;
                } else if (predicates.isSetOrientationX(c)) {
                  generators[c.value0].orientationX.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetOrientationY(c)) {
                  generators[c.value0].orientationY.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetOrientationZ(c)) {
                  generators[c.value0].orientationZ.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetPanningModel(c)) {
                  generators[c.value0].panningModel = c.value1;
                } else if (predicates.isSetPositionX(c)) {
                  generators[c.value0].positionX.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetPositionY(c)) {
                  generators[c.value0].positionY.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetPositionZ(c)) {
                  generators[c.value0].positionZ.linearRampToValueAtTime(
                    c.value1,
                    timeToSet + c.value2
                  );
                } else if (predicates.isSetRefDistance(c)) {
                  generators[c.value0].refDistance = c.value1;
                } else if (predicates.isSetRolloffFactor(c)) {
                  generators[c.value0].rolloffFactor = c.value1;
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

exports.getAudioClockTime = function (ctx) {
  return function () {
    return ctx.currentTime;
  };
};

exports.getBoundingClientRect = function (canvas) {
  return function () {
    var o = canvas.getBoundingClientRect();
    return {
      x: o.left,
      y: o.top,
      width: o.right - o.left,
      height: o.bottom - o.top,
    };
  };
};
