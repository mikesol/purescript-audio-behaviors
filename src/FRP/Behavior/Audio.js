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
 *   String -> -- name
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
