// add-processor.js
class AddProcessor extends AudioWorkletProcessor {
  constructor(options) {
    super(options);
  }
  process(inputs, outputs, parameters) {
    const inputL = inputs[0];
    const inputR = inputs[1];
    const output = outputs[0];
    for (var i = 0; i < Math.min(inputL.length, inputR.length); i++) {
      for (var j = 0; j < inputL[i].length; j++) {
        output[i][j] = inputL[i][j] + inputR[i][j];
      }
    }
    return true;
  }
}

registerProcessor("add-processor", AddProcessor);
