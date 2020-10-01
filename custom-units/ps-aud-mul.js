class PsAudMulProcessor extends AudioWorkletProcessor {
  process(inputs, outputs, parameters) {
    if (outputs) {
      for (var i = 0; i < outputs.length; i++) {
        if (outputs[i]) {
          for (var j = 0; j < outputs[i].length; j++) {
            for (var k = 0; k < outputs[i][j].length; k++) {
              outputs[i][j][k] = 1.0;
            }
          }
        }
      }
    }
    if (inputs) {
      for (var i = 0; i < inputs.length; i++) {
        if (inputs[i]) {
          for (var j = 0; j < inputs[i].length; j++) {
            for (var k = 0; k < inputs[i][j].length; k++) {
              outputs[0][j][k] = outputs[0][j][k] * inputs[i][j][k];
            }
          }
        }
      }
    }
    return true;
  }
}

registerProcessor("ps-aud-mul", PsAudMulProcessor);
