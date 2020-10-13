// gain-processor.js
class GainProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors() {
    return [
      {
        name: "customGain",
        defaultValue: 1,
        minValue: 0,
        maxValue: 1,
        automationRate: "a-rate",
      },
    ];
  }

  process(inputs, outputs, parameters) {
    const input = inputs[0];
    const output = outputs[0];
    for (var i = 0; i < input.length; i++) {
      for (var j = 0; j < input[i].length; j++) {
        output[i][j] =
          input[i][j] *
          (parameters["customGain"].length > 1
            ? parameters["customGain"][i]
            : parameters["customGain"][0]);
      }
    }
    return true;
  }
}

registerProcessor("gain-processor", GainProcessor);
