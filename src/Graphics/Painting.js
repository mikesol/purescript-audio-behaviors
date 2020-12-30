exports.newImageData = function (ctx) {
  return function (imageData) {
    return function (imageDataRep) {
      return function () {
        var newData = ctx.createImageData(imageData);
        var minLen =
          imageDataRep.pixels.length > imageData.data.length
            ? imageData.data.length
            : imageDataRep.pixels.length;
        for (var i = 0; i < minLen; i++) {
          newData.data[i] = imageDataRep.pixels[i];
        }
        return newData;
      };
    };
  };
};
exports.imageDataToRep = function (imageData) {
  return function () {
    var pixels = new Array(imageData.data.length);
    for (var i = 0; i < imageData.data.length; i++) {
      pixels[i] = imageData.data[i];
    }
    return {
      width: imageData.width,
      height: imageData.height,
      pixels: pixels,
    };
  };
};
