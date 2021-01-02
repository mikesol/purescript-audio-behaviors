exports.pixelTransformImpl = function (ctx) {
  return function (za) {
    return function (zb) {
      return function (c) {
        return function (d) {
          return function (idt) {
            return function (a_) {
              return function (b_) {
                return function () {
                  var imageData = ctx.getImageData(za, zb, c, d);
                  var w = imageData.width;
                  var h = imageData.height;
                  for (var i = 0; i < imageData.data.length; i += 4) {
                    var r = imageData.data[i];
                    var g = imageData.data[i + 1];
                    var b = imageData.data[i + 2];
                    var a = imageData.data[i + 3];
                    var iOver4 = i / 4;
                    var x = iOver4 % imageData.width;
                    var y = Math.floor(iOver4 / imageData.width);
                    var odt = idt({
                      r: r,
                      g: g,
                      b: b,
                      a: a,
                      x: x,
                      y: y,
                      w: w,
                      h: h,
                    });
                    imageData.data[i] = odt.r;
                    imageData.data[i + 1] = odt.g;
                    imageData.data[i + 2] = odt.b;
                    imageData.data[i + 3] = odt.a;
                  }
                  ctx.putImageData(imageData, a_, b_);
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.pixelTransformFullImpl = function (ctx) {
  return function (za) {
    return function (zb) {
      return function (c) {
        return function (d) {
          return function (idt) {
            return function (a_) {
              return function (b_) {
                return function (c_) {
                  return function (d_) {
                    return function (e_) {
                      return function (f_) {
                        return function () {
                          var imageData = ctx.getImageData(za, zb, c, d);
                          var w = imageData.width;
                          var h = imageData.height;
                          for (var i = 0; i < imageData.data.length; i += 4) {
                            var r = imageData.data[i];
                            var g = imageData.data[i + 1];
                            var b = imageData.data[i + 2];
                            var a = imageData.data[i + 3];
                            var iOver4 = i / 4;
                            var x = iOver4 % width;
                            var y = Math.floor(iOver4 / width);
                            var odt = idt({
                              r: r,
                              g: g,
                              b: b,
                              a: a,
                              x: x,
                              y: y,
                              w: w,
                              h: h,
                            });
                            imageData.data[i] = odt.r;
                            imageData.data[i + 1] = odt.g;
                            imageData.data[i + 2] = odt.b;
                            imageData.data[i + 3] = odt.a;
                          }
                          ctx.putImageData(imageData, a_, b_, c_, d_, e_, f_);
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
    };
  };
};
