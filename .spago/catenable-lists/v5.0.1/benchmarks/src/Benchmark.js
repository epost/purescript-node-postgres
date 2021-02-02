'use strict';

// module Test.Benchmark

function whileUncons(predicate) {
  return function(tail){
    return function(uncons){
      return function(values){
        var tailValues = values;
        var tco = true;
        while (tco) {
          var result = uncons(tailValues);
          tco = predicate(result);
          if (tco) {
            tailValues = tail(result);
          }
        }
      };
    };
  };
}

exports.whileUncons = whileUncons;
