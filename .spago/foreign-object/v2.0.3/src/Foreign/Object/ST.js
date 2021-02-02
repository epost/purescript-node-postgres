"use strict";

exports["new"] = function () {
  return {};
};

exports.peekImpl = function (just) {
  return function (nothing) {
    return function (k) {
      return function (m) {
        return function () {
          return {}.hasOwnProperty.call(m, k) ? just(m[k]) : nothing;
        };
      };
    };
  };
};

exports.poke = function (k) {
  return function (v) {
    return function (m) {
      return function () {
        m[k] = v;
        return m;
      };
    };
  };
};

exports["delete"] = function (k) {
  return function (m) {
    return function () {
      delete m[k];
      return m;
    };
  };
};
