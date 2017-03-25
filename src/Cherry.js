"use strict";

var state = {};

exports.select = function(f) {
  return function() {
    return f(state);
  }
}

exports.reduce = function(f) {
  return function() {
    state = f(state);
  }
}

exports.requestAnimationFrame = function(f) {
  return function(window_) {
    if (!requestAnimationFrame) {
      requestAnimationFrame = (function(){
        window_.requestAnimationFrame ||
        window_.webkitRequestAnimationFrame ||
        window_.mozRequestAnimationFrame ||
        function(callback) {
          window_.setTimeout(callback, 1000 / 60)
        }
      })();
    }
    return function() { requestAnimationFrame(f); }
  }
}

exports.dispatchEvent = function(evt) {
  return function(window_) {
    return function() {
      window_.dispatchEvent(new Event(evt));
    }
  }
}
