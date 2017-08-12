"use strict";

var state = {};

exports.select = function() {
  return state;
}

exports.reduce = function(f) {
  return function() {
    state = f(state);
  }
}

exports.dispatchEvent = function(evt) {
  return function(window_) {
    return function() {
      window_.dispatchEvent(new Event(evt));
    }
  }
}
