'use strict';

exports.setForeign = function(name) {
  return function(foreign) {
    return function(element) {
      return function() {
        element[name.toLowerCase()] = foreign;
      }
    }
  }
}

exports.removeForeign = function(name) {
  return function(element) {
    return function() {
      element[name.toLowerCase()] = null;
    }
  }
}
