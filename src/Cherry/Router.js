'use strict';

exports.dispatchEvent = function(evt) {
  return function(window_) {
    return function() {
      window_.dispatchEvent(new Event(evt));
    }
  }
}
