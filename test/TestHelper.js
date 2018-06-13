'use strict';

exports.enableJSDOM = function() {
  require('jsdom-global')();
  return {};
}

exports.clearBody = function() {
  document.body.innerHTML = '';
  return {};
}
