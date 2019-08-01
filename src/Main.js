"use strict";

exports.setStyleProp = function(propName) {
  return function(newVal) {
    return function(element) {
      return function() {
        if (element.style[propName] !== undefined) {
          element.style[propName] = newVal;
          return true;
        } else return false;
      };
    };
  };
};
