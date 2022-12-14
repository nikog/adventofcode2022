// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Solution = require("../Solution.bs.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");

function part01(input) {
  return input.split("\n").map(function (x) {
                return x.split(",").map(function (y) {
                            return y.split("-").map(function (str) {
                                        return Belt_Option.getExn(Belt_Int.fromString(str));
                                      });
                          });
              }).reduce((function (acc, i) {
                if (i.length !== 2) {
                  return acc;
                }
                var match = i[0];
                if (match.length !== 2) {
                  return acc;
                }
                var a1 = match[0];
                var a2 = match[1];
                var match$1 = i[1];
                if (match$1.length !== 2) {
                  return acc;
                }
                var b1 = match$1[0];
                var b2 = match$1[1];
                if (a1 >= b1 && a2 <= b2 || b1 >= a1 && b2 <= a2) {
                  return acc + 1 | 0;
                } else {
                  return acc;
                }
              }), 0);
}

function part02(input) {
  return input.split("\n").map(function (x) {
                return x.split(",").map(function (y) {
                            return y.split("-").map(function (str) {
                                        return Belt_Option.getExn(Belt_Int.fromString(str));
                                      });
                          });
              }).reduce((function (acc, i) {
                if (i.length !== 2) {
                  return acc;
                }
                var match = i[0];
                if (match.length !== 2) {
                  return acc;
                }
                var a1 = match[0];
                var a2 = match[1];
                var match$1 = i[1];
                if (match$1.length !== 2) {
                  return acc;
                }
                var b1 = match$1[0];
                var b2 = match$1[1];
                if (a1 >= b1 && a1 <= b2 || b1 >= a1 && b1 <= a2 || a2 <= b2 && a2 >= b2 || b2 <= a2 && b2 >= a2) {
                  return acc + 1 | 0;
                } else {
                  return acc;
                }
              }), 0);
}

Solution.make(part01, "day04/input");

Solution.make(part02, "day04/input");

exports.part01 = part01;
exports.part02 = part02;
/*  Not a pure module */
