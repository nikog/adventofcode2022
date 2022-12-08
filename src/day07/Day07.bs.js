// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Solution = require("../Solution.bs.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Belt_MapString = require("rescript/lib/js/belt_MapString.js");
var Caml_splice_call = require("rescript/lib/js/caml_splice_call.js");

function part01(input) {
  var path = {
    contents: []
  };
  var dirs = {
    contents: undefined
  };
  input.split("\n").map(function (line) {
        var parts = line.split(" ");
        var len = parts.length;
        if (len !== 2) {
          if (len !== 3) {
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "Day07.res",
                    33,
                    13
                  ],
                  Error: new Error()
                };
          }
          var cmd = parts[1];
          var args = parts[2];
          if (cmd === "cd" && args === "/") {
            path.contents = [""];
            return ;
          }
          if (cmd === "cd" && args === "..") {
            path.contents.pop();
            return ;
          }
          if (cmd === "cd") {
            path.contents.push(args);
            return ;
          }
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "Day07.res",
                  33,
                  13
                ],
                Error: new Error()
              };
        }
        var dir = parts[0];
        var cmd$1 = parts[1];
        if (cmd$1 === "ls") {
          return ;
        }
        if (dir === "dir") {
          return ;
        }
        var stack = path.contents.slice(0);
        path.contents.forEach(function (param) {
              var key = stack.join("/");
              var dirVal = Belt_Option.getWithDefault(Belt_MapString.get(dirs.contents, key), 0);
              dirs.contents = Belt_MapString.set(dirs.contents, key, Belt_Option.getExn(Belt_Int.fromString(dir)) + dirVal | 0);
              stack.pop();
            });
      });
  return Belt_MapString.toArray(dirs.contents).reduce((function (acc, i) {
                var val = i[1];
                return acc + (
                        val <= 100000 ? val : 0
                      ) | 0;
              }), 0);
}

function part02(input) {
  var path = {
    contents: []
  };
  var dirs = {
    contents: undefined
  };
  input.split("\n").map(function (line) {
        var parts = line.split(" ");
        var len = parts.length;
        if (len !== 2) {
          if (len !== 3) {
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "Day07.res",
                    75,
                    13
                  ],
                  Error: new Error()
                };
          }
          var cmd = parts[1];
          var args = parts[2];
          if (cmd === "cd" && args === "/") {
            path.contents = [""];
            return ;
          }
          if (cmd === "cd" && args === "..") {
            path.contents.pop();
            return ;
          }
          if (cmd === "cd") {
            path.contents.push(args);
            return ;
          }
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "Day07.res",
                  75,
                  13
                ],
                Error: new Error()
              };
        }
        var dir = parts[0];
        var cmd$1 = parts[1];
        if (cmd$1 === "ls") {
          return ;
        }
        if (dir === "dir") {
          return ;
        }
        var stack = path.contents.slice(0);
        path.contents.forEach(function (param) {
              var key = stack.join("/");
              var dirVal = Belt_Option.getWithDefault(Belt_MapString.get(dirs.contents, key), 0);
              dirs.contents = Belt_MapString.set(dirs.contents, key, Belt_Option.getExn(Belt_Int.fromString(dir)) + dirVal | 0);
              stack.pop();
            });
      });
  var root = Belt_Option.getExn(Belt_MapString.get(dirs.contents, ""));
  var freeSpace = 70000000 - root | 0;
  var neededSpace = 30000000 - freeSpace | 0;
  return Caml_splice_call.spliceApply(Math.min, [Belt_MapString.toArray(dirs.contents).map(function (i) {
                      return i[1];
                    }).filter(function (i) {
                    return i >= neededSpace;
                  })]);
}

Solution.make(part01, "day07/input");

Solution.make(part02, "day07/input");

exports.part01 = part01;
exports.part02 = part02;
/*  Not a pure module */
