"use strict";

var gulp = require("gulp");
var purescript = require("gulp-purescript");

var sources = [
  "client/src/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "client/src/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("purs",function() {
  return purescript.compile({
      src: sources,
      ffi: foreigns,
      output: "client/output"
    });
});

gulp.task("bundle", ["purs"], function() {
  return purescript.bundle({
      src: "client/output/**/*.js",
      output: "assets/bundle.js",
      module: "Main",
      main: "Main"
    });
});

gulp.task("default", ["bundle"]);