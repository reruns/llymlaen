"use strict";

var gulp = require("gulp");
var purescript = require("gulp-purescript");

var sources = [
  "scripts/src/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "scripts/src/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("purs",function() {
  return purescript.compile({
      src: sources,
      ffi: foreigns,
      output: "scripts/output"
    });
});

gulp.task("bundle", ["purs"], function() {
  return purescript.bundle({
      src: "scripts/output/**/*.js",
      output: "bundle.js",
      module: "Main",
      main: "Main"
    });
});

gulp.task("default", ["bundle"]);