"use strict";

var gulp = require("gulp");
var purescript = require("gulp-purescript");
var run = require("gulp-run")

var sources = [
  "client/src/**/*.purs",
  "client/test/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "client/src/**/*.js",
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

gulp.task("test", ["purs"], function() {
  return purescript.bundle({ src: "client/output/**/*.js", main: "Test.Main"})
    .pipe(run("node"));
  
});

gulp.task("exec",["bundle"], function() {
  return run("stack exec server")
}

gulp.task("default", ["bundle"]);