#!/bin/sh

: '
  Integration tests for svg-path-cst binary.

  Environment variables:

  - DEBUG: If not empty, all executed commands will be shown in the output.
'

testSimpleicons() {
  output="$(./target/debug/svg-path-cst "$(cat fuzz/corpus/simpleicons.txt)")"
  assertContains "$output" "[Segment(SVGPathSegment { args: [12.0, 0.0]"
  assertContains "$output" "chained: false })]"
}

testElsevier() {
  output="$(./target/debug/svg-path-cst "$(cat fuzz/corpus/elsevier.txt)")"
  assertContains "$output" "[Segment(SVGPathSegment { "
  assertContains "$output" "chained: false })]"
}

testFailsOnInvalidInput() {
  output="$(./target/debug/svg-path-cst "invalid" 2>&1)"
  exitCode=$?
  assertEquals "$exitCode" 1
  assertContains "$output" "Error: Invalid SVG path command 'i' at index 0, expected 'M' or 'm'"
}

testHelp() {
  output="$(./target/debug/svg-path-cst --help 2>&1)"
  exitCode=$?
  assertEquals "$exitCode" 1
  assertContains "$output" "Usage: svg-path-cst \"<path>\""
}

testNoArgs() {
  output="$(./target/debug/svg-path-cst 2>&1)"
  exitCode=$?
  assertEquals "$exitCode" 1
  assertContains "$output" "Usage: svg-path-cst \"<path>\""
}

testAdditionalArgsAreIgnored() {
  output="$(./target/debug/svg-path-cst foo bar baz "$(cat fuzz/corpus/simpleicons.txt)" 2>&1)"
  assertContains "$output" "[Segment(SVGPathSegment { args: [12.0, 0.0]"
  assertContains "$output" "chained: false })]"
}

prepare() {
  cargo build

  set -e
  if [ -n "$DEBUG" ]; then
    set -x
  fi

  if [ ! -f "shunit2" ]; then
    curl -sSL https://raw.githubusercontent.com/kward/shunit2/master/shunit2 \
      -o shunit2
  fi
}

prepare && . ./shunit2
