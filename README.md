# svg-path-cst

[![Crates.io](https://img.shields.io/crates/v/svg-path-cst?logo=rust)](https://crates.io/crates/svg-path-cst)
[![docs.rs](https://img.shields.io/docsrs/svg-path-cst?logo=docs.rs)](https://docs.rs/svg-path-cst)
![Tests](https://img.shields.io/github/actions/workflow/status/mondeja/svg-path-cst/ci.yml?label=tests&logo=github)
[![License](https://img.shields.io/crates/l/svg-path-cst?logo=mit)](https://github.com/mondeja/svg-path-cst/blob/master/LICENSE.md)

Concrete Syntax Tree parser for SVG v1.1 paths.

## Install

```sh
cargo add svg-path-cst
```

## Usage

```rust
use svg_path_cst::{
    svg_path_cst,
    SVGPathCSTNode,
    SVGPathSegment,
    SVGPathCommand,
    WSP,
    Sign,
    SyntaxError,
};

let cst: Result<Vec<SVGPathCSTNode>, SyntaxError> = svg_path_cst(b"m0 0 L1,-1");

let expected_cst: Vec<SVGPathCSTNode> = vec![
    SVGPathCSTNode::Segment(SVGPathSegment{
        command: &SVGPathCommand::MovetoLower,
        args: vec![0.0, 0.0],
        start: 0,
        end: 4,
        chained: false,
        chain_start: 0,
        chain_end: 4,
        cst: vec![
            SVGPathCSTNode::Command(&SVGPathCommand::MovetoLower),
            SVGPathCSTNode::Number{
                raw_number: "0".to_string(),
                value: 0.0,
                start: 1,
                end: 2,
            },
            SVGPathCSTNode::Whitespace{
                wsp: &WSP::Space,
                start: 2,
                end: 3,
            },
            SVGPathCSTNode::Number{
                raw_number: "0".to_string(),
                value: 0.0,
                start: 3,
                end: 4,
            },
        ],
    }),
    SVGPathCSTNode::Whitespace{
        wsp: &WSP::Space,
        start: 4,
        end: 5,
    },
    SVGPathCSTNode::Segment(SVGPathSegment{
        command: &SVGPathCommand::LinetoUpper,
        args: vec![1.0, -1.0],
        start: 5,
        end: 10,
        chained: false,
        chain_start: 5,
        chain_end: 10,
        cst: vec![
            SVGPathCSTNode::Command(&SVGPathCommand::LinetoUpper),
            SVGPathCSTNode::Number{
                raw_number: "1".to_string(),
                value: 1.0,
                start: 6,
                end: 7,
            },
            SVGPathCSTNode::Comma{ start: 7 },
            SVGPathCSTNode::Sign{ sign: &Sign::Minus, start: 8 },
            SVGPathCSTNode::Number{
                raw_number: "1".to_string(),
                value: 1.0,
                start: 9,
                end: 10,
            },
        ],
    }),
];

assert_eq!(cst, Ok(expected_cst));
```

## Compatibility

This crate is compatible with SVG v1.1 paths, as defined in the [W3C SVG 1.1]
and [`no_std` environments].

## Features

### **`tracing`**

Add [`tracing`] support. See the [`tracing` example] to learn how to use it.

### **`strict`**

Enable strict mode. The differences between strict and non-strict modes are:

- With empty input (`b""`), non-strict mode returns an empty vector, while
  strict mode returns a `SyntaxError::UnexpectedEnding` error.
- With the input `b"none"`, non-strict mode returns a `SVGPathCSTNode::None`
  node, while strict mode returns a `SyntaxError::ExpectedMovetoCommand` error.
  The `"none"` input is defined
  [by the SVG specification](https://www.w3.org/TR/SVG/paths.html#TheDProperty).
- With input containing only whitespaces, non-strict mode returns an empty vector,
  while strict mode returns a `SyntaxError::ExpectedMovetoCommand` error.

[W3C SVG 1.1]: https://www.w3.org/TR/SVG11/paths.html#PathData
[`no_std` environments]: https://docs.rust-embedded.org/book/intro/no-std.html
[`tracing`]: https://docs.rs/tracing/latest/tracing
[`tracing` example]: https://github.com/mondeja/svg-path-cst/tree/master/examples/tracing
