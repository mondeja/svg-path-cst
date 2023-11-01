# svg-path-cst

[![Crates.io](https://img.shields.io/crates/v/svg-path-cst)](https://crates.io/crates/svg-path-cst)
![Tests](https://img.shields.io/github/actions/workflow/status/mondeja/svg-path-cst/ci.yml?label=tests)
[![License](https://img.shields.io/crates/l/svg-path-cst)](https://github.com/mondeja/svg-path-cst/blob/master/LICENSE.md)

Concrete Syntax Tree SVG paths parser.

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
};

let cst = svg_path_cst("m0 0 L1,1");
assert_eq!(
    cst,
    Ok(vec![
        SVGPathCSTNode::Segment(SVGPathSegment {
            command: &SVGPathCommand::MovetoLower,
            args: vec![0.0, 0.0],
            cst: vec![
                SVGPathCSTNode::Command(&SVGPathCommand::MovetoLower),
                SVGPathCSTNode::Number("0".to_string()),
                SVGPathCSTNode::Whitespace(&WSP::Space),
                SVGPathCSTNode::Number("0".to_string()),
            ],
            start: 0,
            end: 4,
            chained: false,
            chain_start: 0,
            chain_end: 4,
        }),
        SVGPathCSTNode::Whitespace(&WSP::Space),
        SVGPathCSTNode::Segment(SVGPathSegment {
            command: &SVGPathCommand::LinetoUpper,
            args: vec![1.0, 1.0],
            cst: vec![
                SVGPathCSTNode::Command(&SVGPathCommand::LinetoUpper),
                SVGPathCSTNode::Number("1".to_string()),
                SVGPathCSTNode::Comma,
                SVGPathCSTNode::Number("1".to_string()),
            ],
            start: 5,
            end: 9,
            chained: false,
            chain_start: 5,
            chain_end: 9,
        }),
    ])
);
```
