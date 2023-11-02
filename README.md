# svg-path-cst

[![Crates.io](https://img.shields.io/crates/v/svg-path-cst)](https://crates.io/crates/svg-path-cst)
![Tests](https://img.shields.io/github/actions/workflow/status/mondeja/svg-path-cst/ci.yml?label=tests)
[![License](https://img.shields.io/crates/l/svg-path-cst)](https://github.com/mondeja/svg-path-cst/blob/master/LICENSE.md)

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
};

let cst = svg_path_cst("m0 0 L1,-1");
assert_eq!(
    cst,
    Ok(vec![
        SVGPathCSTNode::Segment(SVGPathSegment{
            command: &SVGPathCommand::MovetoLower,
            args: vec![0.0, 0.0],
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
            start: 0,
            end: 4,
            chained: false,
            chain_start: 0,
            chain_end: 4,
        }),
        SVGPathCSTNode::Whitespace{
            wsp: &WSP::Space,
            start: 4,
            end: 5,
        },
        SVGPathCSTNode::Segment(SVGPathSegment{
            command: &SVGPathCommand::LinetoUpper,
            args: vec![1.0, -1.0],
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
            start: 5,
            end: 10,
            chained: false,
            chain_start: 5,
            chain_end: 10,
        }),
    ])
);
```
