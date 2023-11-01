use crate::errors::SyntaxError;
use crate::{svg_path_cst, SVGPathCSTNode, SVGPathCommand, SVGPathSegment, Sign, WSP};

macro_rules! assert_svg_path_cst {
    ($path:expr, $expected:expr) => {
        assert_eq!(svg_path_cst($path), Ok($expected));
    };
}

macro_rules! assert_svg_path_cst_fmt {
    ($path:expr, $expected:expr) => {
        let cst = svg_path_cst($path);
        assert!(cst.is_ok());
        assert_eq!(format!("{:?}", cst.unwrap()), $expected);
    };
}

macro_rules! assert_svg_path_cst_err {
    ($path:expr, $expected:expr) => {
        assert_eq!(svg_path_cst($path), Err($expected));
    };
}

#[test]
fn test_empty() {
    assert_svg_path_cst!("", vec![]);
}

#[test]
fn test_none() {
    assert_svg_path_cst!("none", vec![SVGPathCSTNode::None]);
}

#[test]
fn test_none_fmt() {
    assert_svg_path_cst_fmt!("none", "[None]");
}

#[test]
fn test_whitespaces() {
    assert_svg_path_cst!(
        " \t\n\r \x0C",
        vec![
            SVGPathCSTNode::Whitespace(&WSP::Space),
            SVGPathCSTNode::Whitespace(&WSP::Tab),
            SVGPathCSTNode::Whitespace(&WSP::LineFeed),
            SVGPathCSTNode::Whitespace(&WSP::CarriageReturn),
            SVGPathCSTNode::Whitespace(&WSP::Space),
            SVGPathCSTNode::Whitespace(&WSP::FormFeed),
        ]
    );
}

#[test]
fn test_whitespaces_fmt() {
    assert_svg_path_cst_fmt!(
        " \t\n\r \x0C",
        concat!(
            "[Whitespace(Space),",
            " Whitespace(Tab),",
            " Whitespace(LineFeed),",
            " Whitespace(CarriageReturn),",
            " Whitespace(Space),",
            " Whitespace(FormFeed)]",
        )
    );
}

#[test]
fn invalid_character() {
    assert_svg_path_cst_err!(
        "m0 0 !l10 10",
        SyntaxError::InvalidCharacter {
            character: '!',
            index: 5,
            expected: "number or path command".to_string(),
        }
    );

    assert_svg_path_cst_err!(
        "m0 0 l!10 10",
        SyntaxError::InvalidCharacter {
            character: '!',
            index: 6,
            expected: "number or path command".to_string(),
        }
    );

    assert_svg_path_cst_err!(
        "m0 ! 0",
        SyntaxError::InvalidCharacter {
            character: '!',
            index: 3,
            expected: "number or path command".to_string(),
        }
    );

    assert_svg_path_cst_err!(
        "m0 \t\n 0!",
        SyntaxError::InvalidCharacter {
            character: '!',
            index: 7,
            expected: "number or path command".to_string(),
        }
    );
}

#[test]
fn invalid_moveto_command_at_start() {
    assert_svg_path_cst_err!(
        "A 10 10",
        SyntaxError::ExpectedMovetoCommand {
            command: 'A',
            index: 0,
        }
    );
}

#[test]
fn invalid_moveto_command_after_whitespaces() {
    assert_svg_path_cst_err!(
        " \t\n\r \x0C A 10 10",
        SyntaxError::ExpectedMovetoCommand {
            command: 'A',
            index: 7,
        }
    );
}

#[test]
fn basic_moveto() {
    let cst = svg_path_cst("m 10-10");
    assert_eq!(
        cst,
        Ok(vec![SVGPathCSTNode::Segment(SVGPathSegment {
            command: &SVGPathCommand::MovetoLower,
            args: vec![10.0, -10.0],
            cst: vec![
                SVGPathCSTNode::Command(&SVGPathCommand::MovetoLower),
                SVGPathCSTNode::Whitespace(&WSP::Space),
                SVGPathCSTNode::Number("10".to_string()),
                SVGPathCSTNode::Sign(&Sign::Minus),
                SVGPathCSTNode::Number("10".to_string()),
            ],
            start: 0,
            end: 7,
            chained: false,
            chain_start: 0,
            chain_end: 7,
        }),]),
    );
}

#[test]
fn basic_moveto_fmt() {
    assert_svg_path_cst_fmt!(
        "m 10-10",
        concat!(
            "[Segment(SVGPathSegment {",
            " command: MovetoLower, args: [10.0, -10.0],",
            " cst: [",
            "Command(MovetoLower),",
            " Whitespace(Space),",
            " Number(\"10\"),",
            " Sign(Minus),",
            " Number(\"10\")",
            "],",
            " start: 0, end: 7, chained: false,",
            " chain_start: 0, chain_end: 7",
            " })]",
        )
    );
}

#[test]
fn moveto_whitespaces() {
    assert_svg_path_cst!(
        " M \t10\r 10 ",
        vec![
            SVGPathCSTNode::Whitespace(&WSP::Space),
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoUpper,
                args: vec![10.0, 10.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoUpper),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Whitespace(&WSP::Tab),
                    SVGPathCSTNode::Number("10".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::CarriageReturn),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("10".to_string()),
                ],
                start: 1,
                end: 10,
                chained: false,
                chain_start: 1,
                chain_end: 10,
            }),
            SVGPathCSTNode::Whitespace(&WSP::Space),
        ]
    );
}

#[test]
fn chained_moveto() {
    assert_svg_path_cst!(
        "M 10 10 20 20",
        vec![
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoUpper,
                args: vec![10.0, 10.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoUpper),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("10".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("10".to_string()),
                ],
                start: 0,
                end: 7,
                chained: false,
                chain_start: 0,
                chain_end: 13,
            }),
            SVGPathCSTNode::Whitespace(&WSP::Space),
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoUpper,
                args: vec![20.0, 20.0],
                cst: vec![
                    SVGPathCSTNode::Number("20".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("20".to_string()),
                ],
                start: 8,
                end: 13,
                chained: true,
                chain_start: 0,
                chain_end: 13,
            }),
        ]
    );
}

#[test]
fn moveto_and_moveto_drawto() {
    assert_svg_path_cst!(
        "M10 10 M5 -4.6",
        vec![
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoUpper,
                args: vec![10.0, 10.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoUpper),
                    SVGPathCSTNode::Number("10".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("10".to_string()),
                ],
                start: 0,
                end: 6,
                chained: false,
                chain_start: 0,
                chain_end: 6,
            }),
            SVGPathCSTNode::Whitespace(&WSP::Space),
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoUpper,
                args: vec![5.0, -4.6],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoUpper),
                    SVGPathCSTNode::Number("5".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Sign(&Sign::Minus),
                    SVGPathCSTNode::Number("4.6".to_string()),
                ],
                start: 7,
                end: 14,
                chained: false,
                chain_start: 7,
                chain_end: 14,
            }),
        ]
    );
}

#[test]
fn horizontal_and_vertical() {
    assert_svg_path_cst!(
        "m1 2H 10 30 V 20 h6v-3 5",
        vec![
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoLower,
                args: vec![1.0, 2.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoLower),
                    SVGPathCSTNode::Number("1".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("2".to_string()),
                ],
                start: 0,
                end: 4,
                chained: false,
                chain_start: 0,
                chain_end: 4,
            }),
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::HorizontalUpper,
                args: vec![10.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::HorizontalUpper),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("10".to_string()),
                ],
                start: 4,
                end: 8,
                chained: false,
                chain_start: 4,
                chain_end: 11,
            }),
            SVGPathCSTNode::Whitespace(&WSP::Space),
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::HorizontalUpper,
                args: vec![30.0],
                cst: vec![SVGPathCSTNode::Number("30".to_string()),],
                start: 9,
                end: 11,
                chained: true,
                chain_start: 4,
                chain_end: 11,
            }),
            SVGPathCSTNode::Whitespace(&WSP::Space),
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::VerticalUpper,
                args: vec![20.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::VerticalUpper),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("20".to_string()),
                ],
                start: 12,
                end: 16,
                chained: false,
                chain_start: 12,
                chain_end: 16,
            }),
            SVGPathCSTNode::Whitespace(&WSP::Space),
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::HorizontalLower,
                args: vec![6.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::HorizontalLower),
                    SVGPathCSTNode::Number("6".to_string()),
                ],
                start: 17,
                end: 19,
                chained: false,
                chain_start: 17,
                chain_end: 19,
            }),
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::VerticalLower,
                args: vec![-3.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::VerticalLower),
                    SVGPathCSTNode::Sign(&Sign::Minus),
                    SVGPathCSTNode::Number("3".to_string()),
                ],
                start: 19,
                end: 22,
                chained: false,
                chain_start: 19,
                chain_end: 24,
            }),
            SVGPathCSTNode::Whitespace(&WSP::Space),
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::VerticalLower,
                args: vec![5.0],
                cst: vec![SVGPathCSTNode::Number("5".to_string()),],
                start: 23,
                end: 24,
                chained: true,
                chain_start: 19,
                chain_end: 24,
            }),
        ]
    );
}

#[test]
fn moveto_curveto() {
    assert_svg_path_cst!(
        "m0 0c100,100 250,100 250,200 200,200 500,200 500,400",
        vec![
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
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::CurvetoLower,
                args: vec![100.0, 100.0, 250.0, 100.0, 250.0, 200.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::CurvetoLower),
                    SVGPathCSTNode::Number("100".to_string()),
                    SVGPathCSTNode::Comma,
                    SVGPathCSTNode::Number("100".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("250".to_string()),
                    SVGPathCSTNode::Comma,
                    SVGPathCSTNode::Number("100".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("250".to_string()),
                    SVGPathCSTNode::Comma,
                    SVGPathCSTNode::Number("200".to_string()),
                ],
                start: 4,
                end: 28,
                chained: false,
                chain_start: 4,
                chain_end: 52,
            }),
            SVGPathCSTNode::Whitespace(&WSP::Space),
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::CurvetoLower,
                args: vec![200.0, 200.0, 500.0, 200.0, 500.0, 400.0],
                cst: vec![
                    SVGPathCSTNode::Number("200".to_string()),
                    SVGPathCSTNode::Comma,
                    SVGPathCSTNode::Number("200".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("500".to_string()),
                    SVGPathCSTNode::Comma,
                    SVGPathCSTNode::Number("200".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("500".to_string()),
                    SVGPathCSTNode::Comma,
                    SVGPathCSTNode::Number("400".to_string()),
                ],
                start: 29,
                end: 52,
                chained: true,
                chain_start: 4,
                chain_end: 52,
            }),
        ]
    );
}

#[test]
fn moveto_smooth_curveto() {
    assert_svg_path_cst!(
        "m0 0s100,100 250,200 150 150 300 300",
        vec![
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
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::SmoothCurvetoLower,
                args: vec![100.0, 100.0, 250.0, 200.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::SmoothCurvetoLower),
                    SVGPathCSTNode::Number("100".to_string()),
                    SVGPathCSTNode::Comma,
                    SVGPathCSTNode::Number("100".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("250".to_string()),
                    SVGPathCSTNode::Comma,
                    SVGPathCSTNode::Number("200".to_string()),
                ],
                start: 4,
                end: 20,
                chained: false,
                chain_start: 4,
                chain_end: 36,
            }),
            SVGPathCSTNode::Whitespace(&WSP::Space),
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::SmoothCurvetoLower,
                args: vec![150.0, 150.0, 300.0, 300.0],
                cst: vec![
                    SVGPathCSTNode::Number("150".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("150".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("300".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("300".to_string()),
                ],
                start: 21,
                end: 36,
                chained: true,
                chain_start: 4,
                chain_end: 36,
            }),
        ]
    );
}

#[test]
fn moveto_arc() {
    assert_svg_path_cst!(
        "m0 0a100,100 0 0 1 250,200 150 150 0 0 0 300 300",
        vec![
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
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::ArcLower,
                args: vec![100.0, 100.0, 0.0, 0.0, 1.0, 250.0, 200.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::ArcLower),
                    SVGPathCSTNode::Number("100".to_string()),
                    SVGPathCSTNode::Comma,
                    SVGPathCSTNode::Number("100".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("0".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("0".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("1".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("250".to_string()),
                    SVGPathCSTNode::Comma,
                    SVGPathCSTNode::Number("200".to_string()),
                ],
                start: 4,
                end: 26,
                chained: false,
                chain_start: 4,
                chain_end: 48,
            }),
            SVGPathCSTNode::Whitespace(&WSP::Space),
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::ArcLower,
                args: vec![150.0, 150.0, 0.0, 0.0, 0.0, 300.0, 300.0],
                cst: vec![
                    SVGPathCSTNode::Number("150".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("150".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("0".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("0".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("0".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("300".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("300".to_string()),
                ],
                start: 27,
                end: 48,
                chained: true,
                chain_start: 4,
                chain_end: 48,
            }),
        ]
    );
}

#[test]
fn invalid_arc_flag() {
    assert_svg_path_cst_err!(
        "m0 0a100,100 0 2 1 250,200",
        SyntaxError::InvalidArcFlag {
            index: 16,
            value: 2.0,
            command: 'a',
        }
    );
}

#[test]
fn moveto_quadratic() {
    assert_svg_path_cst!(
        "m0 0q100,100 250,200 150 150 300 300",
        vec![
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
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::QuadraticLower,
                args: vec![100.0, 100.0, 250.0, 200.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::QuadraticLower),
                    SVGPathCSTNode::Number("100".to_string()),
                    SVGPathCSTNode::Comma,
                    SVGPathCSTNode::Number("100".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("250".to_string()),
                    SVGPathCSTNode::Comma,
                    SVGPathCSTNode::Number("200".to_string()),
                ],
                start: 4,
                end: 20,
                chained: false,
                chain_start: 4,
                chain_end: 36,
            }),
            SVGPathCSTNode::Whitespace(&WSP::Space),
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::QuadraticLower,
                args: vec![150.0, 150.0, 300.0, 300.0],
                cst: vec![
                    SVGPathCSTNode::Number("150".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("150".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("300".to_string()),
                    SVGPathCSTNode::Whitespace(&WSP::Space),
                    SVGPathCSTNode::Number("300".to_string()),
                ],
                start: 21,
                end: 36,
                chained: true,
                chain_start: 4,
                chain_end: 36,
            }),
        ]
    );
}

#[test]
fn moveto_smooth_quadratic() {
    assert_svg_path_cst!(
        "m0 0t100,100 250,200",
        vec![
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
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::SmoothQuadraticLower,
                args: vec![100.0, 100.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::SmoothQuadraticLower),
                    SVGPathCSTNode::Number("100".to_string()),
                    SVGPathCSTNode::Comma,
                    SVGPathCSTNode::Number("100".to_string()),
                ],
                start: 4,
                end: 12,
                chained: false,
                chain_start: 4,
                chain_end: 20,
            }),
            SVGPathCSTNode::Whitespace(&WSP::Space),
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::SmoothQuadraticLower,
                args: vec![250.0, 200.0],
                cst: vec![
                    SVGPathCSTNode::Number("250".to_string()),
                    SVGPathCSTNode::Comma,
                    SVGPathCSTNode::Number("200".to_string()),
                ],
                start: 13,
                end: 20,
                chained: true,
                chain_start: 4,
                chain_end: 20,
            }),
        ]
    );
}
