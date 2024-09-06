use crate::tests::helpers::*;
use crate::{
    svg_path_cst, SVGPathCSTNode, SVGPathCommand, SVGPathSegment, Sign, SyntaxError,
    WSP,
};

#[test]
fn empty() {
    assert_svg_path_cst(b"", Vec::new());
}

#[test]
fn none() {
    assert_svg_path_cst(b"none", vec![SVGPathCSTNode::None]);
}

#[test]
fn none_fmt() {
    assert_svg_path_cst_fmt(b"none", "[None]");
}

#[test]
fn whitespaces() {
    assert_svg_path_cst(
        b" \t\n\r \x0C",
        vec![
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::Space,
                start: 0,
                end: 1,
            },
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::Tab,
                start: 1,
                end: 2,
            },
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::LineFeed,
                start: 2,
                end: 3,
            },
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::CarriageReturn,
                start: 3,
                end: 4,
            },
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::Space,
                start: 4,
                end: 5,
            },
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::FormFeed,
                start: 5,
                end: 6,
            },
        ],
    );
}

#[test]
fn whitespaces_fmt() {
    assert_svg_path_cst_fmt(
        b" \t\n\r \x0C",
        concat!(
            "[Whitespace { wsp: Space, start: 0, end: 1 },",
            " Whitespace { wsp: Tab, start: 1, end: 2 },",
            " Whitespace { wsp: LineFeed, start: 2, end: 3 },",
            " Whitespace { wsp: CarriageReturn, start: 3, end: 4 },",
            " Whitespace { wsp: Space, start: 4, end: 5 },",
            " Whitespace { wsp: FormFeed, start: 5, end: 6 }]",
        ),
    );
}

#[test]
fn invalid_character() {
    let expected = "number or command";

    assert_svg_path_cst_err(
        b"m0 0 !l10 10",
        SyntaxError::InvalidCharacter {
            character: '!',
            index: 5,
            expected,
        },
    );

    assert_svg_path_cst_err(
        b"m0 0 l!10 10",
        SyntaxError::InvalidCharacter {
            character: '!',
            index: 6,
            expected,
        },
    );

    assert_svg_path_cst_err(
        b"m0 ! 0",
        SyntaxError::InvalidCharacter {
            character: '!',
            index: 3,
            expected,
        },
    );

    assert_svg_path_cst_err(
        b"m0 \t\n 0!",
        SyntaxError::InvalidCharacter {
            character: '!',
            index: 7,
            expected,
        },
    );
}

#[test]
fn invalid_moveto_at_start() {
    assert_svg_path_cst_err(
        b"A 10 10",
        SyntaxError::ExpectedMovetoCommand {
            character: 'A',
            index: 0,
        },
    );
}

#[test]
fn invalid_moveto_after_wsp() {
    assert_svg_path_cst_err(
        b" \t\n\r \x0C A 10 10",
        SyntaxError::ExpectedMovetoCommand {
            character: 'A',
            index: 7,
        },
    );
}

#[test]
fn invalid_end_in_moveto() {
    assert_svg_path_cst_err(
        b"m 10",
        SyntaxError::UnexpectedEnding {
            index: 3,
            expected: "comma or whitespace",
        },
    );
}

#[test]
fn invalid_moveto_args() {
    assert_svg_path_cst_err(
        b"m 1 2 3",
        SyntaxError::UnexpectedEnding {
            index: 6,
            expected: "comma or whitespace",
        },
    );
}

#[test]
fn basic_moveto() {
    let cst = svg_path_cst(b"m 10-10");
    assert_eq!(
        cst,
        Ok(vec![SVGPathCSTNode::Segment(SVGPathSegment {
            command: &SVGPathCommand::MovetoLower,
            args: vec![10.0, -10.0],
            cst: vec![
                SVGPathCSTNode::Command(&SVGPathCommand::MovetoLower),
                SVGPathCSTNode::Whitespace {
                    wsp: &WSP::Space,
                    start: 1,
                    end: 2,
                },
                SVGPathCSTNode::Number {
                    raw_number: "10".into(),
                    value: 10.0,
                    start: 2,
                    end: 4,
                },
                SVGPathCSTNode::Sign {
                    sign: &Sign::Minus,
                    start: 4,
                },
                SVGPathCSTNode::Number {
                    raw_number: "10".into(),
                    value: 10.0,
                    start: 5,
                    end: 7,
                },
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
    assert_svg_path_cst_fmt(
        b"m 10-10",
        concat!(
            "[Segment(SVGPathSegment {",
            " command: MovetoLower, args: [10.0, -10.0],",
            " cst: [",
            "Command(MovetoLower),",
            " Whitespace { wsp: Space, start: 1, end: 2 },",
            " Number { raw_number: \"10\", value: 10.0, start: 2, end: 4 },",
            " Sign { sign: Minus, start: 4 },",
            " Number { raw_number: \"10\", value: 10.0, start: 5, end: 7 }",
            "],",
            " start: 0, end: 7, chained: false,",
            " chain_start: 0, chain_end: 7",
            " })]",
        ),
    );
}

#[test]
fn moveto_whitespaces() {
    assert_svg_path_cst(
        b" M \t10\r 10 ",
        vec![
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::Space,
                start: 0,
                end: 1,
            },
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoUpper,
                args: vec![10.0, 10.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoUpper),
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 2,
                        end: 3,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Tab,
                        start: 3,
                        end: 4,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "10".into(),
                        value: 10.0,
                        start: 4,
                        end: 6,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::CarriageReturn,
                        start: 6,
                        end: 7,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 7,
                        end: 8,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "10".into(),
                        value: 10.0,
                        start: 8,
                        end: 10,
                    },
                ],
                start: 1,
                end: 10,
                chained: false,
                chain_start: 1,
                chain_end: 10,
            }),
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::Space,
                start: 10,
                end: 11,
            },
        ],
    );
}

#[test]
fn chained_moveto() {
    assert_svg_path_cst(
        b"M 10 10 20 20",
        vec![
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoUpper,
                args: vec![10.0, 10.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoUpper),
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 1,
                        end: 2,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "10".into(),
                        value: 10.0,
                        start: 2,
                        end: 4,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 4,
                        end: 5,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "10".into(),
                        value: 10.0,
                        start: 5,
                        end: 7,
                    },
                ],
                start: 0,
                end: 7,
                chained: false,
                chain_start: 0,
                chain_end: 13,
            }),
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::Space,
                start: 7,
                end: 8,
            },
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoUpper,
                args: vec![20.0, 20.0],
                cst: vec![
                    SVGPathCSTNode::Number {
                        raw_number: "20".into(),
                        value: 20.0,
                        start: 8,
                        end: 10,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 10,
                        end: 11,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "20".into(),
                        value: 20.0,
                        start: 11,
                        end: 13,
                    },
                ],
                start: 8,
                end: 13,
                chained: true,
                chain_start: 0,
                chain_end: 13,
            }),
        ],
    );
}

#[test]
fn moveto_and_moveto_drawto() {
    assert_svg_path_cst(
        b"M10 10 M5 -4.6",
        vec![
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoUpper,
                args: vec![10.0, 10.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoUpper),
                    SVGPathCSTNode::Number {
                        raw_number: "10".into(),
                        value: 10.0,
                        start: 1,
                        end: 3,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 3,
                        end: 4,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "10".into(),
                        value: 10.0,
                        start: 4,
                        end: 6,
                    },
                ],
                start: 0,
                end: 6,
                chained: false,
                chain_start: 0,
                chain_end: 6,
            }),
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::Space,
                start: 6,
                end: 7,
            },
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoUpper,
                args: vec![5.0, -4.6],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoUpper),
                    SVGPathCSTNode::Number {
                        raw_number: "5".into(),
                        value: 5.0,
                        start: 8,
                        end: 9,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 9,
                        end: 10,
                    },
                    SVGPathCSTNode::Sign {
                        sign: &Sign::Minus,
                        start: 10,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "4.6".into(),
                        value: 4.6,
                        start: 11,
                        end: 14,
                    },
                ],
                start: 7,
                end: 14,
                chained: false,
                chain_start: 7,
                chain_end: 14,
            }),
        ],
    );
}

#[test]
fn horizontal_and_vertical() {
    assert_svg_path_cst(
        b"m1 2h6v-3 5",
        vec![
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoLower,
                args: vec![1.0, 2.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoLower),
                    SVGPathCSTNode::Number {
                        raw_number: "1".into(),
                        value: 1.0,
                        start: 1,
                        end: 2,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 2,
                        end: 3,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "2".into(),
                        value: 2.0,
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
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::HorizontalLower,
                args: vec![6.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::HorizontalLower),
                    SVGPathCSTNode::Number {
                        raw_number: "6".into(),
                        value: 6.0,
                        start: 5,
                        end: 6,
                    },
                ],
                start: 4,
                end: 6,
                chained: false,
                chain_start: 4,
                chain_end: 6,
            }),
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::VerticalLower,
                args: vec![-3.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::VerticalLower),
                    SVGPathCSTNode::Sign {
                        sign: &Sign::Minus,
                        start: 7,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "3".into(),
                        value: 3.0,
                        start: 8,
                        end: 9,
                    },
                ],
                start: 6,
                end: 9,
                chained: false,
                chain_start: 6,
                chain_end: 11,
            }),
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::Space,
                start: 9,
                end: 10,
            },
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::VerticalLower,
                args: vec![5.0],
                cst: vec![SVGPathCSTNode::Number {
                    raw_number: "5".into(),
                    value: 5.0,
                    start: 10,
                    end: 11,
                }],
                start: 10,
                end: 11,
                chained: true,
                chain_start: 6,
                chain_end: 11,
            }),
        ],
    );
}

#[test]
fn moveto_curveto() {
    assert_svg_path_cst(
        b"m0 0c100,100 250,100 250,200 200,200 500,200 500,400",
        vec![
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoLower,
                args: vec![0.0, 0.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoLower),
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
                        value: 0.0,
                        start: 1,
                        end: 2,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 2,
                        end: 3,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
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
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::CurvetoLower,
                args: vec![100.0, 100.0, 250.0, 100.0, 250.0, 200.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::CurvetoLower),
                    SVGPathCSTNode::Number {
                        raw_number: "100".into(),
                        value: 100.0,
                        start: 5,
                        end: 8,
                    },
                    SVGPathCSTNode::Comma { start: 8 },
                    SVGPathCSTNode::Number {
                        raw_number: "100".into(),
                        value: 100.0,
                        start: 9,
                        end: 12,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 12,
                        end: 13,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "250".into(),
                        value: 250.0,
                        start: 13,
                        end: 16,
                    },
                    SVGPathCSTNode::Comma { start: 16 },
                    SVGPathCSTNode::Number {
                        raw_number: "100".into(),
                        value: 100.0,
                        start: 17,
                        end: 20,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 20,
                        end: 21,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "250".into(),
                        value: 250.0,
                        start: 21,
                        end: 24,
                    },
                    SVGPathCSTNode::Comma { start: 24 },
                    SVGPathCSTNode::Number {
                        raw_number: "200".into(),
                        value: 200.0,
                        start: 25,
                        end: 28,
                    },
                ],
                start: 4,
                end: 28,
                chained: false,
                chain_start: 4,
                chain_end: 52,
            }),
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::Space,
                start: 28,
                end: 29,
            },
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::CurvetoLower,
                args: vec![200.0, 200.0, 500.0, 200.0, 500.0, 400.0],
                cst: vec![
                    SVGPathCSTNode::Number {
                        raw_number: "200".into(),
                        value: 200.0,
                        start: 29,
                        end: 32,
                    },
                    SVGPathCSTNode::Comma { start: 32 },
                    SVGPathCSTNode::Number {
                        raw_number: "200".into(),
                        value: 200.0,
                        start: 33,
                        end: 36,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 36,
                        end: 37,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "500".into(),
                        value: 500.0,
                        start: 37,
                        end: 40,
                    },
                    SVGPathCSTNode::Comma { start: 40 },
                    SVGPathCSTNode::Number {
                        raw_number: "200".into(),
                        value: 200.0,
                        start: 41,
                        end: 44,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 44,
                        end: 45,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "500".into(),
                        value: 500.0,
                        start: 45,
                        end: 48,
                    },
                    SVGPathCSTNode::Comma { start: 48 },
                    SVGPathCSTNode::Number {
                        raw_number: "400".into(),
                        value: 400.0,
                        start: 49,
                        end: 52,
                    },
                ],
                start: 29,
                end: 52,
                chained: true,
                chain_start: 4,
                chain_end: 52,
            }),
        ],
    );
}

#[test]
fn moveto_smooth_curveto() {
    assert_svg_path_cst(
        b"m0 0s100,100 250,200 150 150 300 300",
        vec![
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoLower,
                args: vec![0.0, 0.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoLower),
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
                        value: 0.0,
                        start: 1,
                        end: 2,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 2,
                        end: 3,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
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
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::SmoothCurvetoLower,
                args: vec![100.0, 100.0, 250.0, 200.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::SmoothCurvetoLower),
                    SVGPathCSTNode::Number {
                        raw_number: "100".into(),
                        value: 100.0,
                        start: 5,
                        end: 8,
                    },
                    SVGPathCSTNode::Comma { start: 8 },
                    SVGPathCSTNode::Number {
                        raw_number: "100".into(),
                        value: 100.0,
                        start: 9,
                        end: 12,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 12,
                        end: 13,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "250".into(),
                        value: 250.0,
                        start: 13,
                        end: 16,
                    },
                    SVGPathCSTNode::Comma { start: 16 },
                    SVGPathCSTNode::Number {
                        raw_number: "200".into(),
                        value: 200.0,
                        start: 17,
                        end: 20,
                    },
                ],
                start: 4,
                end: 20,
                chained: false,
                chain_start: 4,
                chain_end: 36,
            }),
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::Space,
                start: 20,
                end: 21,
            },
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::SmoothCurvetoLower,
                args: vec![150.0, 150.0, 300.0, 300.0],
                cst: vec![
                    SVGPathCSTNode::Number {
                        raw_number: "150".into(),
                        value: 150.0,
                        start: 21,
                        end: 24,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 24,
                        end: 25,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "150".into(),
                        value: 150.0,
                        start: 25,
                        end: 28,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 28,
                        end: 29,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "300".into(),
                        value: 300.0,
                        start: 29,
                        end: 32,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 32,
                        end: 33,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "300".into(),
                        value: 300.0,
                        start: 33,
                        end: 36,
                    },
                ],
                start: 21,
                end: 36,
                chained: true,
                chain_start: 4,
                chain_end: 36,
            }),
        ],
    );
}

#[test]
fn moveto_arc() {
    assert_svg_path_cst(
        b"m0 0a100,100 0 0 1 250,200 150 150 0 0 0 300 300",
        vec![
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoLower,
                args: vec![0.0, 0.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoLower),
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
                        value: 0.0,
                        start: 1,
                        end: 2,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 2,
                        end: 3,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
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
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::ArcLower,
                args: vec![100.0, 100.0, 0.0, 0.0, 1.0, 250.0, 200.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::ArcLower),
                    SVGPathCSTNode::Number {
                        raw_number: "100".into(),
                        value: 100.0,
                        start: 5,
                        end: 8,
                    },
                    SVGPathCSTNode::Comma { start: 8 },
                    SVGPathCSTNode::Number {
                        raw_number: "100".into(),
                        value: 100.0,
                        start: 9,
                        end: 12,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 12,
                        end: 13,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
                        value: 0.0,
                        start: 13,
                        end: 14,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 14,
                        end: 15,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
                        value: 0.0,
                        start: 15,
                        end: 16,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 16,
                        end: 17,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "1".into(),
                        value: 1.0,
                        start: 17,
                        end: 18,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 18,
                        end: 19,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "250".into(),
                        value: 250.0,
                        start: 19,
                        end: 22,
                    },
                    SVGPathCSTNode::Comma { start: 22 },
                    SVGPathCSTNode::Number {
                        raw_number: "200".into(),
                        value: 200.0,
                        start: 23,
                        end: 26,
                    },
                ],
                start: 4,
                end: 26,
                chained: false,
                chain_start: 4,
                chain_end: 48,
            }),
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::Space,
                start: 26,
                end: 27,
            },
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::ArcLower,
                args: vec![150.0, 150.0, 0.0, 0.0, 0.0, 300.0, 300.0],
                cst: vec![
                    SVGPathCSTNode::Number {
                        raw_number: "150".into(),
                        value: 150.0,
                        start: 27,
                        end: 30,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 30,
                        end: 31,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "150".into(),
                        value: 150.0,
                        start: 31,
                        end: 34,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 34,
                        end: 35,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
                        value: 0.0,
                        start: 35,
                        end: 36,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 36,
                        end: 37,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
                        value: 0.0,
                        start: 37,
                        end: 38,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 38,
                        end: 39,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
                        value: 0.0,
                        start: 39,
                        end: 40,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 40,
                        end: 41,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "300".into(),
                        value: 300.0,
                        start: 41,
                        end: 44,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 44,
                        end: 45,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "300".into(),
                        value: 300.0,
                        start: 45,
                        end: 48,
                    },
                ],
                start: 27,
                end: 48,
                chained: true,
                chain_start: 4,
                chain_end: 48,
            }),
        ],
    );
}

#[test]
fn invalid_arc_flag() {
    assert_svg_path_cst_err(
        b"m0 0a100,100 0 2 1 250,200",
        SyntaxError::InvalidArcFlag {
            index: 15,
            character: '2',
            command: 'a',
        },
    );
}

#[test]
fn moveto_quadratic() {
    assert_svg_path_cst(
        b"m0 0q100,100 250,200 150 150 300 300",
        vec![
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoLower,
                args: vec![0.0, 0.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoLower),
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
                        value: 0.0,
                        start: 1,
                        end: 2,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 2,
                        end: 3,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
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
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::QuadraticLower,
                args: vec![100.0, 100.0, 250.0, 200.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::QuadraticLower),
                    SVGPathCSTNode::Number {
                        raw_number: "100".into(),
                        value: 100.0,
                        start: 5,
                        end: 8,
                    },
                    SVGPathCSTNode::Comma { start: 8 },
                    SVGPathCSTNode::Number {
                        raw_number: "100".into(),
                        value: 100.0,
                        start: 9,
                        end: 12,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 12,
                        end: 13,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "250".into(),
                        value: 250.0,
                        start: 13,
                        end: 16,
                    },
                    SVGPathCSTNode::Comma { start: 16 },
                    SVGPathCSTNode::Number {
                        raw_number: "200".into(),
                        value: 200.0,
                        start: 17,
                        end: 20,
                    },
                ],
                start: 4,
                end: 20,
                chained: false,
                chain_start: 4,
                chain_end: 36,
            }),
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::Space,
                start: 20,
                end: 21,
            },
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::QuadraticLower,
                args: vec![150.0, 150.0, 300.0, 300.0],
                cst: vec![
                    SVGPathCSTNode::Number {
                        raw_number: "150".into(),
                        value: 150.0,
                        start: 21,
                        end: 24,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 24,
                        end: 25,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "150".into(),
                        value: 150.0,
                        start: 25,
                        end: 28,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 28,
                        end: 29,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "300".into(),
                        value: 300.0,
                        start: 29,
                        end: 32,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 32,
                        end: 33,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "300".into(),
                        value: 300.0,
                        start: 33,
                        end: 36,
                    },
                ],
                start: 21,
                end: 36,
                chained: true,
                chain_start: 4,
                chain_end: 36,
            }),
        ],
    );
}

#[test]
fn moveto_smooth_quadratic() {
    assert_svg_path_cst(
        b"m0 0t100,100 250,200",
        vec![
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoLower,
                args: vec![0.0, 0.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoLower),
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
                        value: 0.0,
                        start: 1,
                        end: 2,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 2,
                        end: 3,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
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
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::SmoothQuadraticLower,
                args: vec![100.0, 100.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::SmoothQuadraticLower),
                    SVGPathCSTNode::Number {
                        raw_number: "100".into(),
                        value: 100.0,
                        start: 5,
                        end: 8,
                    },
                    SVGPathCSTNode::Comma { start: 8 },
                    SVGPathCSTNode::Number {
                        raw_number: "100".into(),
                        value: 100.0,
                        start: 9,
                        end: 12,
                    },
                ],
                start: 4,
                end: 12,
                chained: false,
                chain_start: 4,
                chain_end: 20,
            }),
            SVGPathCSTNode::Whitespace {
                wsp: &WSP::Space,
                start: 12,
                end: 13,
            },
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::SmoothQuadraticLower,
                args: vec![250.0, 200.0],
                cst: vec![
                    SVGPathCSTNode::Number {
                        raw_number: "250".into(),
                        value: 250.0,
                        start: 13,
                        end: 16,
                    },
                    SVGPathCSTNode::Comma { start: 16 },
                    SVGPathCSTNode::Number {
                        raw_number: "200".into(),
                        value: 200.0,
                        start: 17,
                        end: 20,
                    },
                ],
                start: 13,
                end: 20,
                chained: true,
                chain_start: 4,
                chain_end: 20,
            }),
        ],
    );
}

#[test]
fn invalid_multiple_commas() {
    assert_svg_path_cst_err(
        b"m0 0,,100,100",
        SyntaxError::InvalidNumber {
            number: ",".into(),
            start: 5,
            end: 6,
        },
    );
}

#[test]
fn arc_with_flags_together() {
    assert_svg_path_cst(
        b"m0 0a1.862 1.862 0 00-.248.033",
        vec![
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::MovetoLower,
                args: vec![0.0, 0.0],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::MovetoLower),
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
                        value: 0.0,
                        start: 1,
                        end: 2,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 2,
                        end: 3,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
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
            SVGPathCSTNode::Segment(SVGPathSegment {
                command: &SVGPathCommand::ArcLower,
                args: vec![1.862, 1.862, 0.0, 0.0, 0.0, -0.248, 0.033],
                cst: vec![
                    SVGPathCSTNode::Command(&SVGPathCommand::ArcLower),
                    SVGPathCSTNode::Number {
                        raw_number: "1.862".into(),
                        value: 1.862,
                        start: 5,
                        end: 10,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 10,
                        end: 11,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "1.862".into(),
                        value: 1.862,
                        start: 11,
                        end: 16,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 16,
                        end: 17,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
                        value: 0.0,
                        start: 17,
                        end: 18,
                    },
                    SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: 18,
                        end: 19,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
                        value: 0.0,
                        start: 19,
                        end: 20,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: "0".into(),
                        value: 0.0,
                        start: 20,
                        end: 21,
                    },
                    SVGPathCSTNode::Sign {
                        sign: &Sign::Minus,
                        start: 21,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: ".248".into(),
                        value: 0.248,
                        start: 22,
                        end: 26,
                    },
                    SVGPathCSTNode::Number {
                        raw_number: ".033".into(),
                        value: 0.033,
                        start: 26,
                        end: 30,
                    },
                ],
                start: 4,
                end: 30,
                chained: false,
                chain_start: 4,
                chain_end: 30,
            }),
        ],
    );
}

#[test]
fn invalid_utf8_length_1() {
    assert_svg_path_cst_err(
        b"m0 0\xE1", // \xE1 is á
        SyntaxError::InvalidCharacter {
            character: 'á',
            index: 4,
            expected: "number or command",
        },
    );
}

#[test]
fn invalid_path_ending() {
    // https://github.com/simple-icons/simple-icons/pull/11053
    assert_svg_path_cst_err(
        b"m2.249 8.801a2.6 2.6 0 0 1-2.6 2.6 2.6 2.6 0 0 1-2.6-2.6 2.6 2.6 0 0 1 2.6-2.6 2.6 2.6 0 0 1 2.6 2.6z55",
        SyntaxError::InvalidCharacter {
            character: '5',
            index: 101,
            expected: "command",
        }
    );
}

#[test]
fn simple_icons_icon_path() {
    let cst = svg_path_cst(include_bytes!("../../fuzz/corpus/simpleicons.txt"));
    assert!(cst.is_ok());
    assert_eq!(cst.unwrap().len(), 46);
}

#[test]
fn sign_as_char() {
    assert_eq!(Sign::Plus as u8 as char, '+');
    assert_eq!(Sign::Minus as u8 as char, '-');
}

#[test]
fn wsp_as_char() {
    assert_eq!(WSP::Space as u8 as char, ' ');
    assert_eq!(WSP::Tab as u8 as char, '\t');
    assert_eq!(WSP::LineFeed as u8 as char, '\n');
    assert_eq!(WSP::CarriageReturn as u8 as char, '\r');
    assert_eq!(WSP::FormFeed as u8 as char, '\x0C');
}

#[test]
fn svg_path_command_as_char() {
    assert_eq!(SVGPathCommand::MovetoUpper as u8 as char, 'M');
    assert_eq!(SVGPathCommand::MovetoLower as u8 as char, 'm');
    assert_eq!(SVGPathCommand::ClosepathUpper as u8 as char, 'Z');
    assert_eq!(SVGPathCommand::ClosepathLower as u8 as char, 'z');
    assert_eq!(SVGPathCommand::LinetoUpper as u8 as char, 'L');
    assert_eq!(SVGPathCommand::LinetoLower as u8 as char, 'l');
    assert_eq!(SVGPathCommand::HorizontalUpper as u8 as char, 'H');
    assert_eq!(SVGPathCommand::HorizontalLower as u8 as char, 'h');
    assert_eq!(SVGPathCommand::VerticalUpper as u8 as char, 'V');
    assert_eq!(SVGPathCommand::VerticalLower as u8 as char, 'v');
    assert_eq!(SVGPathCommand::CurvetoUpper as u8 as char, 'C');
    assert_eq!(SVGPathCommand::CurvetoLower as u8 as char, 'c');
    assert_eq!(SVGPathCommand::SmoothCurvetoUpper as u8 as char, 'S');
    assert_eq!(SVGPathCommand::SmoothCurvetoLower as u8 as char, 's');
    assert_eq!(SVGPathCommand::QuadraticUpper as u8 as char, 'Q');
    assert_eq!(SVGPathCommand::QuadraticLower as u8 as char, 'q');
    assert_eq!(SVGPathCommand::SmoothQuadraticUpper as u8 as char, 'T');
    assert_eq!(SVGPathCommand::SmoothQuadraticLower as u8 as char, 't');
    assert_eq!(SVGPathCommand::ArcUpper as u8 as char, 'A');
    assert_eq!(SVGPathCommand::ArcLower as u8 as char, 'a');
}

/// Regression tests related to the optimization of the `is_command` function.
mod is_command_function_optimization {
    use super::*;

    #[test]
    fn b_is_not_numeric_after_wsp_before_segment() {
        assert_svg_path_cst_err(
            b"m0 0 bv3",
            SyntaxError::InvalidCharacter {
                character: 'b',
                index: 5,
                // TODO: this should be "number or command"
                expected: "command",
            },
        );
    }

    #[test]
    fn b_is_not_numeric_after_segment_and_wsp() {
        assert_svg_path_cst_err(
            b"m0 0 b",
            SyntaxError::InvalidCharacter {
                character: 'b',
                index: 5,
                // TODO: this should be "number or command"
                expected: "command",
            },
        );
    }

    #[test]
    fn b_is_not_numeric_after_segment() {
        assert_svg_path_cst_err(
            b"m0 0b",
            SyntaxError::InvalidCharacter {
                character: 'b',
                index: 4,
                // TODO: this should be "number or command"
                expected: "command",
            },
        );
    }

    #[test]
    fn b_is_not_numeric_at_coordinate() {
        assert_svg_path_cst_err(
            b"mb",
            SyntaxError::InvalidNumber {
                number: "b".into(),
                start: 1,
                end: 2,
            },
        );
    }

    #[test]
    fn b_is_not_numeric_at_coordinate_after_wsp() {
        assert_svg_path_cst_err(
            b"m b",
            SyntaxError::InvalidNumber {
                number: "b".into(),
                start: 2,
                end: 3,
            },
        );
    }
}

/// Regression tests with to the Simple Icons corpus.
mod simple_icons {
    use super::*;

    fn get_simple_icons_temp_dir() -> std::path::PathBuf {
        std::env::temp_dir().join("simple-icons")
    }

    fn clone_simple_icons_repo() {
        let simple_icons_temp_dir = get_simple_icons_temp_dir();
        if !simple_icons_temp_dir.exists() {
            std::process::Command::new("git")
                .arg("clone")
                .arg("--depth=1")
                .arg("https://github.com/simple-icons/simple-icons")
                .arg(simple_icons_temp_dir.to_str().unwrap())
                .output()
                .expect("Failed to clone Simple Icons repository.");
        }
        println!(
            "Simple Icons repository cloned to: {:?}",
            simple_icons_temp_dir.exists()
        );
    }

    fn get_simple_icons_paths() -> Vec<(String, String)> {
        let simple_icons_temp_dir = get_simple_icons_temp_dir();
        // (title, path)
        let mut svg_paths: Vec<(String, String)> = Vec::new();
        for entry in std::fs::read_dir(simple_icons_temp_dir.join("icons")).unwrap() {
            let entry = entry.unwrap();
            let icon_file_path = entry.path();
            let svg = std::fs::read_to_string(&icon_file_path).unwrap();
            let svg_path = svg.split("d=\"").nth(1).unwrap().split('"').next().unwrap();
            let title = svg
                .split("<title>")
                .nth(1)
                .unwrap()
                .split("</title>")
                .next()
                .unwrap();
            svg_paths.push((title.into(), svg_path.into()));
        }
        svg_paths
    }

    #[test]
    fn parse_all_simple_icons() {
        clone_simple_icons_repo();
        let svg_paths = get_simple_icons_paths();
        let mut errors: Vec<String> = Vec::new();
        for (title, svg_path) in svg_paths {
            let cst = svg_path_cst(svg_path.as_bytes());
            if cst.is_err() {
                errors.push(format!(
                    "Failed to parse SVG path for Simple Icon '{}'. Error: {}\nPath: {}",
                    title,
                    cst.unwrap_err(),
                    svg_path
                ));
            }
        }
        assert_eq!(errors.len(), 0, "{}", errors.join("\n\n"));
    }
}
