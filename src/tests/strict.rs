use crate::tests::helpers::*;
use crate::SyntaxError;

#[test]
fn empty() {
    assert_svg_path_cst_err(
        b"",
        SyntaxError::UnexpectedEnding {
            expected: "moveto command",
            index: 0,
        },
    );
}

#[test]
fn none() {
    assert_svg_path_cst_err(
        b"none",
        SyntaxError::ExpectedMovetoCommand {
            character: 'n',
            index: 0,
        },
    );
}

#[test]
fn whitespaces() {
    assert_svg_path_cst_err(
        b"\t\n\r \x0C",
        SyntaxError::ExpectedMovetoCommand {
            character: '\t',
            index: 0,
        },
    );
}
