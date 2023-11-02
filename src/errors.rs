use snafu::prelude::*;

/// Syntax errors that can occur when parsing an SVG path
///
/// These errors try to be exhaustive.
#[derive(Debug, PartialEq, Snafu, Clone)]
pub enum SyntaxError {
    /// The first command in a path is not moveto.
    #[snafu(display(
        "Invalid SVG path command '{command}' at index {start}, expected 'M' or 'm'"
    ))]
    ExpectedMovetoCommand {
        /// Command letter found
        command: char,
        /// Index of the command in the path
        start: usize,
    },

    /// Invalid number found in path.
    #[snafu(display("Invalid number '{number}' at index {start}"))]
    InvalidNumber {
        /// Number found
        number: String,
        /// Index of the number in the path
        start: usize,
        /// Index of the end of the number in the path
        end: usize,
    },

    /// Invalid character found in path.
    #[snafu(display(
        "Invalid character '{character}' at index {index}, expected {expected}"
    ))]
    InvalidCharacter {
        /// Character found
        character: char,
        /// Index of the character in the path
        index: usize,
        /// Expected character
        expected: String,
    },

    /// Invalid path ending.
    #[snafu(display("Unexpected SVG path end at index {index}, expected {expected}"))]
    UnexpectedEnding {
        /// Index of the end of the path
        index: usize,
        /// Expected token
        expected: String,
    },

    /// Invalid SVG quaractic arc command flag argument.
    #[snafu(display("Invalid SVG path elliptical arc flag at index {index}. Expected 0 or 1 but found '{character}'"))]
    InvalidArcFlag {
        /// Command letter
        command: char,
        /// Character found instead of valid arc flag (0 or 1)
        character: char,
        /// Index of the command in the path
        index: usize,
    },

    /// Invalid SVG quaractic arc command radius argument.
    #[snafu(display("Invalid SVG path elliptical arc command '{command}' radius at index {start}. Expected positive number but found '{value}'"))]
    InvalidArcRadius {
        /// Command letter
        command: char,
        /// Index of the command in the path
        start: usize,
        /// Index of the end of the number in the path
        end: usize,
        /// Value found
        value: f64,
    },
}
