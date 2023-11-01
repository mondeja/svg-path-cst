use snafu::prelude::*;

/// Syntax errors that can occur when parsing an SVG path.
///
/// These errors try to be exhaustive,.
#[derive(Debug, PartialEq, Snafu, Clone)]
pub enum SyntaxError {
    /// The first command in a path is not moveto.
    #[snafu(display(
        "Invalid SVG path command '{command}' at index {index}, expected 'M' or 'm'"
    ))]
    ExpectedMovetoCommand { command: char, index: usize },

    /// Invalid number found in path.
    #[snafu(display("Invalid number '{number}' at index {index}"))]
    InvalidNumber { number: String, index: usize },

    /// Invalid character found in path.
    #[snafu(display(
        "Invalid character '{character}' at index {index}, expected {expected}"
    ))]
    InvalidCharacter {
        character: char,
        index: usize,
        expected: String,
    },

    /// Invalid path ending.
    #[snafu(display("Invalid SVG path end at index {index}, expected {expected}"))]
    InvalidPathTermination { index: usize, expected: String },

    /// Invalid SVG quaractic arc command flag argument.
    #[snafu(display("Invalid SVG path elliptical arc command '{command}' at index {index}. Expected 0 or 1 (flag) but found '{value}'"))]
    InvalidArcFlag {
        command: char,
        index: usize,
        value: f64,
    },
}
