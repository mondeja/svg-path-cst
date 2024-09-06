#![doc = include_str!("../README.md")]
#![warn(missing_docs)]
#![doc(test(attr(deny(warnings))))]
#![cfg_attr(not(test), no_std)]

extern crate alloc;
use crate::alloc::{
    string::{String, ToString},
    vec,
    vec::Vec,
};

#[cfg(doctest)]
::doc_comment::doctest!("../README.md");

#[cfg(test)]
pub(crate) mod tests;

mod errors;
pub use errors::SyntaxError;

/// Checks if a character is a command character.
///
/// The previous implementation used a slice and `slice::contains()`, which
/// was O(n). The current implementation is O(1), but is not exact because
/// other characters like `b` and `B` are considered commands. Anyways,
/// in the context that this function is used, the character raises a syntax
/// error in the next iteration of the parser.
#[inline]
fn is_command(c: u8) -> bool {
    let bitwise3 = c >> 3;
    bitwise3 > 7 && bitwise3 < 16
}

#[inline]
fn is_wsp_or_comma(c: u8) -> bool {
    c == b' ' || c == b',' || c == b'\n' || c == b'\t' || c == b'\x0C' || c == b'\r'
}

// Sign, number and value
type Coordinate = (Option<SVGPathCSTNode>, SVGPathCSTNode, f64);

/// SVG path command
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum SVGPathCommand {
    /// M
    MovetoUpper = b'M',
    /// m
    MovetoLower = b'm',
    /// L
    LinetoUpper = b'L',
    /// l
    LinetoLower = b'l',
    /// H
    HorizontalUpper = b'H',
    /// h
    HorizontalLower = b'h',
    /// V
    VerticalUpper = b'V',
    /// v
    VerticalLower = b'v',
    /// Z
    ClosepathUpper = b'Z',
    /// z
    ClosepathLower = b'z',
    /// C
    CurvetoUpper = b'C',
    /// c
    CurvetoLower = b'c',
    /// S
    SmoothCurvetoUpper = b'S',
    /// s
    SmoothCurvetoLower = b's',
    /// A
    ArcUpper = b'A',
    /// a
    ArcLower = b'a',
    /// Q
    QuadraticUpper = b'Q',
    /// q
    QuadraticLower = b'q',
    /// T
    SmoothQuadraticUpper = b'T',
    /// t
    SmoothQuadraticLower = b't',
}

impl SVGPathCommand {
    /// Returns the number of arguments that the command takes
    pub fn capacity(&self) -> usize {
        match self {
            SVGPathCommand::MovetoUpper => 2,
            SVGPathCommand::MovetoLower => 2,
            SVGPathCommand::LinetoUpper => 2,
            SVGPathCommand::LinetoLower => 2,
            SVGPathCommand::HorizontalUpper => 1,
            SVGPathCommand::HorizontalLower => 1,
            SVGPathCommand::VerticalUpper => 1,
            SVGPathCommand::VerticalLower => 1,
            SVGPathCommand::ClosepathUpper => 0,
            SVGPathCommand::ClosepathLower => 0,
            SVGPathCommand::CurvetoUpper => 6,
            SVGPathCommand::CurvetoLower => 6,
            SVGPathCommand::SmoothCurvetoUpper => 4,
            SVGPathCommand::SmoothCurvetoLower => 4,
            SVGPathCommand::ArcUpper => 7,
            SVGPathCommand::ArcLower => 7,
            SVGPathCommand::QuadraticUpper => 4,
            SVGPathCommand::QuadraticLower => 4,
            SVGPathCommand::SmoothQuadraticUpper => 2,
            SVGPathCommand::SmoothQuadraticLower => 2,
        }
    }

    /// Returns the SVG path command character as `u8`
    #[deprecated(
        since = "0.1.2",
        note = "This method has been deprecated and will be removed in v1.0.0. Use `as u8` instead."
    )]
    pub fn to_u8(&self) -> u8 {
        match self {
            SVGPathCommand::MovetoUpper => b'M',
            SVGPathCommand::MovetoLower => b'm',
            SVGPathCommand::LinetoUpper => b'L',
            SVGPathCommand::LinetoLower => b'l',
            SVGPathCommand::HorizontalUpper => b'H',
            SVGPathCommand::HorizontalLower => b'h',
            SVGPathCommand::VerticalUpper => b'V',
            SVGPathCommand::VerticalLower => b'v',
            SVGPathCommand::ClosepathUpper => b'Z',
            SVGPathCommand::ClosepathLower => b'z',
            SVGPathCommand::CurvetoUpper => b'C',
            SVGPathCommand::CurvetoLower => b'c',
            SVGPathCommand::SmoothCurvetoUpper => b'S',
            SVGPathCommand::SmoothCurvetoLower => b's',
            SVGPathCommand::ArcUpper => b'A',
            SVGPathCommand::ArcLower => b'a',
            SVGPathCommand::QuadraticUpper => b'Q',
            SVGPathCommand::QuadraticLower => b'q',
            SVGPathCommand::SmoothQuadraticUpper => b'T',
            SVGPathCommand::SmoothQuadraticLower => b't',
        }
    }
}

/// Whitespace token
///
/// Represents a whitespace character in the SVG path following
/// the v1.1 SVG Path specification.
///
/// # Example
///
/// A path compound by whitespaces is considered a valid empty path
/// according to the SVG Path v1.1 specification.
///
/// ```
/// # // Don't test this in strict mode because it would fail
/// # #[cfg(not(feature = "strict"))] {
/// use svg_path_cst::{svg_path_cst, SVGPathCSTNode, WSP};
///
/// let cst = svg_path_cst(b" \t\n\r \x0C");
/// assert_eq!(
///     cst,
///     Ok(vec![
///         SVGPathCSTNode::Whitespace{
///             wsp: &WSP::Space,
///             start: 0,
///             end: 1,
///         },
///         SVGPathCSTNode::Whitespace{
///             wsp: &WSP::Tab,
///             start: 1,
///             end: 2,
///         },
///         SVGPathCSTNode::Whitespace{
///             wsp: &WSP::LineFeed,
///             start: 2,
///             end: 3,
///         },
///         SVGPathCSTNode::Whitespace{
///             wsp: &WSP::CarriageReturn,
///             start: 3,
///             end: 4,
///         },
///         SVGPathCSTNode::Whitespace{
///             wsp: &WSP::Space,
///             start: 4,
///             end: 5,
///         },
///         SVGPathCSTNode::Whitespace{
///             wsp: &WSP::FormFeed,
///             start: 5,
///             end: 6,
///         },
///     ])
/// );
///
/// for node in cst.unwrap() {
///     match node {
///         SVGPathCSTNode::Whitespace{wsp, start, ..} => {
///             println!("'{}' at index {}", *wsp as u8 as char, start);
///         },
///         _ => (),
///     }
/// }
/// # }
/// ```
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum WSP {
    /// Space (U+0020)
    Space = b' ',
    /// Tab (U+0009)
    Tab = b'\t',
    /// Line Feed (U+000A)
    LineFeed = b'\n',
    /// Form Feed (U+000C)
    FormFeed = b'\x0C',
    /// Carriage Return (U+000D)
    CarriageReturn = b'\r',
}

impl WSP {
    /// Returns the whitespace character as `u8`
    #[deprecated(
        since = "0.1.2",
        note = "This method has been deprecated and will be removed in v1.0.0. Use `as u8` instead."
    )]
    pub fn to_u8(&self) -> u8 {
        match self {
            WSP::Space => b' ',
            WSP::Tab => b'\t',
            WSP::LineFeed => b'\n',
            WSP::FormFeed => b'\x0C',
            WSP::CarriageReturn => b'\r',
        }
    }
}

/// Sign token
///
/// Represents a sign character in the SVG path. The following characters are
/// considered signs following the v1.1 SVG Path specification:
///
/// - Plus (U+002B)
/// - Minus (U+002D)
///
/// # Example
///
/// ```
/// use svg_path_cst::{
///     svg_path_cst, SVGPathCSTNode, SVGPathCommand, SVGPathSegment, Sign,
/// };
///
/// let cst = svg_path_cst(b"M+10-10").unwrap();
/// assert_eq!(
///     cst,
///     vec![
///         SVGPathCSTNode::Segment(SVGPathSegment {
///             command: &SVGPathCommand::MovetoUpper,
///             args: vec![10.0, -10.0],
///             cst: vec![
///                 SVGPathCSTNode::Command(&SVGPathCommand::MovetoUpper),
///                 SVGPathCSTNode::Sign{
///                    sign: &Sign::Plus,
///                    start: 1,
///                 },
///                 SVGPathCSTNode::Number{
///                     raw_number: "10".to_string(),
///                     value: 10.0,
///                     start: 2,
///                     end: 4,
///                 },
///                 SVGPathCSTNode::Sign{
///                     sign: &Sign::Minus,
///                     start: 4,
///                 },
///                 SVGPathCSTNode::Number{
///                     raw_number: "10".to_string(),
///                     value: 10.0,
///                     start: 5,
///                     end: 7,
///                 },
///             ],
///             start: 0,
///             end: 7,
///             chained: false,
///             chain_start: 0,
///             chain_end: 7,
///         }),
///     ]
/// );
///
/// for node in &cst {
///     match node {
///         SVGPathCSTNode::Sign{sign, ..} => {
///             match sign {
///                Sign::Plus => println!("+"),
///                Sign::Minus => println!("-"),
///             }
///         }
///         _ => (),
///     }
/// }
///
/// // or just use `as u8 as char`
/// for node in cst {
///     match node {
///         SVGPathCSTNode::Sign{sign, ..} => {
///             println!("{}", *sign as u8 as char);
///         }
///         _ => (),
///     }
/// }
/// ```
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum Sign {
    /// Plus sign (U+002B)
    Plus = b'+',
    /// Minus sign (U+002D)
    Minus = b'-',
}

impl Sign {
    /// Returns the sign character as `u8`
    #[deprecated(
        since = "0.1.2",
        note = "This method has been deprecated and will be removed in v1.0.0. Use `as u8` instead."
    )]
    pub fn to_u8(&self) -> u8 {
        match self {
            Sign::Plus => b'+',
            Sign::Minus => b'-',
        }
    }
}

/// A CST node
///
/// Represents a node in the SVG path concrete syntax tree.
///
/// The `SVGPathCommand` node represents a SVG path command and it
/// stores children CST nodes that represent the command arguments
/// in the `cst` field.
#[derive(Debug, PartialEq, Clone)]
pub enum SVGPathCSTNode {
    /// None token. This is used to represent an empty SVG path with a
    /// explicit `"none"` value in `d` attribute.
    None,
    /// Whitespace
    Whitespace {
        /// Whitespace token
        wsp: &'static WSP,
        /// Start index of the whitespace in the SVG path string
        start: usize,
        /// End index of the whitespace in the SVG path string
        end: usize,
    },
    /// A SVG path segment tokens struct
    Segment(SVGPathSegment),

    /// Plus and minus numeric sign tokens
    Sign {
        /// Sign enum
        sign: &'static Sign,
        /// Start index of the sign in the SVG path string
        start: usize,
    },
    /// Number token
    Number {
        /// Number string
        raw_number: String,
        /// Number value
        value: f64,
        /// Start index of the number in the SVG path string
        start: usize,
        /// End index of the number in the SVG path string
        end: usize,
    },
    /// Comma token
    Comma {
        /// Start index of the comma in the SVG path string
        start: usize,
    },
    /// SVG path command token
    Command(&'static SVGPathCommand),
}

/// A SVG path segment
///
/// Represents a segment in the SVG path concrete syntax tree. It includes
// non sintactical information such as the start and end index of the segment
/// in the SVG path string.
#[derive(Debug, PartialEq, Clone)]
pub struct SVGPathSegment {
    /// SVG path command that uses this segment, even implicit
    pub command: &'static SVGPathCommand,
    /// Explicit argument values passed to the command
    pub args: Vec<f64>,
    /// CST nodes that represent sintactically the command arguments
    pub cst: Vec<SVGPathCSTNode>,
    /// Start index of the segment in the SVG path string
    pub start: usize,
    /// End index of the segment in the SVG path string
    pub end: usize,
    /// Whether the segment is chained to the previous one
    pub chained: bool,
    /// Start index of the chain of segments in the SVG path string.
    /// Only valid if `chained` is `true`.
    pub chain_start: usize,
    /// End index of the chain of segments in the SVG path string.
    /// Only valid if `chained` is `true`.
    pub chain_end: usize,
}

fn new_segment(
    command: &'static SVGPathCommand,
    start: usize,
    chained: bool,
) -> SVGPathSegment {
    let capacity = command.capacity();
    SVGPathSegment {
        command,
        args: Vec::with_capacity(capacity),
        // After some profiling found that *3 looks like a good compromise
        // between memory usage and reallocation
        cst: Vec::with_capacity(capacity * 3),
        start,
        end: start,
        chained,
        chain_start: start,
        chain_end: start,
    }
}

macro_rules! set_commands_chain_info {
    ($cst:expr, $nodes:expr, $chain_start:expr, $chain_end:expr) => {
        for node in $nodes {
            match node {
                SVGPathCSTNode::Segment(mut segment) => {
                    segment.chain_start = $chain_start;
                    segment.chain_end = $chain_end;
                    $cst.push(SVGPathCSTNode::Segment(segment));
                }
                _ => $cst.push(node),
            }
        }
    };
}

#[derive(Clone)]
struct Parser<'a> {
    index: usize,
    path: &'a [u8],
}

impl<'a> Parser<'a> {
    pub fn new(path: &'a [u8]) -> Self {
        Self { index: 0, path }
    }

    fn next(&mut self) -> Option<u8> {
        if self.index == self.path.len() {
            return None;
        }
        let ch = self.path[self.index];
        self.index += 1;
        Some(ch)
    }

    fn peek(&mut self) -> Option<u8> {
        if self.index == self.path.len() {
            return None;
        }
        Some(self.path[self.index])
    }

    fn check_unexpected_end(
        &mut self,
        expected: &'static str,
    ) -> Result<(), SyntaxError> {
        if self.peek().is_none() {
            return Err(SyntaxError::UnexpectedEnding {
                index: self.index - 1,
                expected,
            });
        }
        Ok(())
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(level = "trace", skip_all))]
    fn parse_whitespaces(&mut self, nodes: &mut Vec<SVGPathCSTNode>) {
        while let Some(next) = self.peek() {
            match next {
                b' ' => {
                    nodes.push(SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: self.index,
                        end: self.index + 1,
                    });
                    self.index += 1;
                }
                b'\t' => {
                    nodes.push(SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Tab,
                        start: self.index,
                        end: self.index + 1,
                    });
                    self.index += 1;
                }
                b'\n' => {
                    nodes.push(SVGPathCSTNode::Whitespace {
                        wsp: &WSP::LineFeed,
                        start: self.index,
                        end: self.index + 1,
                    });
                    self.index += 1;
                }
                b'\x0C' => {
                    nodes.push(SVGPathCSTNode::Whitespace {
                        wsp: &WSP::FormFeed,
                        start: self.index,
                        end: self.index + 1,
                    });
                    self.index += 1;
                }
                b'\r' => {
                    nodes.push(SVGPathCSTNode::Whitespace {
                        wsp: &WSP::CarriageReturn,
                        start: self.index,
                        end: self.index + 1,
                    });
                    self.index += 1;
                }
                _ => break,
            }
        }
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(level = "trace", skip_all, err(Debug))
    )]
    fn parse_mandatory_comma_wsp(
        &mut self,
        nodes: &mut Vec<SVGPathCSTNode>,
    ) -> Result<(), SyntaxError> {
        if let Some(next) = self.peek() {
            if next == b',' {
                nodes.push(SVGPathCSTNode::Comma { start: self.index });
                self.index += 1;
                self.parse_whitespaces(nodes);
            } else {
                self.parse_whitespaces(nodes);
                if let Some(next_after_wsp) = self.peek() {
                    if next_after_wsp == b',' {
                        nodes.push(SVGPathCSTNode::Comma { start: self.index });
                        self.index += 1;
                        self.parse_whitespaces(nodes);
                    }
                }
            }
        } else {
            return Err(SyntaxError::UnexpectedEnding {
                index: self.index - 1,
                expected: "comma or whitespace",
            });
        }

        Ok(())
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(level = "trace", skip_all))]
    fn parse_comma_wsp(&mut self, nodes: &mut Vec<SVGPathCSTNode>) {
        if let Some(next) = self.peek() {
            if next == b',' {
                nodes.push(SVGPathCSTNode::Comma { start: self.index });
                self.index += 1;
                self.parse_whitespaces(nodes);
            } else {
                self.parse_whitespaces(nodes);
                if let Some(next_after_wsp) = self.peek() {
                    if next_after_wsp == b',' {
                        nodes.push(SVGPathCSTNode::Comma { start: self.index });
                        self.index += 1;
                        self.parse_whitespaces(nodes);
                    }
                }
            }
        }
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(level = "trace", skip_all, err(Debug))
    )]
    fn parse_number(&mut self) -> Result<SVGPathCSTNode, SyntaxError> {
        let start = self.index;
        let mut number: Vec<u8> = Vec::new();
        let mut has_dot = false;
        let mut has_e = false;
        let mut has_sign = false;
        let mut has_digit = false;

        while let Some(next) = self.next() {
            match next {
                b'1'..=b'9' => {
                    has_digit = true;
                    number.push(next);
                }
                b'0' => {
                    if number == [b'0'] {
                        break;
                    }
                    has_digit = true;
                    number.push(next);
                }
                b'.' => {
                    if has_dot {
                        self.index -= 1;
                        break;
                    }
                    number.push(next);
                    has_dot = true;
                }
                b'+' | b'-' => {
                    if has_sign || has_dot || has_digit && !has_e {
                        self.index -= 1;
                        break;
                    }
                    number.push(next);
                    has_sign = true;
                }
                b'e' | b'E' => {
                    if has_e {
                        let number_length = number.clone().len();
                        let number_as_string =
                            unsafe { String::from_utf8_unchecked(number) };
                        return Err(SyntaxError::InvalidNumber {
                            number: number_as_string,
                            start,
                            end: start + number_length,
                        });
                    }
                    number.push(next);
                    has_e = true;
                }
                _ => {
                    if !is_command(next) && !is_wsp_or_comma(next) {
                        return Err(SyntaxError::InvalidCharacter {
                            character: next as char,
                            index: self.index - 1,
                            expected: "number or command",
                        });
                    } else if !has_digit {
                        number.push(next);
                    }

                    self.index -= 1;
                    break;
                }
            }
        }

        if !has_digit {
            let processed_length = number.len();
            let processed_as_string = unsafe { String::from_utf8_unchecked(number) };
            return Err(SyntaxError::InvalidNumber {
                number: processed_as_string,
                start,
                end: start + processed_length,
            });
        }

        let number_length = number.len();
        let number_as_string = unsafe { String::from_utf8_unchecked(number) };
        match number_as_string.parse::<f64>() {
            Ok(value) => Ok(SVGPathCSTNode::Number {
                raw_number: number_as_string,
                value,
                start,
                end: start + number_length,
            }),
            Err(_) => Err(SyntaxError::InvalidNumber {
                number: number_as_string,
                start,
                end: start + number_length,
            }),
        }
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(level = "trace", skip_all))]
    fn parse_sign(&mut self) -> Option<SVGPathCSTNode> {
        match self.peek() {
            Some(b'+') => {
                self.index += 1;
                Some(SVGPathCSTNode::Sign {
                    sign: &Sign::Plus,
                    start: self.index - 1,
                })
            }
            Some(b'-') => {
                self.index += 1;
                Some(SVGPathCSTNode::Sign {
                    sign: &Sign::Minus,
                    start: self.index - 1,
                })
            }
            _ => None,
        }
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(level = "trace", skip_all, err(Debug))
    )]
    fn parse_flag(&mut self, command: u8) -> Result<f64, SyntaxError> {
        match self.next() {
            Some(b'0') => Ok(0.0),
            Some(b'1') => Ok(1.0),
            Some(character) => Err(SyntaxError::InvalidArcFlag {
                index: self.index - 1,
                character: character as char,
                command: command as char,
            }),
            None => Err(SyntaxError::UnexpectedEnding {
                index: self.index,
                expected: "flag (0 or 1)",
            }),
        }
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(level = "trace", skip_all, err(Debug))
    )]
    fn parse_coordinate(&mut self) -> Result<Coordinate, SyntaxError> {
        let sign_node = self.parse_sign();
        let SVGPathCSTNode::Number {
            value,
            start,
            end,
            raw_number,
        } = self.parse_number()?
        else {
            Err(SyntaxError::UnexpectedEnding {
                index: self.index - 1,
                expected: "number",
            })?
        };

        let coord_value = if let Some(SVGPathCSTNode::Sign { sign, .. }) = &sign_node {
            match sign {
                Sign::Plus => value,
                Sign::Minus => -value,
            }
        } else {
            value
        };

        Ok((
            sign_node,
            SVGPathCSTNode::Number {
                value,
                start,
                end,
                raw_number,
            },
            coord_value,
        ))
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(level = "trace", skip_all, err(Debug))
    )]
    fn parse_coordinate_pair(
        &mut self,
        nodes: &mut Vec<SVGPathCSTNode>,
        values: &mut Vec<f64>,
    ) -> Result<(), SyntaxError> {
        let (first_sign, first_number, first_value) = self.parse_coordinate()?;
        if let Some(sign) = first_sign {
            nodes.push(sign);
        }
        nodes.push(first_number);
        values.push(first_value);
        self.parse_mandatory_comma_wsp(nodes)?;
        let (second_sign, second_number, second_value) = self.parse_coordinate()?;
        if let Some(sign) = second_sign {
            nodes.push(sign);
        }
        nodes.push(second_number);
        values.push(second_value);

        Ok(())
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(level = "trace", skip_all, err(Debug))
    )]
    fn parse_two_operands_command(
        &mut self,
        command: &'static SVGPathCommand,
    ) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
        let mut first_segment = new_segment(command, self.index - 1, false);
        first_segment.cst.push(SVGPathCSTNode::Command(command));
        self.parse_whitespaces(&mut first_segment.cst);

        self.parse_coordinate_pair(&mut first_segment.cst, &mut first_segment.args)?;
        first_segment.end = self.index;
        first_segment.chain_end = self.index;

        let mut next_nodes = Vec::new();
        self.parse_whitespaces(&mut next_nodes);
        self.parse_comma_wsp(&mut first_segment.cst);

        if let Some(mut next) = self.peek() {
            while !is_command(next) {
                let mut segment = new_segment(command, self.index, true);
                self.parse_coordinate_pair(&mut segment.cst, &mut segment.args)?;
                first_segment.chain_end = self.index;
                segment.end = self.index;
                next_nodes.push(SVGPathCSTNode::Segment(segment));
                self.parse_whitespaces(&mut next_nodes);
                self.parse_comma_wsp(&mut next_nodes);

                if let Some(n) = self.peek() {
                    next = n;
                } else {
                    break;
                }
            }
        }

        let (start, end) = (first_segment.chain_start, first_segment.chain_end);
        let mut cst = Vec::with_capacity(1 + next_nodes.len());
        cst.push(SVGPathCSTNode::Segment(first_segment));
        set_commands_chain_info!(cst, next_nodes, start, end);
        Ok(cst)
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(level = "trace", skip_all, err(Debug))
    )]
    fn parse_four_operands_command(
        &mut self,
        command: &'static SVGPathCommand,
    ) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
        let mut first_segment = new_segment(command, self.index - 1, false);
        first_segment.cst.push(SVGPathCSTNode::Command(command));

        // previously written as `for _ in 0..2` but unrolled
        self.parse_whitespaces(&mut first_segment.cst);
        self.parse_comma_wsp(&mut first_segment.cst);
        self.check_unexpected_end("coordinate pair")?;
        self.parse_coordinate_pair(&mut first_segment.cst, &mut first_segment.args)?;

        self.parse_whitespaces(&mut first_segment.cst);
        self.parse_comma_wsp(&mut first_segment.cst);
        self.check_unexpected_end("coordinate pair")?;
        self.parse_coordinate_pair(&mut first_segment.cst, &mut first_segment.args)?;

        first_segment.chain_end = self.index;
        first_segment.end = self.index;

        let mut next_nodes = Vec::new();
        self.parse_whitespaces(&mut next_nodes);
        self.parse_comma_wsp(&mut next_nodes);

        if let Some(mut next) = self.peek() {
            while !is_command(next) {
                let mut segment = new_segment(command, self.index, true);
                self.parse_coordinate_pair(&mut segment.cst, &mut segment.args)?;
                self.parse_whitespaces(&mut segment.cst);
                self.check_unexpected_end("coordinate pair")?;

                self.parse_comma_wsp(&mut segment.cst);

                self.parse_coordinate_pair(&mut segment.cst, &mut segment.args)?;

                first_segment.chain_end = self.index;
                segment.end = self.index;
                next_nodes.push(SVGPathCSTNode::Segment(segment));
                self.parse_whitespaces(&mut next_nodes);
                self.parse_comma_wsp(&mut next_nodes);
                if let Some(n) = self.peek() {
                    next = n;
                } else {
                    break;
                }
            }
        }

        let (start, end) = (first_segment.chain_start, first_segment.chain_end);
        let mut cst = Vec::with_capacity(1 + next_nodes.len());
        cst.push(SVGPathCSTNode::Segment(first_segment));
        set_commands_chain_info!(cst, next_nodes, start, end);
        Ok(cst)
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(level = "trace", skip_all, err(Debug))
    )]
    fn parse_curveto(
        &mut self,
        command: &'static SVGPathCommand,
    ) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
        let mut first_segment = new_segment(command, self.index - 1, false);
        first_segment.cst.push(SVGPathCSTNode::Command(command));

        // previously written as `for _ in 0..3` but unrolled
        // because creates an iterator which is costly
        self.parse_whitespaces(&mut first_segment.cst);
        self.check_unexpected_end("coordinate pair")?;
        self.parse_coordinate_pair(&mut first_segment.cst, &mut first_segment.args)?;

        self.parse_comma_wsp(&mut first_segment.cst);
        self.parse_whitespaces(&mut first_segment.cst);
        self.check_unexpected_end("coordinate pair")?;
        self.parse_coordinate_pair(&mut first_segment.cst, &mut first_segment.args)?;

        self.parse_comma_wsp(&mut first_segment.cst);
        self.parse_whitespaces(&mut first_segment.cst);
        self.check_unexpected_end("coordinate pair")?;
        self.parse_coordinate_pair(&mut first_segment.cst, &mut first_segment.args)?;

        first_segment.chain_end = self.index;
        first_segment.end = self.index;

        let mut next_nodes = Vec::new();
        self.parse_whitespaces(&mut next_nodes);
        self.parse_comma_wsp(&mut first_segment.cst);

        if let Some(mut next) = self.peek() {
            while !is_command(next) {
                let mut segment = new_segment(command, self.index, true);
                self.parse_coordinate_pair(&mut segment.cst, &mut segment.args)?;
                self.parse_whitespaces(&mut segment.cst);
                self.parse_comma_wsp(&mut segment.cst);

                self.check_unexpected_end("coordinate pair")?;
                self.parse_coordinate_pair(&mut segment.cst, &mut segment.args)?;
                self.parse_whitespaces(&mut segment.cst);
                self.parse_comma_wsp(&mut segment.cst);

                self.check_unexpected_end("coordinate pair")?;
                self.parse_coordinate_pair(&mut segment.cst, &mut segment.args)?;

                first_segment.chain_end = self.index;
                segment.end = self.index;
                next_nodes.push(SVGPathCSTNode::Segment(segment));
                self.parse_whitespaces(&mut next_nodes);
                self.parse_comma_wsp(&mut next_nodes);
                if let Some(n) = self.peek() {
                    next = n;
                } else {
                    break;
                }
            }
        }

        let (start, end) = (first_segment.chain_start, first_segment.chain_end);
        let mut cst = Vec::with_capacity(1 + next_nodes.len());
        cst.push(SVGPathCSTNode::Segment(first_segment));
        set_commands_chain_info!(cst, next_nodes, start, end);
        Ok(cst)
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(level = "trace", skip_all, err(Debug))
    )]
    fn parse_arc(
        &mut self,
        command: &'static SVGPathCommand,
    ) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
        let u8_command = *command as u8;
        let mut first_segment = new_segment(command, self.index - 1, false);
        first_segment.cst.push(SVGPathCSTNode::Command(command));
        self.parse_whitespaces(&mut first_segment.cst);

        // 0 - 1
        self.check_unexpected_end("number")?;
        self.parse_coordinate_pair(&mut first_segment.cst, &mut first_segment.args)?;
        self.parse_comma_wsp(&mut first_segment.cst);

        // 2
        self.check_unexpected_end("number")?;
        let index_before_parse_radius = self.index;
        let (sign_node, number_node, value) = self.parse_coordinate()?;
        if sign_node.is_some() || value > 360.0 {
            return Err(SyntaxError::InvalidArcRadius {
                start: index_before_parse_radius,
                end: self.index,
                value,
                command: u8_command as char,
            });
        }
        first_segment.args.push(value);
        first_segment.cst.push(number_node);
        self.parse_comma_wsp(&mut first_segment.cst);

        // loop unrolling here, previously written as `for _ in 0..2`
        // 0
        let value = self.parse_flag(u8_command)?;
        first_segment.args.push(value);
        first_segment.cst.push(SVGPathCSTNode::Number {
            raw_number: if value == 0.0 {
                "0".to_string()
            } else {
                "1".to_string()
            },
            value,
            start: self.index - 1,
            end: self.index,
        });
        self.parse_comma_wsp(&mut first_segment.cst);

        // 1
        let value = self.parse_flag(u8_command)?;
        first_segment.args.push(value);
        first_segment.cst.push(SVGPathCSTNode::Number {
            raw_number: if value == 0.0 {
                "0".to_string()
            } else {
                "1".to_string()
            },
            value,
            start: self.index - 1,
            end: self.index,
        });
        self.parse_comma_wsp(&mut first_segment.cst);

        self.parse_coordinate_pair(&mut first_segment.cst, &mut first_segment.args)?;

        first_segment.chain_end = self.index;
        first_segment.end = self.index;

        let mut next_nodes = Vec::new();
        self.parse_whitespaces(&mut next_nodes);
        self.parse_comma_wsp(&mut next_nodes);

        if let Some(mut next) = self.peek() {
            while !is_command(next) {
                let mut segment = new_segment(command, self.index, true);

                // 0 - 1
                self.check_unexpected_end("number")?;
                self.parse_coordinate_pair(&mut segment.cst, &mut segment.args)?;
                self.parse_comma_wsp(&mut segment.cst);

                self.parse_whitespaces(&mut next_nodes);

                // 2
                self.check_unexpected_end("number")?;
                let index_before_parse_radius = self.index;
                let (sign_node, number_node, value) = self.parse_coordinate()?;
                if sign_node.is_some() || value > 360.0 {
                    return Err(SyntaxError::InvalidArcRadius {
                        start: index_before_parse_radius,
                        end: self.index,
                        value,
                        command: u8_command as char,
                    });
                }

                segment.args.push(value);
                segment.cst.push(number_node);
                self.parse_comma_wsp(&mut segment.cst);

                self.parse_whitespaces(&mut next_nodes);

                // 3 - 4
                let value = self.parse_flag(u8_command)?;
                segment.args.push(value);
                segment.cst.push(SVGPathCSTNode::Number {
                    raw_number: if value == 0.0 {
                        "0".to_string()
                    } else {
                        "1".to_string()
                    },
                    value,
                    start: self.index - 1,
                    end: self.index,
                });
                self.parse_comma_wsp(&mut segment.cst);

                let value = self.parse_flag(u8_command)?;
                segment.args.push(value);
                segment.cst.push(SVGPathCSTNode::Number {
                    raw_number: if value == 0.0 {
                        "0".to_string()
                    } else {
                        "1".to_string()
                    },
                    value,
                    start: self.index - 1,
                    end: self.index,
                });
                self.parse_comma_wsp(&mut segment.cst);

                self.check_unexpected_end("coordinate pair")?;
                self.parse_coordinate_pair(&mut segment.cst, &mut segment.args)?;

                first_segment.chain_end = self.index;
                segment.end = self.index;
                next_nodes.push(SVGPathCSTNode::Segment(segment));
                self.parse_whitespaces(&mut next_nodes);
                self.parse_comma_wsp(&mut next_nodes);

                if let Some(n) = self.peek() {
                    next = n;
                } else {
                    break;
                }
            }
        }

        let (start, end) = (first_segment.chain_start, first_segment.chain_end);
        let mut cst = Vec::with_capacity(1 + next_nodes.len());
        cst.push(SVGPathCSTNode::Segment(first_segment));
        set_commands_chain_info!(cst, next_nodes, start, end);
        Ok(cst)
    }

    #[cfg_attr(feature = "tracing", tracing::instrument(level = "trace", skip_all))]
    fn parse_closepath(&mut self, command: &'static SVGPathCommand) -> SVGPathCSTNode {
        let start = self.index - 1;
        let end = self.index;
        SVGPathCSTNode::Segment(SVGPathSegment {
            command,
            args: Vec::with_capacity(0),
            cst: vec![SVGPathCSTNode::Command(command); 1],
            start,
            end,
            chained: false,
            chain_start: start,
            chain_end: end,
        })
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(level = "trace", skip_all, err(Debug))
    )]
    fn parse_horizontal_or_vertical(
        &mut self,
        command: &'static SVGPathCommand,
    ) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
        let mut first_segment = new_segment(command, self.index - 1, false);
        first_segment.cst.push(SVGPathCSTNode::Command(command));
        self.parse_whitespaces(&mut first_segment.cst);

        let (sign, number, value) = self.parse_coordinate()?;
        first_segment.args.push(value);
        if let Some(sign) = sign {
            first_segment.cst.push(sign);
        }
        first_segment.cst.push(number);
        first_segment.end = self.index;
        first_segment.chain_end = self.index;

        let mut next_nodes = Vec::new();
        self.parse_whitespaces(&mut next_nodes);

        if let Some(mut next) = self.peek() {
            while !is_command(next) {
                let mut segment = new_segment(command, self.index, true);
                let (sign, number, value) = self.parse_coordinate()?;
                segment.end = self.index;
                first_segment.chain_end = self.index;
                segment.cst.push(number);
                segment.args.push(value);
                if let Some(sign) = sign {
                    segment.cst.push(sign);
                }
                next_nodes.push(SVGPathCSTNode::Segment(segment));

                if let Some(n) = self.peek() {
                    next = n;
                } else {
                    break;
                }
            }
        }

        let (start, end) = (first_segment.chain_start, first_segment.chain_end);
        let mut cst = Vec::with_capacity(1 + next_nodes.len());
        cst.push(SVGPathCSTNode::Segment(first_segment));
        set_commands_chain_info!(cst, next_nodes, start, end);
        Ok(cst)
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(level = "trace", skip_all, err(Debug))
    )]
    pub fn parse(&mut self) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
        if self.path.is_empty() {
            #[cfg(feature = "tracing")]
            tracing::trace!("Empty SVG path");

            #[cfg(feature = "strict")]
            return Err(SyntaxError::UnexpectedEnding {
                expected: "moveto command",
                index: 0,
            });

            #[cfg(not(feature = "strict"))]
            return Ok(Vec::new());
        }

        if self.path == b"none" {
            #[cfg(feature = "tracing")]
            tracing::trace!("SVG path with 'none' value");

            #[cfg(feature = "strict")]
            return Err(SyntaxError::ExpectedMovetoCommand {
                character: 'n',
                index: 0,
            });

            #[cfg(not(feature = "strict"))]
            return Ok(Vec::from([SVGPathCSTNode::None]));
        }

        let mut nodes = Vec::with_capacity(self.path.len() / 4);
        self.parse_whitespaces(&mut nodes);
        let next = self.next().unwrap_or(b' ');
        match next {
            b'm' | b'M' => {
                nodes.extend(self.parse_two_operands_command(match next {
                    b'm' => &SVGPathCommand::MovetoLower,
                    _ => &SVGPathCommand::MovetoUpper,
                })?);
                self.parse_whitespaces(&mut nodes);
                while let Some(next) = self.next() {
                    match next {
                        b'm' => {
                            nodes.extend(self.parse_two_operands_command(
                                &SVGPathCommand::MovetoLower,
                            )?);
                        }
                        b'M' => {
                            nodes.extend(self.parse_two_operands_command(
                                &SVGPathCommand::MovetoUpper,
                            )?);
                        }
                        b'l' => {
                            nodes.extend(self.parse_two_operands_command(
                                &SVGPathCommand::LinetoLower,
                            )?);
                        }
                        b'L' => {
                            nodes.extend(self.parse_two_operands_command(
                                &SVGPathCommand::LinetoUpper,
                            )?);
                        }
                        b'h' => {
                            nodes.extend(self.parse_horizontal_or_vertical(
                                &SVGPathCommand::HorizontalLower,
                            )?);
                        }
                        b'H' => {
                            nodes.extend(self.parse_horizontal_or_vertical(
                                &SVGPathCommand::HorizontalUpper,
                            )?);
                        }
                        b'v' => {
                            nodes.extend(self.parse_horizontal_or_vertical(
                                &SVGPathCommand::VerticalLower,
                            )?);
                        }
                        b'V' => {
                            nodes.extend(self.parse_horizontal_or_vertical(
                                &SVGPathCommand::VerticalUpper,
                            )?);
                        }
                        b'z' => {
                            nodes.push(
                                self.parse_closepath(&SVGPathCommand::ClosepathLower),
                            );
                        }
                        b'Z' => {
                            nodes.push(
                                self.parse_closepath(&SVGPathCommand::ClosepathUpper),
                            );
                        }
                        b'c' => {
                            nodes.extend(
                                self.parse_curveto(&SVGPathCommand::CurvetoLower)?,
                            );
                        }
                        b'C' => {
                            nodes.extend(
                                self.parse_curveto(&SVGPathCommand::CurvetoUpper)?,
                            );
                        }
                        b'q' => {
                            nodes.extend(self.parse_four_operands_command(
                                &SVGPathCommand::QuadraticLower,
                            )?);
                        }
                        b'Q' => {
                            nodes.extend(self.parse_four_operands_command(
                                &SVGPathCommand::QuadraticUpper,
                            )?);
                        }
                        b's' => {
                            nodes.extend(self.parse_four_operands_command(
                                &SVGPathCommand::SmoothCurvetoLower,
                            )?);
                        }
                        b'S' => {
                            nodes.extend(self.parse_four_operands_command(
                                &SVGPathCommand::SmoothCurvetoUpper,
                            )?);
                        }
                        b'a' => {
                            nodes.extend(self.parse_arc(&SVGPathCommand::ArcLower)?);
                        }
                        b'A' => {
                            nodes.extend(self.parse_arc(&SVGPathCommand::ArcUpper)?);
                        }
                        b't' => {
                            nodes.extend(self.parse_two_operands_command(
                                &SVGPathCommand::SmoothQuadraticLower,
                            )?);
                        }
                        b'T' => {
                            nodes.extend(self.parse_two_operands_command(
                                &SVGPathCommand::SmoothQuadraticUpper,
                            )?);
                        }
                        _ => {
                            return Err(SyntaxError::InvalidCharacter {
                                character: next as char,
                                index: self.index - 1,
                                expected: "command",
                            })
                        }
                    }
                    self.parse_whitespaces(&mut nodes);
                }
            }
            b' ' => {
                #[cfg(all(feature = "tracing", feature = "strict"))]
                tracing::trace!("Empty SVG path");

                #[cfg(feature = "strict")]
                return Err(SyntaxError::ExpectedMovetoCommand {
                    character: self.path[0] as char,
                    index: 0,
                });
            }
            _ => {
                #[cfg(feature = "tracing")]
                tracing::trace!("Expected moveto command, found '{}'", next as char);
                return Err(SyntaxError::ExpectedMovetoCommand {
                    character: next as char,
                    index: self.index - 1,
                });
            }
        }

        Ok(nodes)
    }
}

/// Parse a SVG path string and returns a concrete syntax tree
///
/// # Example
///
/// Errors can be handled by matching the `SyntaxError` enum.
///
/// ```
/// use svg_path_cst::{svg_path_cst, SyntaxError as SVGPathSyntaxError};
///
/// let cst = svg_path_cst(b"M10 10!");
/// assert_eq!(
///     cst,
///     Err(SVGPathSyntaxError::InvalidCharacter {
///         character: '!',
///         index: 6,
///         expected: "number or command",
///     })
/// );
///
/// assert_eq!(
///     cst.unwrap_err().to_string(),
///     "Invalid character '!' at index 6, expected number or command"
/// );
/// ```
#[cfg_attr(feature = "tracing", tracing::instrument(level = "trace", skip_all))]
pub fn svg_path_cst(path: &[u8]) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
    #[cfg(feature = "tracing")]
    tracing::trace!("{:?}", &path.iter().map(|&c| c as char).collect::<String>());

    let mut parser = Parser::new(path);
    parser.parse()
}
