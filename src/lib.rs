#![doc = include_str!("../README.md")]
#![warn(missing_docs)]
#![doc(test(attr(deny(warnings))))]

#[cfg(doctest)]
::doc_comment::doctest!("../README.md");

#[cfg(test)]
mod tests;

use std::iter::Peekable;
use std::str::Chars;
mod errors;
pub use errors::SyntaxError;

static COMMAND_CHARACTERS: [char; 20] = [
    'M', 'm', 'V', 'v', 'H', 'h', 'L', 'l', 'Z', 'z', 'C', 'c', 'S', 's', 'Q', 'q',
    'T', 't', 'A', 'a',
];
static WSP_COMMA_CHARACTERS: [char; 6] = [' ', '\t', '\n', '\x0C', '\r', ','];

// Sign, number and value
type Coordinate = (Option<SVGPathCSTNode>, SVGPathCSTNode, f64);

/// SVG path command
#[derive(Debug, PartialEq, Clone)]
pub enum SVGPathCommand {
    /// M
    MovetoUpper,
    /// m
    MovetoLower,
    /// L
    LinetoUpper,
    /// l
    LinetoLower,
    /// H
    HorizontalUpper,
    /// h
    HorizontalLower,
    /// V
    VerticalUpper,
    /// v
    VerticalLower,
    /// Z
    ClosepathUpper,
    /// z
    ClosepathLower,
    /// C
    CurvetoUpper,
    /// c
    CurvetoLower,
    /// S
    SmoothCurvetoUpper,
    /// s
    SmoothCurvetoLower,
    /// A
    ArcUpper,
    /// a
    ArcLower,
    /// Q
    QuadraticUpper,
    /// q
    QuadraticLower,
    /// T
    SmoothQuadraticUpper,
    /// t
    SmoothQuadraticLower,
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

    /// Returns the SVG path command character
    pub fn to_char(&self) -> char {
        match self {
            SVGPathCommand::MovetoUpper => 'M',
            SVGPathCommand::MovetoLower => 'm',
            SVGPathCommand::LinetoUpper => 'L',
            SVGPathCommand::LinetoLower => 'l',
            SVGPathCommand::HorizontalUpper => 'H',
            SVGPathCommand::HorizontalLower => 'h',
            SVGPathCommand::VerticalUpper => 'V',
            SVGPathCommand::VerticalLower => 'v',
            SVGPathCommand::ClosepathUpper => 'Z',
            SVGPathCommand::ClosepathLower => 'z',
            SVGPathCommand::CurvetoUpper => 'C',
            SVGPathCommand::CurvetoLower => 'c',
            SVGPathCommand::SmoothCurvetoUpper => 'S',
            SVGPathCommand::SmoothCurvetoLower => 's',
            SVGPathCommand::ArcUpper => 'A',
            SVGPathCommand::ArcLower => 'a',
            SVGPathCommand::QuadraticUpper => 'Q',
            SVGPathCommand::QuadraticLower => 'q',
            SVGPathCommand::SmoothQuadraticUpper => 'T',
            SVGPathCommand::SmoothQuadraticLower => 't',
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
/// use svg_path_cst::{svg_path_cst, SVGPathCSTNode, WSP};
///
/// let cst = svg_path_cst(" \t\n\r \x0C");
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
///             println!("\"{}\" at index {}", wsp.to_char(), start);
///         },
///         _ => (),
///     }
/// }
/// ```
#[derive(Debug, PartialEq, Clone)]
pub enum WSP {
    /// Space (U+0020)
    Space,
    /// Tab (U+0009)
    Tab,
    /// Line Feed (U+000A)
    LineFeed,
    /// Form Feed (U+000C)
    FormFeed,
    /// Carriage Return (U+000D)
    CarriageReturn,
}

impl WSP {
    /// Returns the whitespace character
    pub fn to_char(&self) -> char {
        match self {
            WSP::Space => ' ',
            WSP::Tab => '\t',
            WSP::LineFeed => '\n',
            WSP::FormFeed => '\x0C',
            WSP::CarriageReturn => '\r',
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
/// let cst = svg_path_cst("M+10-10");
/// assert_eq!(
///     cst,
///     Ok(vec![
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
///     ])
/// );
///
/// for node in cst.unwrap() {
///     match node {
///         SVGPathCSTNode::Sign{sign, ..} => {
///             match sign {
///                Sign::Plus => println!("+"),
///                Sign::Minus => println!("-"),
///             }
///             // or just println!("{}", sign.to_char());`
///         }
///         _ => (),
///     }
/// }
/// ```
#[derive(Debug, PartialEq, Clone)]
pub enum Sign {
    /// Plus sign (U+002B)
    Plus,
    /// Minus sign (U+002D)
    Minus,
}

impl Sign {
    /// Returns the sign character
    pub fn to_char(&self) -> char {
        match self {
            Sign::Plus => '+',
            Sign::Minus => '-',
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
        cst: Vec::with_capacity(capacity * 2),
        start,
        end: start,
        chained,
        chain_start: start,
        chain_end: 0,
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
    path: &'a str,
    chars: Peekable<Chars<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(path: &'a str) -> Self {
        Self {
            index: 0,
            path,
            chars: path.chars().peekable(),
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.index += 1;
        self.chars.next()
    }

    fn check_unexpected_end(&mut self, expected: &str) -> Result<(), SyntaxError> {
        if self.chars.peek().is_none() {
            return Err(SyntaxError::UnexpectedEnding {
                index: self.index - 1,
                expected: expected.to_string(),
            });
        }
        Ok(())
    }

    fn parse_whitespaces(&mut self) -> Vec<SVGPathCSTNode> {
        let mut whitespaces = vec![];
        while let Some(next) = self.chars.peek() {
            match next {
                ' ' => {
                    whitespaces.push(SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Space,
                        start: self.index,
                        end: self.index + 1,
                    });
                    self.next_char();
                }
                '\t' => {
                    whitespaces.push(SVGPathCSTNode::Whitespace {
                        wsp: &WSP::Tab,
                        start: self.index,
                        end: self.index + 1,
                    });
                    self.next_char();
                }
                '\n' => {
                    whitespaces.push(SVGPathCSTNode::Whitespace {
                        wsp: &WSP::LineFeed,
                        start: self.index,
                        end: self.index + 1,
                    });
                    self.next_char();
                }
                '\x0C' => {
                    whitespaces.push(SVGPathCSTNode::Whitespace {
                        wsp: &WSP::FormFeed,
                        start: self.index,
                        end: self.index + 1,
                    });
                    self.next_char();
                }
                '\r' => {
                    whitespaces.push(SVGPathCSTNode::Whitespace {
                        wsp: &WSP::CarriageReturn,
                        start: self.index,
                        end: self.index + 1,
                    });
                    self.next_char();
                }
                _ => break,
            }
        }
        whitespaces
    }

    fn parse_comma_wsp(&mut self) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
        let mut comma_wsp = vec![];
        if let Some(next) = self.chars.peek() {
            if *next == ',' {
                comma_wsp.push(SVGPathCSTNode::Comma { start: self.index });
                self.next_char();
                comma_wsp.extend(self.parse_whitespaces());
            } else {
                comma_wsp.extend(self.parse_whitespaces());
                if let Some(next_after_wsp) = self.chars.peek() {
                    if *next_after_wsp == ',' {
                        comma_wsp.push(SVGPathCSTNode::Comma { start: self.index });
                        self.next_char();
                        comma_wsp.extend(self.parse_whitespaces());
                    }
                }
            }
        } else {
            return Err(SyntaxError::UnexpectedEnding {
                index: self.index - 1,
                expected: "comma or whitespace".to_string(),
            });
        }

        Ok(comma_wsp)
    }

    fn parse_number(&mut self) -> Result<SVGPathCSTNode, SyntaxError> {
        let mut number = String::new();
        let mut processed = String::new();
        let mut has_dot = false;
        let mut has_e = false;
        let mut has_sign = false;
        let mut has_digit = false;
        let start = self.index;

        while let Some(next) = self.chars.peek() {
            processed.push(*next);
            match next {
                '0' => {
                    if number == "0" {
                        break;
                    }
                    has_digit = true;
                    number.push(*next);
                }
                '1'..='9' => {
                    has_digit = true;
                    number.push(*next);
                }
                '.' => {
                    if has_dot {
                        break;
                    }
                    number.push(*next);
                    has_dot = true;
                }
                'e' | 'E' => {
                    if has_e {
                        return Err(SyntaxError::InvalidNumber {
                            number: processed.clone(),
                            start,
                            end: start + processed.clone().len(),
                        });
                    }
                    number.push(*next);
                    has_e = true;
                }
                '+' | '-' => {
                    if has_sign || has_dot || has_digit && !has_e {
                        break;
                    }
                    number.push(*next);
                    has_sign = true;
                }
                _ => {
                    if !WSP_COMMA_CHARACTERS.contains(next)
                        && !COMMAND_CHARACTERS.contains(next)
                    {
                        return Err(SyntaxError::InvalidCharacter {
                            character: *next,
                            index: self.index,
                            expected: "number or command".to_string(),
                        });
                    }
                    break;
                }
            }
            self.next_char();
        }

        if !has_digit {
            return Err(SyntaxError::InvalidNumber {
                number: processed.clone(),
                start,
                end: start + processed.clone().len(),
            });
        }

        match number.parse::<f64>() {
            Ok(value) => Ok(SVGPathCSTNode::Number {
                raw_number: number,
                value,
                start,
                end: self.index,
            }),
            Err(_) => Err(SyntaxError::InvalidNumber {
                number: processed.clone(),
                start,
                end: start + processed.clone().len(),
            }),
        }
    }

    fn parse_sign(&mut self) -> Option<SVGPathCSTNode> {
        if let Some(next) = self.chars.peek() {
            match next {
                '+' => {
                    self.next_char();
                    return Some(SVGPathCSTNode::Sign {
                        sign: &Sign::Plus,
                        start: self.index - 1,
                    });
                }
                '-' => {
                    self.next_char();
                    return Some(SVGPathCSTNode::Sign {
                        sign: &Sign::Minus,
                        start: self.index - 1,
                    });
                }
                _ => return None,
            }
        }
        None
    }

    fn parse_flag(&mut self, command: char) -> Result<f64, SyntaxError> {
        match self.next_char() {
            Some('0') => Ok(0.0),
            Some('1') => Ok(1.0),
            Some(character) => Err(SyntaxError::InvalidArcFlag {
                index: self.index - 1,
                character,
                command,
            }),
            None => Err(SyntaxError::UnexpectedEnding {
                index: self.index,
                expected: "flag (0 or 1)".to_string(),
            }),
        }
    }

    fn parse_coordinate(&mut self) -> Result<Coordinate, SyntaxError> {
        let sign_node = self.parse_sign();
        let SVGPathCSTNode::Number {
            value,
            start,
            end,
            raw_number,
        } = self.parse_number()?
        else {
            unreachable!()
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

    fn parse_coordinate_pair(
        &mut self,
    ) -> Result<(Vec<SVGPathCSTNode>, (f64, f64)), SyntaxError> {
        let mut nodes = vec![];

        let (first_sign, first_number, first_value) = self.parse_coordinate()?;
        if let Some(sign) = first_sign {
            nodes.push(sign);
        }
        nodes.push(first_number);
        nodes.extend(self.parse_comma_wsp()?);
        let (second_sign, second_number, second_value) = self.parse_coordinate()?;
        if let Some(sign) = second_sign {
            nodes.push(sign);
        }
        nodes.push(second_number);

        Ok((nodes, (first_value, second_value)))
    }

    fn parse_two_operands_command(
        &mut self,
        command: &'static SVGPathCommand,
    ) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
        let mut first_segment = new_segment(command, self.index - 1, false);
        first_segment.cst.push(SVGPathCSTNode::Command(command));
        first_segment.cst.extend(self.parse_whitespaces());

        let (coord_nodes, coord_values) = self.parse_coordinate_pair()?;
        first_segment.args.push(coord_values.0);
        first_segment.args.push(coord_values.1);
        first_segment.cst.extend(coord_nodes);
        first_segment.end = self.index;
        first_segment.chain_end = self.index;

        let mut next_nodes = self.parse_whitespaces();

        if let Some(mut next) = self.chars.peek() {
            while !COMMAND_CHARACTERS.contains(next) {
                let mut segment = new_segment(command, self.index, true);
                let (coord_nodes, coord_values) = self.parse_coordinate_pair()?;
                first_segment.chain_end = self.index;
                segment.end = self.index;
                segment.cst.extend(coord_nodes);
                segment.args.push(coord_values.0);
                segment.args.push(coord_values.1);
                next_nodes.push(SVGPathCSTNode::Segment(segment));

                match self.parse_comma_wsp() {
                    Ok(comma_wsp) => next_nodes.extend(comma_wsp),
                    Err(_) => break,
                }
                if let Some(next_) = self.chars.peek() {
                    next = next_;
                } else {
                    break;
                }
            }
        }

        let (start, end) = (first_segment.chain_start, first_segment.chain_end);
        let mut cst = vec![SVGPathCSTNode::Segment(first_segment)];
        set_commands_chain_info!(cst, next_nodes, start, end);
        Ok(cst)
    }

    fn parse_four_operands_command(
        &mut self,
        command: &'static SVGPathCommand,
    ) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
        let mut first_segment = new_segment(command, self.index - 1, false);
        first_segment.cst.push(SVGPathCSTNode::Command(command));

        for _ in 0..2 {
            first_segment.cst.extend(self.parse_whitespaces());
            self.check_unexpected_end("coordinate pair")?;
            let (coord_nodes, coord_values) = self.parse_coordinate_pair()?;
            first_segment.args.push(coord_values.0);
            first_segment.args.push(coord_values.1);
            first_segment.cst.extend(coord_nodes);
        }

        first_segment.chain_end = self.index;
        first_segment.end = self.index;

        let mut next_nodes = self.parse_whitespaces();

        if let Some(mut next) = self.chars.peek() {
            while !COMMAND_CHARACTERS.contains(next) {
                let mut segment = new_segment(command, self.index, true);
                let (coord_nodes_1, coord_values_1) = self.parse_coordinate_pair()?;
                segment.cst.extend(coord_nodes_1);
                segment.args.push(coord_values_1.0);
                segment.args.push(coord_values_1.1);
                segment.cst.extend(self.parse_whitespaces());
                self.check_unexpected_end("coordinate pair")?;

                let (coord_nodes_2, coord_values_2) = self.parse_coordinate_pair()?;
                segment.cst.extend(coord_nodes_2);
                segment.args.push(coord_values_2.0);
                segment.args.push(coord_values_2.1);

                first_segment.chain_end = self.index;
                segment.end = self.index;
                next_nodes.push(SVGPathCSTNode::Segment(segment));
                if let Some(next_) = self.chars.peek() {
                    next = next_;
                } else {
                    break;
                }
            }
        }

        let (start, end) = (first_segment.chain_start, first_segment.chain_end);
        let mut cst = vec![SVGPathCSTNode::Segment(first_segment)];
        set_commands_chain_info!(cst, next_nodes, start, end);
        Ok(cst)
    }

    fn parse_curveto(
        &mut self,
        command: &'static SVGPathCommand,
    ) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
        let mut first_segment = new_segment(command, self.index - 1, false);
        first_segment.cst.push(SVGPathCSTNode::Command(command));

        for _ in 0..3 {
            first_segment.cst.extend(self.parse_whitespaces());
            self.check_unexpected_end("coordinate pair")?;
            let (coord_nodes, coord_values) = self.parse_coordinate_pair()?;
            first_segment.args.push(coord_values.0);
            first_segment.args.push(coord_values.1);
            first_segment.cst.extend(coord_nodes);
        }

        first_segment.chain_end = self.index;
        first_segment.end = self.index;

        let mut next_nodes = self.parse_whitespaces();

        if let Some(mut next) = self.chars.peek() {
            while !COMMAND_CHARACTERS.contains(next) {
                let mut segment = new_segment(command, self.index, true);
                let (coord_nodes_1, coord_values_1) = self.parse_coordinate_pair()?;
                segment.cst.extend(coord_nodes_1);
                segment.args.push(coord_values_1.0);
                segment.args.push(coord_values_1.1);
                segment.cst.extend(self.parse_whitespaces());

                self.check_unexpected_end("coordinate pair")?;
                let (coord_nodes_2, coord_values_2) = self.parse_coordinate_pair()?;
                segment.cst.extend(coord_nodes_2);
                segment.args.push(coord_values_2.0);
                segment.args.push(coord_values_2.1);
                segment.cst.extend(self.parse_whitespaces());

                self.check_unexpected_end("coordinate pair")?;
                let (coord_nodes_3, coord_values_3) = self.parse_coordinate_pair()?;
                segment.cst.extend(coord_nodes_3);
                segment.args.push(coord_values_3.0);
                segment.args.push(coord_values_3.1);

                first_segment.chain_end = self.index;
                segment.end = self.index;
                next_nodes.push(SVGPathCSTNode::Segment(segment));
                next_nodes.extend(self.parse_whitespaces());
                if let Some(next_) = self.chars.peek() {
                    next = next_;
                } else {
                    break;
                }
            }
        }

        let (start, end) = (first_segment.chain_start, first_segment.chain_end);
        let mut cst = vec![SVGPathCSTNode::Segment(first_segment)];
        set_commands_chain_info!(cst, next_nodes, start, end);
        Ok(cst)
    }

    fn parse_arc(
        &mut self,
        command: &'static SVGPathCommand,
    ) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
        let command_as_char = command.to_char();
        let mut first_segment = new_segment(command, self.index - 1, false);
        first_segment.cst.push(SVGPathCSTNode::Command(command));
        first_segment.cst.extend(self.parse_whitespaces());

        for _ in 0..3 {
            self.check_unexpected_end("number")?;
            let index_before_parse_radius = self.index;
            let (sign_node, number_node, value) = self.parse_coordinate()?;
            if sign_node.is_some() || value > 360.0 {
                return Err(SyntaxError::InvalidArcRadius {
                    start: index_before_parse_radius,
                    end: self.index,
                    value,
                    command: command_as_char,
                });
            }
            first_segment.args.push(value);
            first_segment.cst.push(number_node);
            first_segment.cst.extend(self.parse_comma_wsp()?);
        }

        for _ in 0..2 {
            let value = self.parse_flag(command_as_char)?;
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
            first_segment.cst.extend(self.parse_comma_wsp()?);
        }

        let (coord_nodes, coord_values) = self.parse_coordinate_pair()?;
        first_segment.args.push(coord_values.0);
        first_segment.args.push(coord_values.1);
        first_segment.cst.extend(coord_nodes);

        first_segment.chain_end = self.index;
        first_segment.end = self.index;

        let mut next_nodes = self.parse_whitespaces();

        if let Some(mut next) = self.chars.peek() {
            while !COMMAND_CHARACTERS.contains(next) {
                let mut segment = new_segment(command, self.index, true);

                for _ in 0..3 {
                    next_nodes.extend(self.parse_whitespaces());
                    self.check_unexpected_end("number")?;

                    let index_before_parse_radius = self.index;
                    let (sign_node, number_node, value) = self.parse_coordinate()?;
                    if sign_node.is_some() || value > 360.0 {
                        return Err(SyntaxError::InvalidArcRadius {
                            start: index_before_parse_radius,
                            end: self.index,
                            value,
                            command: command_as_char,
                        });
                    }

                    segment.args.push(value);
                    segment.cst.push(number_node);
                    segment.cst.extend(self.parse_comma_wsp()?);
                }

                for _ in 0..2 {
                    next_nodes.extend(self.parse_whitespaces());

                    let value = self.parse_flag(command_as_char)?;
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
                    segment.cst.extend(self.parse_comma_wsp()?);
                }

                self.check_unexpected_end("coordinate pair")?;
                let (coord_nodes, coord_values) = self.parse_coordinate_pair()?;
                segment.args.push(coord_values.0);
                segment.args.push(coord_values.1);
                segment.cst.extend(coord_nodes);

                first_segment.chain_end = self.index;
                segment.end = self.index;
                next_nodes.push(SVGPathCSTNode::Segment(segment));
                if let Some(next_) = self.chars.peek() {
                    next = next_;
                } else {
                    break;
                }
            }
        }

        let (start, end) = (first_segment.chain_start, first_segment.chain_end);
        let mut cst = vec![SVGPathCSTNode::Segment(first_segment)];
        set_commands_chain_info!(cst, next_nodes, start, end);
        Ok(cst)
    }

    fn parse_closepath(
        &mut self,
        command: &'static SVGPathCommand,
    ) -> Vec<SVGPathCSTNode> {
        let mut segment = new_segment(command, self.index - 1, false);
        segment.end = self.index;
        segment.chain_end = self.index;
        segment.cst.push(SVGPathCSTNode::Command(command));
        vec![SVGPathCSTNode::Segment(segment)]
    }

    fn parse_horizontal_or_vertical(
        &mut self,
        command: &'static SVGPathCommand,
    ) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
        let mut first_segment = new_segment(command, self.index - 1, false);
        first_segment.cst.push(SVGPathCSTNode::Command(command));
        first_segment.cst.extend(self.parse_whitespaces());

        let (sign, number, value) = self.parse_coordinate()?;
        first_segment.args.push(value);
        if let Some(sign) = sign {
            first_segment.cst.push(sign);
        }
        first_segment.cst.push(number);
        first_segment.end = self.index;
        first_segment.chain_end = self.index;

        let mut next_nodes = self.parse_whitespaces();

        if let Some(mut next) = self.chars.peek() {
            while !COMMAND_CHARACTERS.contains(next) {
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

                match self.parse_comma_wsp() {
                    Ok(comma_wsp) => next_nodes.extend(comma_wsp),
                    Err(_) => break,
                }
                if let Some(next_) = self.chars.peek() {
                    next = next_;
                } else {
                    break;
                }
            }
        }

        let (start, end) = (first_segment.chain_start, first_segment.chain_end);
        let mut cst = vec![SVGPathCSTNode::Segment(first_segment)];
        set_commands_chain_info!(cst, next_nodes, start, end);
        Ok(cst)
    }

    fn parse_drawto(
        &mut self,
        command: char,
    ) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
        match command {
            'm' => self.parse_two_operands_command(&SVGPathCommand::MovetoLower),
            'M' => self.parse_two_operands_command(&SVGPathCommand::MovetoUpper),
            'l' => self.parse_two_operands_command(&SVGPathCommand::LinetoLower),
            'L' => self.parse_two_operands_command(&SVGPathCommand::LinetoUpper),
            'h' => self.parse_horizontal_or_vertical(&SVGPathCommand::HorizontalLower),
            'H' => self.parse_horizontal_or_vertical(&SVGPathCommand::HorizontalUpper),
            'v' => self.parse_horizontal_or_vertical(&SVGPathCommand::VerticalLower),
            'V' => self.parse_horizontal_or_vertical(&SVGPathCommand::VerticalUpper),
            'z' => Ok(self.parse_closepath(&SVGPathCommand::ClosepathLower)),
            'Z' => Ok(self.parse_closepath(&SVGPathCommand::ClosepathUpper)),
            'c' => self.parse_curveto(&SVGPathCommand::CurvetoLower),
            'C' => self.parse_curveto(&SVGPathCommand::CurvetoUpper),
            'q' => self.parse_four_operands_command(&SVGPathCommand::QuadraticLower),
            'Q' => self.parse_four_operands_command(&SVGPathCommand::QuadraticUpper),
            's' => {
                self.parse_four_operands_command(&SVGPathCommand::SmoothCurvetoLower)
            }
            'S' => {
                self.parse_four_operands_command(&SVGPathCommand::SmoothCurvetoUpper)
            }
            'a' => self.parse_arc(&SVGPathCommand::ArcLower),
            'A' => self.parse_arc(&SVGPathCommand::ArcUpper),
            't' => {
                self.parse_two_operands_command(&SVGPathCommand::SmoothQuadraticLower)
            }
            'T' => {
                self.parse_two_operands_command(&SVGPathCommand::SmoothQuadraticUpper)
            }
            _ => Err(SyntaxError::InvalidCharacter {
                character: command,
                index: self.index - 1,
                expected: "command".to_string(),
            }),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
        if self.path.is_empty() {
            return Ok(vec![]);
        }
        if self.path == "none" {
            return Ok(vec![SVGPathCSTNode::None]);
        }

        let mut cst = self.parse_whitespaces();
        if self.chars.peek().is_some() {
            let next = self.next_char().unwrap();
            if next == 'M' || next == 'm' {
                cst.extend(self.parse_two_operands_command(match next {
                    'M' => &SVGPathCommand::MovetoUpper,
                    _ => &SVGPathCommand::MovetoLower,
                })?);
                cst.extend(self.parse_whitespaces());
                while let Some(next) = self.next_char() {
                    cst.extend(self.parse_drawto(next)?);
                    cst.extend(self.parse_whitespaces());
                }
            } else {
                return Err(SyntaxError::ExpectedMovetoCommand {
                    command: next,
                    index: self.index - 1,
                });
            }
        }

        Ok(cst)
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
/// let cst = svg_path_cst("M10 10!");
/// assert_eq!(
///     cst,
///     Err(SVGPathSyntaxError::InvalidCharacter {
///         character: '!',
///         index: 6,
///         expected: "number or command".to_string(),
///     })
/// );
///
/// assert_eq!(
///     cst.unwrap_err().to_string(),
///     "Invalid character '!' at index 6, expected number or command"
/// );
/// ```
pub fn svg_path_cst(path: &str) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
    let mut parser = Parser::new(path);
    parser.parse()
}
