//! Concrete Syntax Tree parser for SVG v1.1 paths.
//!
//! Provides all the necessary types and functions to parse a SVG path into a
//! concrete syntax tree (CST).
//!
//! # Example
//!
//! ```
//! use svg_path_cst::{
//!     svg_path_cst,
//!     SVGPathCSTNode,
//!     SVGPathSegment,
//!     SVGPathCommand,
//!     WSP,
//! };
//!
//! let path = "M 10 10 L 20 20";
//! let cst = svg_path_cst(path);
//! assert_eq!(
//!     cst,
//!     Ok(vec![
//!         SVGPathCSTNode::Segment(SVGPathSegment {
//!             command: &SVGPathCommand::MovetoUpper,
//!             args: vec![10.0, 10.0],
//!             cst: vec![
//!                 SVGPathCSTNode::Command(&SVGPathCommand::MovetoUpper),
//!                 SVGPathCSTNode::Whitespace(&WSP::Space),
//!                 SVGPathCSTNode::Number("10".to_string()),
//!                 SVGPathCSTNode::Whitespace(&WSP::Space),
//!                 SVGPathCSTNode::Number("10".to_string()),
//!             ],
//!             start: 0,
//!             end: 7,
//!             chained: false,
//!             chain_start: 0,
//!             chain_end: 7,
//!         }),
//!         SVGPathCSTNode::Whitespace(&WSP::Space),
//!         SVGPathCSTNode::Segment(SVGPathSegment {
//!             command: &SVGPathCommand::LinetoUpper,
//!             args: vec![20.0, 20.0],
//!             cst: vec![
//!                 SVGPathCSTNode::Command(&SVGPathCommand::LinetoUpper),
//!                 SVGPathCSTNode::Whitespace(&WSP::Space),
//!                 SVGPathCSTNode::Number("20".to_string()),
//!                 SVGPathCSTNode::Whitespace(&WSP::Space),
//!                 SVGPathCSTNode::Number("20".to_string()),
//!             ],
//!             start: 8,
//!             end: 15,
//!             chained: false,
//!             chain_start: 8,
//!             chain_end: 15,
//!         }),
//!     ])
//! );
//! ```

#[cfg(doctest)]
::doc_comment::doctest!("../README.md");

use std::iter::Peekable;
use std::str::Chars;
mod errors;
pub use errors::SyntaxError;

#[cfg(test)]
mod tests;

static COMMAND_CHARACTERS: [char; 20] = [
    'M', 'm', 'V', 'v', 'H', 'h', 'L', 'l', 'Z', 'z', 'C', 'c', 'S', 's', 'Q', 'q',
    'T', 't', 'A', 'a',
];
static WSP_COMMA_CHARACTERS: [char; 6] = [' ', '\t', '\n', '\x0C', '\r', ','];

// Sign, number and value
type Coordinate = (Option<SVGPathCSTNode>, SVGPathCSTNode, f64);

/// Whitespace token
///
/// Represents a whitespace character in the SVG path. The following characters
/// are considered whitespace following the v1.1 SVG Path specification:
///
/// - Space (U+0020)
/// - Tab (U+0009)
/// - Line Feed (U+000A)
/// - Form Feed (U+000C)
/// - Carriage Return (U+000D)
///
/// # Example
///
/// A path compound by whitespaces is considered a valid empty SVG path
/// according to the SVG Path v1.1 specification.
///
/// ```
/// use svg_path_cst::{svg_path_cst, SVGPathCSTNode, WSP};
///
/// let cst = svg_path_cst(" \t\n\r \x0C");
/// assert_eq!(
///     cst,
///     Ok(vec![
///         SVGPathCSTNode::Whitespace(&WSP::Space),
///         SVGPathCSTNode::Whitespace(&WSP::Tab),
///         SVGPathCSTNode::Whitespace(&WSP::LineFeed),
///         SVGPathCSTNode::Whitespace(&WSP::CarriageReturn),
///         SVGPathCSTNode::Whitespace(&WSP::Space),
///         SVGPathCSTNode::Whitespace(&WSP::FormFeed),
///     ])
/// );
///
/// for node in cst.unwrap() {
///     let SVGPathCSTNode::Whitespace(wsp) = node else { unreachable!() };
///     match wsp {
///         WSP::Space => println!(" "),
///         WSP::Tab => println!("\t"),
///         WSP::LineFeed => println!("\n"),
///         WSP::FormFeed => println!("\x0C"),
///         WSP::CarriageReturn => println!("\r"),
///     }
/// }
/// ```
#[derive(Debug, PartialEq, Clone)]
pub enum WSP {
    Space,
    Tab,
    LineFeed,
    FormFeed,
    CarriageReturn,
}

/// SVG path command
///
/// Represents a SVG path command. The following commands are supported:
///
/// - Moveto (M, m)
/// - Lineto (L, l)
/// - Horizontal Lineto (H, h)
/// - Vertical Lineto (V, v)
/// - Closepath (Z, z)
/// - Curveto (C, c)
/// - Smooth Curveto (S, s)
/// - Quadratic Bezier Curve (Q, q)
/// - Smooth Quadratic Bezier Curve (T, t)
/// - Elliptical Arc (A, a)
#[derive(Debug, PartialEq, Clone)]
pub enum SVGPathCommand {
    MovetoUpper,
    MovetoLower,
    LinetoUpper,
    LinetoLower,
    HorizontalUpper,
    HorizontalLower,
    VerticalUpper,
    VerticalLower,
    ClosepathUpper,
    ClosepathLower,
    CurvetoUpper,         // C
    CurvetoLower,         // c
    SmoothCurvetoUpper,   // S
    SmoothCurvetoLower,   // s
    ArcUpper,             // A
    ArcLower,             // a
    QuadraticUpper,       // Q
    QuadraticLower,       // q
    SmoothQuadraticUpper, // T
    SmoothQuadraticLower, // t
}

impl SVGPathCommand {
    fn capacity(&self) -> usize {
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
///                 SVGPathCSTNode::Sign(&Sign::Plus),
///                 SVGPathCSTNode::Number("10".to_string()),
///                 SVGPathCSTNode::Sign(&Sign::Minus),
///                 SVGPathCSTNode::Number("10".to_string()),
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
///         SVGPathCSTNode::Sign(sign) => {
///             match sign {
///                 Sign::Plus => println!("+"),
///                 Sign::Minus => println!("-"),
///             }
///         }
///         _ => (),
///     }
/// }
/// ```
#[derive(Debug, PartialEq, Clone)]
pub enum Sign {
    Plus,
    Minus,
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
    None,
    Whitespace(&'static WSP),
    Segment(SVGPathSegment),

    Sign(&'static Sign),
    Number(String),
    Comma,
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

impl SVGPathSegment {
    pub fn new(command: &'static SVGPathCommand, start: usize, chained: bool) -> Self {
        let capacity = command.capacity();
        Self {
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
                    whitespaces.push(SVGPathCSTNode::Whitespace(&WSP::Space));
                    self.next_char();
                }
                '\t' => {
                    whitespaces.push(SVGPathCSTNode::Whitespace(&WSP::Tab));
                    self.next_char();
                }
                '\n' => {
                    whitespaces.push(SVGPathCSTNode::Whitespace(&WSP::LineFeed));
                    self.next_char();
                }
                '\x0C' => {
                    whitespaces.push(SVGPathCSTNode::Whitespace(&WSP::FormFeed));
                    self.next_char();
                }
                '\r' => {
                    whitespaces.push(SVGPathCSTNode::Whitespace(&WSP::CarriageReturn));
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
                self.next_char();
                comma_wsp.push(SVGPathCSTNode::Comma);
                comma_wsp.extend(self.parse_whitespaces());
            } else {
                comma_wsp.extend(self.parse_whitespaces());
                if let Some(next_after_wsp) = self.chars.peek() {
                    if *next_after_wsp == ',' {
                        comma_wsp.push(SVGPathCSTNode::Comma);
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

    fn parse_number(&mut self) -> Result<(SVGPathCSTNode, f64), SyntaxError> {
        let mut number = String::new();
        let mut has_dot = false;
        let mut has_e = false;
        let mut has_sign = false;
        let mut has_digit = false;
        while let Some(next) = self.chars.peek() {
            match next {
                '0'..='9' => {
                    number.push(*next);
                    has_digit = true;
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
                            number: number.clone(),
                            index: self.index,
                        });
                    }
                    number.push(*next);
                    has_e = true;
                }
                '+' | '-' => {
                    if has_sign {
                        return Err(SyntaxError::InvalidNumber {
                            number: number.clone(),
                            index: self.index,
                        });
                    }
                    if !has_e {
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
                number: number.clone(),
                index: self.index,
            });
        }

        match number.parse::<f64>() {
            Ok(value) => Ok((SVGPathCSTNode::Number(number), value)),
            Err(_) => Err(SyntaxError::InvalidNumber {
                number: number.clone(),
                index: self.index,
            }),
        }
    }

    fn parse_sign(&mut self) -> Option<SVGPathCSTNode> {
        if let Some(next) = self.chars.peek() {
            match next {
                '+' => {
                    self.next_char();
                    return Some(SVGPathCSTNode::Sign(&Sign::Plus));
                }
                '-' => {
                    self.next_char();
                    return Some(SVGPathCSTNode::Sign(&Sign::Minus));
                }
                _ => return None,
            }
        }
        None
    }

    fn parse_coordinate(&mut self) -> Result<Coordinate, SyntaxError> {
        let sign_node = self.parse_sign();
        let (number_node, number_value) = self.parse_number()?;

        let mut value = number_value;
        if let Some(SVGPathCSTNode::Sign(sign)) = &sign_node {
            value = match sign {
                Sign::Plus => number_value,
                Sign::Minus => -number_value,
            };
        }

        Ok((sign_node, number_node, value))
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
        let mut first_segment = SVGPathSegment::new(command, self.index - 1, false);
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
                let mut segment = SVGPathSegment::new(command, self.index, true);
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
        let mut first_segment = SVGPathSegment::new(command, self.index - 1, false);
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
                let mut segment = SVGPathSegment::new(command, self.index, true);
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
        let mut first_segment = SVGPathSegment::new(command, self.index - 1, false);
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
                let mut segment = SVGPathSegment::new(command, self.index, true);
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
        let mut first_segment = SVGPathSegment::new(command, self.index - 1, false);
        first_segment.cst.push(SVGPathCSTNode::Command(command));
        first_segment.cst.extend(self.parse_whitespaces());

        for _ in 0..3 {
            self.check_unexpected_end("number")?;
            let (number_node, number_value) = self.parse_number()?;
            first_segment.args.push(number_value);
            first_segment.cst.push(number_node);
            first_segment.cst.extend(self.parse_comma_wsp()?);
        }

        for _ in 0..2 {
            self.check_unexpected_end("arc flag (0 or 1)")?;
            let (number_node, number_value) = self.parse_number()?;
            if number_value != 0.0 && number_value != 1.0 {
                return Err(SyntaxError::InvalidArcFlag {
                    index: self.index,
                    value: number_value,
                    command: match command {
                        SVGPathCommand::ArcLower => 'a',
                        _ => 'A',
                    },
                });
            }

            first_segment.args.push(number_value);
            first_segment.cst.push(number_node);
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
                let mut segment = SVGPathSegment::new(command, self.index, true);

                for _ in 0..3 {
                    next_nodes.extend(self.parse_whitespaces());
                    self.check_unexpected_end("number")?;
                    let (number_node, number_value) = self.parse_number()?;
                    segment.args.push(number_value);
                    segment.cst.push(number_node);
                    segment.cst.extend(self.parse_comma_wsp()?);
                }

                for _ in 0..2 {
                    next_nodes.extend(self.parse_whitespaces());
                    self.check_unexpected_end("arc flag (0 or 1)")?;
                    let (number_node, number_value) = self.parse_number()?;
                    if number_value != 0.0 && number_value != 1.0 {
                        return Err(SyntaxError::InvalidArcFlag {
                            index: self.index,
                            value: number_value,
                            command: match segment.command {
                                SVGPathCommand::ArcLower => 'a',
                                _ => 'A',
                            },
                        });
                    }

                    segment.args.push(number_value);
                    segment.cst.push(number_node);
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
        let mut segment = SVGPathSegment::new(command, self.index - 1, false);
        segment.end = self.index;
        segment.chain_end = self.index;
        segment.cst.push(SVGPathCSTNode::Command(command));
        vec![SVGPathCSTNode::Segment(segment)]
    }

    fn parse_horizontal_or_vertical(
        &mut self,
        command: &'static SVGPathCommand,
    ) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
        let mut first_segment = SVGPathSegment::new(command, self.index - 1, false);
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
                let mut segment = SVGPathSegment::new(command, self.index, true);
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
/// use svg_path_cst::{
///     svg_path_cst,
///     SVGPathCSTNode,
///     SyntaxError as SVGPathSyntaxError,
/// };
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
