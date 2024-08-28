use crate::{svg_path_cst, SyntaxError};

#[cfg_attr(feature = "strict", allow(dead_code))]
pub(crate) fn assert_svg_path_cst(path: &[u8], expected: Vec<crate::SVGPathCSTNode>) {
    assert_eq!(svg_path_cst(path), Ok(expected));
}

#[cfg_attr(feature = "strict", allow(dead_code))]
pub(crate) fn assert_svg_path_cst_fmt(path: &[u8], expected: &str) {
    let cst = svg_path_cst(path);
    assert!(cst.is_ok());
    assert_eq!(format!("{:?}", cst.unwrap()), expected);
}

pub(crate) fn assert_svg_path_cst_err(path: &[u8], expected: SyntaxError) {
    assert_eq!(svg_path_cst(path), Err(expected));
}
