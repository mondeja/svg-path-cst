mod docs;
pub(in crate::tests) mod helpers;

#[cfg(not(feature = "strict"))]
mod no_features;

#[cfg(feature = "strict")]
mod strict;
