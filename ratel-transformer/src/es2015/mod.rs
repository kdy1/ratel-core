mod arrow;
mod classes;

pub use self::{arrow::TransformArrow, classes::TransformClass};

pub type PresetES2015<'ast> = TransformArrow<'ast>;
