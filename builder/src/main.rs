#![allow(dead_code)]
use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
    executable: String,
    args: std::string::String,
    env: Result<String, usize>,
    current_dir: Option<String>,
    // foo: <Vec<Option<bool>> as IntoIterator>::Item,
}

pub fn main() {}
