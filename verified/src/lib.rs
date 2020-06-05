pub mod bool;
pub use crate::bool::*;
pub use verify_macro::verify;

pub struct ForAll;

pub trait Same<T> {}
impl<T> Same<T> for T {}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
