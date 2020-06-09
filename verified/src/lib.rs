pub mod bool;
pub mod usize;
pub use crate::bool::*;
pub use crate::usize::{Add, Usize, B0, B1, T, U};
pub use verify_macro::verify;

pub struct ForAll;

pub trait Same<Rhs> {
    type Output: Bool;
}
impl<Rhs> Same<Rhs> for Rhs {
    type Output = True;
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
