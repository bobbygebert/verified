pub mod bool;
pub use crate::bool::*;
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
