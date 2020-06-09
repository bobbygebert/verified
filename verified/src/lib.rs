pub mod bool;
pub mod usize;
pub use crate::bool::*;
pub use crate::usize::{Add, Equal, Ge, Greater, Gt, Le, Less, Lt, Ordering, Usize, B0, B1, T, U};
pub use verify_macro::verify;

pub struct ForAll;

pub trait Same<Rhs> {
    type Output: Bool;
}

pub trait Ne<Rhs> {
    type Output: Bool;
}

impl<Lhs, Rhs> Ne<Rhs> for Lhs
where
    Lhs: Same<Rhs>,
    <Lhs as Same<Rhs>>::Output: Not,
    <<Lhs as Same<Rhs>>::Output as Not>::Output: Bool,
{
    type Output = <<Lhs as Same<Rhs>>::Output as Not>::Output;
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
