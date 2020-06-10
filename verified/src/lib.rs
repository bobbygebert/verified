//! This crate, in conjunction with the `verify_macro` crate, aims to facilitate the development of
//! formally verifiable rust code.
//!
//! Type level programming allows us to implement logic that can be verified by the compiler, which
//! makes it possible to catch bugs at compile time, rather than at runtime.
//!
//! Say we have an algorithm who's runtime scales exponentially. We would like to be able to
//! restrict the number of elements in our working set to a reasonable number, let's say 128, in
//! order to ensure that the algorithm completes in a reasonable amount of time, every time.
//!
//! ```compile_fail
//! use verified::*;
//!
//! #[derive(Default)]
//! struct Collection<E, Size: Usize> {
//!     elements: Vec<E>,
//!     size: Size,
//! }
//!
//! #[verify]
//! fn really_really_really_slow_routine<E, Size: Usize>(working_set: Collection<E, Size>)
//! where
//!     // Restrict the size of the working set.
//!     _: Verify<{ Size < 128 }>
//! {
//!     todo! {}
//! }
//!
//! type U128 = U<U<U<U<U<U<U<U<T, B1>, B0>, B0>, B0>, B0>, B0>, B0>, B0>;
//! type U127 = U<U<U<U<U<U<U<T, B0>, B1>, B1>, B1>, B1>, B1>, B1>;
//!
//! fn main() {
//!     // No problem here...
//!     really_really_really_slow_routine::<String, U127>(Default::default());
//!
//!     // XXX: Does not compile because our working set is one element too large.
//!     really_really_really_slow_routine::<String, U128>(Default::default());
//! }
//! ```
pub mod bool;
pub mod ops;
pub mod usize;
pub use crate::bool::{Bool, False, True};
pub use crate::ops::{
    And, BitAnd, BitOr, BitXor, Compare, Equal, Ge, Greater, Gt, Le, Less, Lt, Ne, Not, Or,
    Ordering, Same,
};
pub use crate::usize::{Add, Usize, B0, B1, T, U};
pub use verify_macro::{verify, Literal};

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
