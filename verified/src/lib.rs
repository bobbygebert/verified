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
//! struct Collection<E, Size: Unsigned> {
//!     elements: Vec<E>,
//!     size: Size,
//! }
//!
//! #[verify]
//! fn slow_routine<E, Size: Unsigned>(working_set: Collection<E, Size>)
//! where
//!     // Restrict the size of the working set.
//!     _: Verify<{ Size < 128 }, { Size > 0 }>
//! {
//!     // TODO
//! }
//!
//! fn main() {
//!     // No problem here...
//!     slow_routine::<String, U1>(Default::default());
//!     slow_routine::<String, U127>(Default::default());
//!
//!     // XXX: Does not compile because our working set is empty.
//!     slow_routine::<String, U0>(Default::default());
//!
//!     // XXX: Does not compile because our working set is one element too large.
//!     slow_routine::<String, U128>(Default::default());
//! }
//! ```
//!
//! For a more complete example, see the `vec` module. Here is an abbreviated snippet:
//!
//! ```
//! use verified::*;
//! use std::vec::Vec as Raw;
//!
//! pub struct Vec<Size: Unsigned, Element>(Size, Raw<Element>);
//!
//! #[verify]
//! impl<Size: Unsigned, Element> Vec<Size, Element> {
//!     pub fn append<OtherSize: Unsigned>(
//!         self,
//!         other: Vec<OtherSize, Element>,
//!     ) -> Vec<{ Size + OtherSize }, Element> {
//!         self + other
//!     }
//!
//!     pub fn pop(self) -> (Vec<{ Size - 1 }, Element>, Element)
//!     where
//!         _: Verify<{ Size > 0 }>,
//!     {
//!         self.into()
//!     }
//!
//!     pub fn push(self, e: Element) -> Vec<{ Size + 1 }, Element> {
//!         (self, e).into()
//!     }
//! }
//!
//! #[verify]
//! impl<SizeL: Unsigned, SizeR: Unsigned, Element> std::ops::Add<Vec<SizeR, Element>>
//!     for Vec<SizeL, Element>
//! {
//!     type Output = Vec<{ SizeL + SizeR }, Element>;
//!     fn add(self, Vec(os, mut ov): Vec<SizeR, Element>) -> Self::Output {
//!         let Self(s, mut v) = self;
//!         v.append(&mut ov);
//!         Vec(s + os, v)
//!     }
//! }
//!
//! #[verify]
//! impl<Size: Unsigned, Element> std::convert::From<(Vec<Size, Element>, Element)>
//!     for Vec<{ Size + 1 }, Element>
//! {
//!     fn from((Vec(_, mut v), e): (Vec<Size, Element>, Element)) -> Self {
//!         v.push(e);
//!         Self(Default::default(), v)
//!     }
//! }
//!
//! #[verify]
//! impl<Size: Unsigned, Element> std::convert::From<Vec<Size, Element>>
//!     for (Vec<{ Size - 1 }, Element>, Element)
//! where
//!     _: Verify<{ Size > 0 }>,
//! {
//!     fn from(Vec(_, mut v): Vec<Size, Element>) -> Self {
//!         let e = v.pop().unwrap();
//!         (Vec(Default::default(), v), e)
//!     }
//! }
//! ```

pub mod vec;
pub use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Shl, Shr, Sub};
pub use typenum::*;
pub use verify_macro::verify;
