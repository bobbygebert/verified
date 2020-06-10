//! Types and traits for representing binary and unary operations as types.

pub use std::ops::{BitAnd, BitOr, BitXor, Not};

mod internal {
    pub trait NotCompare {}
    impl<T: crate::Usize> NotCompare for T {}
    impl NotCompare for crate::bool::False {}
    impl NotCompare for crate::bool::True {}
    impl NotCompare for crate::usize::B0 {}
    impl NotCompare for crate::usize::B1 {}
}
use internal::*;

pub use crate::{Bool, False, True};

/// Check two types for equality.
///
/// # Examples
///
/// ```
/// use verified::*;
///
/// let _: <True as Same<True>>::Output = True;
/// let _: <True as Same<False>>::Output = False;
/// ```
pub trait Same<Rhs> {
    type Output: Bool;
}

/// Logical AND operator `&&`.
///
/// # Examples
///
/// ```
/// use verified::*;
///
/// let _: <True as And<True>>::Output = True;
/// let _: <True as And<False>>::Output = False;
/// ```
pub trait And<Rhs: Bool> {
    type Output: Bool;
}

/// Logical OR operator `||`.
///
/// # Examples
///
/// ```
/// use verified::*;
///
/// let _: <True as Or<True>>::Output = True;
/// let _: <True as Or<False>>::Output = True;
/// let _: <False as Or<False>>::Output = False;
/// ```
pub trait Or<Rhs: Bool> {
    type Output: Bool;
}

/// Inequality operator `!=`.
///
/// # Examples
///
/// ```
/// use verified::*;
///
/// let _: <True as Ne<False>>::Output = True;
/// let _: <True as Ne<True>>::Output = False;
/// ```
pub trait Ne<Rhs> {
    type Output: Bool;
}

/// Less-than operator `<`.
///
/// # Examples
///
/// ```
/// use verified::*;
///
/// let _: <U<T, B0> as Lt<U<T, B1>>>::Output = True;
/// let _: <U<T, B0> as Lt<U<T, B0>>>::Output = False;
/// ```
pub trait Lt<Rhs> {
    type Output: Bool;
}
impl<Lhs: NotCompare, Rhs: NotCompare> Lt<Rhs> for Lhs
where
    Lhs: Compare<Rhs>,
    <Lhs as Compare<Rhs>>::Output: Ordering + Same<Less>,
{
    type Output = <<Lhs as Compare<Rhs>>::Output as Same<Less>>::Output;
}

/// Less-equal operator `<=`.
///
/// # Examples
///
/// ```
/// use verified::*;
///
/// let _: <U<T, B0> as Le<U<T, B1>>>::Output = True;
/// let _: <U<T, B1> as Le<U<T, B1>>>::Output = True;
/// let _: <U<T, B1> as Le<U<T, B0>>>::Output = False;
/// ```
pub trait Le<Rhs> {
    type Output: Bool;
}
impl<Lhs: NotCompare, Rhs: NotCompare> Le<Rhs> for Lhs
where
    Lhs: Compare<Rhs>,
    <Lhs as Compare<Rhs>>::Output: Ordering + Same<Less> + Same<Equal>,
    <<Lhs as Compare<Rhs>>::Output as Same<Less>>::Output:
        Or<<<Lhs as Compare<Rhs>>::Output as Same<Equal>>::Output>,
{
    type Output = <<<Lhs as Compare<Rhs>>::Output as Same<Less>>::Output as Or<
        <<Lhs as Compare<Rhs>>::Output as Same<Equal>>::Output,
    >>::Output;
}

/// Greater-than operator `>`.
///
/// # Examples
///
/// ```
/// use verified::*;
///
/// let _: <U<T, B1> as Gt<U<T, B0>>>::Output = True;
/// let _: <U<T, B0> as Gt<U<T, B1>>>::Output = False;
/// ```
pub trait Gt<Rhs> {
    type Output: Bool;
}
impl<Lhs: NotCompare, Rhs: NotCompare> Gt<Rhs> for Lhs
where
    Lhs: Compare<Rhs>,
    <Lhs as Compare<Rhs>>::Output: Ordering + Same<Greater>,
{
    type Output = <<Lhs as Compare<Rhs>>::Output as Same<Greater>>::Output;
}

/// Greater-equal operator `>=`.
///
/// # Examples
///
/// ```
/// use verified::*;
///
/// let _: <U<T, B1> as Ge<U<T, B0>>>::Output = True;
/// let _: <U<T, B0> as Ge<U<T, B0>>>::Output = True;
/// let _: <U<T, B0> as Ge<U<T, B1>>>::Output = False;
/// ```
pub trait Ge<Rhs> {
    type Output: Bool;
}
impl<Lhs: NotCompare, Rhs: NotCompare> Ge<Rhs> for Lhs
where
    Lhs: Compare<Rhs>,
    <Lhs as Compare<Rhs>>::Output: Ordering + Same<Greater> + Same<Equal>,
    <<Lhs as Compare<Rhs>>::Output as Same<Greater>>::Output:
        Or<<<Lhs as Compare<Rhs>>::Output as Same<Equal>>::Output>,
{
    type Output = <<<Lhs as Compare<Rhs>>::Output as Same<Greater>>::Output as Or<
        <<Lhs as Compare<Rhs>>::Output as Same<Equal>>::Output,
    >>::Output;
}
impl<Lhs, Rhs> Ne<Rhs> for Lhs
where
    Lhs: Same<Rhs>,
    <Lhs as Same<Rhs>>::Output: Not,
    <<Lhs as Same<Rhs>>::Output as Not>::Output: Bool,
{
    type Output = <<Lhs as Same<Rhs>>::Output as Not>::Output;
}

/// Trait bound satisfied by only `Less`, `Equal`, and `Greater`.
pub trait Ordering:
    Into<std::cmp::Ordering> + Default + Same<Less> + Same<Equal> + Same<Greater>
{
}

/// The result of comparing types A and B when A is strictly less than B.
#[derive(Default)]
pub struct Less;
impl Ordering for Less {}
impl Same<Less> for Less {
    type Output = True;
}
impl Same<Equal> for Less {
    type Output = False;
}
impl Same<Greater> for Less {
    type Output = False;
}
impl From<Less> for std::cmp::Ordering {
    fn from(_from: Less) -> Self {
        std::cmp::Ordering::Less
    }
}

/// The result of comparing types A and B when A is equal to B.
#[derive(Default)]
pub struct Equal;
impl Ordering for Equal {}
impl Same<Less> for Equal {
    type Output = False;
}
impl Same<Equal> for Equal {
    type Output = True;
}
impl Same<Greater> for Equal {
    type Output = False;
}
impl From<Equal> for std::cmp::Ordering {
    fn from(_from: Equal) -> Self {
        std::cmp::Ordering::Equal
    }
}

/// The result of comparing types A and B when A is strictly greater than B.
#[derive(Default)]
pub struct Greater;
impl Ordering for Greater {}
impl Same<Less> for Greater {
    type Output = False;
}
impl Same<Equal> for Greater {
    type Output = False;
}
impl Same<Greater> for Greater {
    type Output = True;
}
impl From<Greater> for std::cmp::Ordering {
    fn from(_from: Greater) -> Self {
        std::cmp::Ordering::Greater
    }
}

/// Compare types A and B. Output is one of `Less`, `Equal` or `Greater`.
pub trait Compare<Rhs> {
    type Output: Ordering;
}
impl<LsbOrdering> Compare<LsbOrdering> for Less {
    type Output = Less;
}
impl<LsbOrdering: Ordering> Compare<LsbOrdering> for Equal {
    type Output = LsbOrdering;
}
impl<LsbOrdering> Compare<LsbOrdering> for Greater {
    type Output = Greater;
}
