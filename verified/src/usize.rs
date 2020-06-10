//! Types and traits for representing unsigned integers as types.

use crate::ops::*;
use crate::{Bool, False, True};
pub use std::ops::Add;

mod internal {
    use std::ops::BitAnd;

    pub trait Bit: Default + BitAnd + super::Compare<Self> + super::Same<Self> {}
    impl Bit for super::B0 {}
    impl Bit for super::B1 {}

    pub trait Nat: Default {}
    impl Nat for super::T {}
    impl<Msb: super::Usize, Lsb: Bit> Nat for super::U<Msb, Lsb> {}

    pub trait NotT {}
    impl<Msb: super::Usize, Lsb: Bit> NotT for super::U<Msb, Lsb> {}
}
use internal::*;

/// Trait bound for unsigned types.
pub trait Usize: Nat + Compare<Self> + Same<Self> {}

/// Representation of a `0` bit.
///
/// # Examples
///
/// ```
/// use verified::*;
///
/// let _: U<T, B0>;
/// //          ^
/// ```
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct B0;
impl Bool for B0 {}

/// Representation of a `1` bit.
///
/// # Examples
///
/// ```
/// use verified::*;
///
/// let _: U<T, B1>;
/// //          ^
/// ```
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct B1;
impl Bool for B1 {}

crate::impl_bool_ops!(B0, B1);

/// Null terminator for `U` types.
///
/// # Examples
///
/// ```
/// use verified::*;
///
/// let _: U<T, B1>;
/// //       ^
/// ```
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct T;
impl Usize for T {}

/// Constructor for `Usize` types.
///
/// # Examples
///
/// ```
/// use verified::*;
///
/// let zero: U<T, B0>;
/// let one: U<T, B1>;
/// let two: U<U<T, B1>, B0>;
/// let three: U<U<T, B1>, B1>;
/// ```
#[derive(Clone, Copy, Debug, Default)]
pub struct U<Msb: Usize, Lsb: Bit>(Msb, Lsb);
impl<Msb: Usize, Lsb: Bit> Usize for U<Msb, Lsb> where
    <Msb as Compare<Msb>>::Output: Compare<<Lsb as Compare<Lsb>>::Output>
{
}

impl Compare<T> for T {
    type Output = Equal;
}
impl<LhsMsb: Usize, LhsLsb: Bit> Compare<T> for U<LhsMsb, LhsLsb> {
    type Output = Greater;
}
impl<LhsMsb: Usize, LhsLsb: Bit> Compare<U<LhsMsb, LhsLsb>> for T {
    type Output = Less;
}
impl<LhsMsb: Usize, LhsLsb: Bit, RhsMsb: Usize, RhsLsb: Bit> Compare<U<RhsMsb, RhsLsb>>
    for U<LhsMsb, LhsLsb>
where
    LhsMsb: Compare<RhsMsb>,
    LhsLsb: Compare<RhsLsb>,
    <LhsMsb as Compare<RhsMsb>>::Output: Ordering,
    <LhsLsb as Compare<RhsLsb>>::Output: Ordering,
    <LhsMsb as Compare<RhsMsb>>::Output: Compare<<LhsLsb as Compare<RhsLsb>>::Output>,
{
    type Output = <<LhsMsb as Compare<RhsMsb>>::Output as Compare<
        <LhsLsb as Compare<RhsLsb>>::Output,
    >>::Output;
}
impl<LhsMsb: Usize, LhsLsb: Bit, RhsMsb: Usize, RhsLsb: Bit> PartialOrd<U<RhsMsb, RhsLsb>>
    for U<LhsMsb, LhsLsb>
where
    U<LhsMsb, LhsLsb>: Compare<U<RhsMsb, RhsLsb>>,
    U<LhsMsb, LhsLsb>: PartialEq<U<RhsMsb, RhsLsb>>,
{
    fn partial_cmp(&self, _other: &U<RhsMsb, RhsLsb>) -> Option<std::cmp::Ordering> {
        Some(<Self as Compare<U<RhsMsb, RhsLsb>>>::Output::default().into())
    }
}

impl<Lhs: Usize, Rhs: Usize> Same<Rhs> for Lhs
where
    Lhs: Compare<Rhs>,
{
    type Output = <<Lhs as Compare<Rhs>>::Output as Same<Equal>>::Output;
}
impl<LhsMsb: Usize, LhsLsb: Bit, RhsMsb: Usize, RhsLsb: Bit> PartialEq<U<RhsMsb, RhsLsb>>
    for U<LhsMsb, LhsLsb>
where
    U<LhsMsb, LhsLsb>: Same<U<RhsMsb, RhsLsb>>,
{
    fn eq(&self, _other: &U<RhsMsb, RhsLsb>) -> bool {
        <Self as Same<U<RhsMsb, RhsLsb>>>::Output::default().into()
    }
}

impl BitAnd<T> for T {
    type Output = T;
    fn bitand(self, _other: T) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, LhsLsb> BitAnd<T> for U<LhsMsb, LhsLsb>
where
    LhsMsb: Usize,
    LhsLsb: Bit,
    U<LhsMsb, LhsLsb>: Usize,
{
    type Output = T;
    fn bitand(self, _other: T) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, LhsLsb> BitAnd<U<LhsMsb, LhsLsb>> for T
where
    LhsMsb: Usize,
    LhsLsb: Bit,
    U<LhsMsb, LhsLsb>: Usize,
{
    type Output = T;
    fn bitand(self, _other: U<LhsMsb, LhsLsb>) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, LhsLsb, RhsMsb> BitAnd<U<RhsMsb, B0>> for U<LhsMsb, LhsLsb>
where
    LhsMsb: BitAnd<RhsMsb> + Usize,
    <LhsMsb as BitAnd<RhsMsb>>::Output: Usize,
    LhsLsb: Bit,
    RhsMsb: Usize,
    U<LhsMsb, LhsLsb>: Usize,
{
    type Output = U<<LhsMsb as BitAnd<RhsMsb>>::Output, B0>;
    fn bitand(self, _other: U<RhsMsb, B0>) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, RhsMsb> BitAnd<U<RhsMsb, B1>> for U<LhsMsb, B0>
where
    LhsMsb: BitAnd<RhsMsb> + Usize,
    <LhsMsb as BitAnd<RhsMsb>>::Output: Usize,
    RhsMsb: Usize,
{
    type Output = U<<LhsMsb as BitAnd<RhsMsb>>::Output, B0>;
    fn bitand(self, _other: U<RhsMsb, B1>) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, RhsMsb> BitAnd<U<RhsMsb, B1>> for U<LhsMsb, B1>
where
    LhsMsb: BitAnd<RhsMsb> + Usize,
    <LhsMsb as BitAnd<RhsMsb>>::Output: Usize,
    RhsMsb: Usize,
    U<LhsMsb, B1>: Usize,
{
    type Output = U<<LhsMsb as BitAnd<RhsMsb>>::Output, B1>;
    fn bitand(self, _other: U<RhsMsb, B1>) -> Self::Output {
        Default::default()
    }
}

impl BitOr<T> for T {
    type Output = T;
    fn bitor(self, _other: T) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, LhsLsb> BitOr<T> for U<LhsMsb, LhsLsb>
where
    LhsMsb: Usize,
    LhsLsb: Bit,
    U<LhsMsb, LhsLsb>: Usize,
{
    type Output = Self;
    fn bitor(self, _other: T) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, LhsLsb> BitOr<U<LhsMsb, LhsLsb>> for T
where
    LhsMsb: Usize,
    LhsLsb: Bit,
    U<LhsMsb, LhsLsb>: Usize,
{
    type Output = U<LhsMsb, LhsLsb>;
    fn bitor(self, _other: U<LhsMsb, LhsLsb>) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, LhsLsb, RhsMsb> BitOr<U<RhsMsb, B0>> for U<LhsMsb, LhsLsb>
where
    LhsMsb: BitOr<RhsMsb> + Usize,
    <LhsMsb as BitOr<RhsMsb>>::Output: Usize,
    LhsLsb: Bit,
    RhsMsb: Usize,
    U<LhsMsb, LhsLsb>: Usize,
{
    type Output = U<<LhsMsb as BitOr<RhsMsb>>::Output, LhsLsb>;
    fn bitor(self, _other: U<RhsMsb, B0>) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, RhsMsb> BitOr<U<RhsMsb, B1>> for U<LhsMsb, B0>
where
    LhsMsb: BitOr<RhsMsb> + Usize,
    <LhsMsb as BitOr<RhsMsb>>::Output: Usize,
    RhsMsb: Usize,
{
    type Output = U<<LhsMsb as BitOr<RhsMsb>>::Output, B1>;
    fn bitor(self, _other: U<RhsMsb, B1>) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, RhsMsb> BitOr<U<RhsMsb, B1>> for U<LhsMsb, B1>
where
    LhsMsb: BitOr<RhsMsb> + Usize,
    <LhsMsb as BitOr<RhsMsb>>::Output: Usize,
    RhsMsb: Usize,
    U<LhsMsb, B1>: Usize,
{
    type Output = U<<LhsMsb as BitOr<RhsMsb>>::Output, B1>;
    fn bitor(self, _other: U<RhsMsb, B1>) -> Self::Output {
        Default::default()
    }
}

impl BitXor<T> for T {
    type Output = T;
    fn bitxor(self, _other: T) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, LhsLsb> BitXor<T> for U<LhsMsb, LhsLsb>
where
    LhsMsb: Usize,
    LhsLsb: Bit,
    U<LhsMsb, LhsLsb>: Usize,
{
    type Output = Self;
    fn bitxor(self, _other: T) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, LhsLsb> BitXor<U<LhsMsb, LhsLsb>> for T
where
    LhsMsb: Usize,
    LhsLsb: Bit,
    U<LhsMsb, LhsLsb>: Usize,
{
    type Output = U<LhsMsb, LhsLsb>;
    fn bitxor(self, _other: U<LhsMsb, LhsLsb>) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, LhsLsb, RhsMsb> BitXor<U<RhsMsb, B0>> for U<LhsMsb, LhsLsb>
where
    LhsMsb: BitXor<RhsMsb> + Usize,
    <LhsMsb as BitXor<RhsMsb>>::Output: Usize,
    LhsLsb: Bit,
    RhsMsb: Usize,
    U<LhsMsb, LhsLsb>: Usize,
{
    type Output = U<<LhsMsb as BitXor<RhsMsb>>::Output, LhsLsb>;
    fn bitxor(self, _other: U<RhsMsb, B0>) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, RhsMsb> BitXor<U<RhsMsb, B1>> for U<LhsMsb, B0>
where
    LhsMsb: BitXor<RhsMsb> + Usize,
    <LhsMsb as BitXor<RhsMsb>>::Output: Usize,
    RhsMsb: Usize,
{
    type Output = U<<LhsMsb as BitXor<RhsMsb>>::Output, B1>;
    fn bitxor(self, _other: U<RhsMsb, B1>) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, RhsMsb> BitXor<U<RhsMsb, B1>> for U<LhsMsb, B1>
where
    LhsMsb: BitXor<RhsMsb> + Usize,
    <LhsMsb as BitXor<RhsMsb>>::Output: Usize,
    RhsMsb: Usize,
    U<LhsMsb, B1>: Usize,
{
    type Output = U<<LhsMsb as BitXor<RhsMsb>>::Output, B0>;
    fn bitxor(self, _other: U<RhsMsb, B1>) -> Self::Output {
        Default::default()
    }
}

impl Add<T> for T {
    type Output = T;
    fn add(self, _other: T) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, LhsLsb> Add<T> for U<LhsMsb, LhsLsb>
where
    LhsMsb: Usize,
    LhsLsb: Bit,
    U<LhsMsb, LhsLsb>: Usize,
{
    type Output = U<LhsMsb, LhsLsb>;
    fn add(self, _other: T) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, LhsLsb> Add<U<LhsMsb, LhsLsb>> for T
where
    LhsMsb: Usize,
    LhsLsb: Bit,
    U<LhsMsb, LhsLsb>: Usize,
{
    type Output = U<LhsMsb, LhsLsb>;
    fn add(self, _other: U<LhsMsb, LhsLsb>) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, LhsLsb, RhsMsb> Add<U<RhsMsb, B0>> for U<LhsMsb, LhsLsb>
where
    LhsMsb: Add<RhsMsb> + Usize,
    <LhsMsb as Add<RhsMsb>>::Output: Usize,
    LhsLsb: Bit,
    RhsMsb: Usize,
    U<LhsMsb, LhsLsb>: Usize,
{
    type Output = U<<LhsMsb as Add<RhsMsb>>::Output, LhsLsb>;
    fn add(self, _other: U<RhsMsb, B0>) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, RhsMsb> Add<U<RhsMsb, B1>> for U<LhsMsb, B0>
where
    LhsMsb: Add<RhsMsb> + Usize,
    <LhsMsb as Add<RhsMsb>>::Output: Usize,
    RhsMsb: Usize,
{
    type Output = U<<LhsMsb as Add<RhsMsb>>::Output, B1>;
    fn add(self, _other: U<RhsMsb, B1>) -> Self::Output {
        Default::default()
    }
}

impl<LhsMsb, RhsMsb> Add<U<RhsMsb, B1>> for U<LhsMsb, B1>
where
    LhsMsb: Add<RhsMsb> + Usize,
    <LhsMsb as Add<RhsMsb>>::Output: Usize,
    <LhsMsb as Add<RhsMsb>>::Output: Add<U<T, B1>>,
    <<LhsMsb as Add<RhsMsb>>::Output as Add<U<T, B1>>>::Output: Usize,
    RhsMsb: Usize,
    U<LhsMsb, B1>: Usize,
{
    type Output = U<<<LhsMsb as Add<RhsMsb>>::Output as Add<U<T, B1>>>::Output, B0>;
    fn add(self, _other: U<RhsMsb, B1>) -> Self::Output {
        Default::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn usize_implements_equal() {
        assert_eq!(U(T, B0), U(T, B0));
    }

    #[test]
    fn self_bitand_self_is_self() {
        assert_eq!(U(T, B0) & U(T, B0), U(T, B0));
        assert_eq!(U(T, B1) & U(T, B1), U(T, B1));
        assert_eq!(U(U(T, B1), B0) & U(U(T, B1), B0), U(U(T, B1), B0));
    }

    #[test]
    fn self_bitand_self_with_different_lsb_is_self_with_lsb_of_zero() {
        assert_eq!(U(U(T, B1), B0) & U(U(T, B1), B1), U(U(T, B1), B0));
    }

    #[test]
    fn self_bitand_other_with_different_lsb_and_msb_applies_bitand_to_each_bit() {
        assert_eq!(
            U(U(U(U(T, B1), B0), B1), B0) & U(U(U(U(T, B1), B1), B1), B0),
            U(U(U(U(T, B1), B0), B1), B0)
        );
    }

    #[test]
    fn self_bitand_other_assumes_zero_msb_bits_for_other_when_shorter() {
        assert_eq!(
            U(U(U(U(T, B1), B1), B1), B1) & U(U(U(T, B1), B1), B1),
            U(U(U(T, B1), B1), B1)
        );
    }

    #[test]
    fn self_bitand_other_assumes_zero_msb_bits_for_self_when_shorter() {
        assert_eq!(
            U(U(U(T, B1), B1), B1) & U(U(U(U(T, B1), B1), B1), B1),
            U(U(U(T, B1), B1), B1)
        );
    }

    #[test]
    fn self_bitor_self_is_self() {
        assert_eq!(U(T, B0) | U(T, B0), U(T, B0));
        assert_eq!(U(T, B1) | U(T, B1), U(T, B1));
        assert_eq!(U(U(T, B1), B0) | U(U(T, B1), B0), U(U(T, B1), B0));
    }

    #[test]
    fn self_bitor_self_with_different_lsb_is_self_with_lsb_of_one() {
        assert_eq!(U(U(T, B1), B0) | U(U(T, B1), B1), U(U(T, B1), B1));
    }

    #[test]
    fn self_bitor_other_with_different_lsb_and_msb_applies_bitor_to_each_bit() {
        assert_eq!(
            U(U(U(U(T, B1), B0), B1), B0) | U(U(U(U(T, B1), B1), B1), B0),
            U(U(U(U(T, B1), B1), B1), B0)
        );
    }

    #[test]
    fn self_bitor_other_assumes_unchanged_msb_bits_for_other_when_shorter() {
        assert_eq!(
            U(U(U(U(T, B1), B1), B1), B1) | U(U(U(T, B1), B0), B0),
            U(U(U(U(T, B1), B1), B1), B1)
        );
    }

    #[test]
    fn self_bitor_other_assumes_unchanged_msb_bits_for_self_when_shorter() {
        assert_eq!(
            U(U(U(T, B1), B0), B0) | U(U(U(U(T, B1), B1), B1), B1),
            U(U(U(U(T, B1), B1), B1), B1)
        );
    }

    #[test]
    fn self_bitxor_self_is_zero() {
        assert_eq!(U(T, B0) ^ U(T, B0), U(T, B0));
        assert_eq!(U(T, B1) ^ U(T, B1), U(T, B0));
        assert_eq!(U(U(T, B1), B0) ^ U(U(T, B1), B0), U(U(T, B0), B0));
    }

    #[test]
    fn self_bitxor_self_with_different_lsb_is_self_with_lsb_of_one() {
        assert_eq!(U(U(T, B0), B0) ^ U(U(T, B0), B1), U(U(T, B0), B1));
    }

    #[test]
    fn self_bitxor_other_with_different_lsb_and_msb_applies_bitxor_to_each_bit() {
        assert_eq!(
            U(U(U(U(T, B1), B0), B1), B0) ^ U(U(U(U(T, B1), B1), B1), B0),
            U(U(U(U(T, B0), B1), B0), B0)
        );
    }

    #[test]
    fn self_bitxor_other_assumes_unchanged_msb_bits_for_other_when_shorter() {
        assert_eq!(
            U(U(U(U(T, B1), B1), B1), B1) ^ U(U(U(T, B1), B0), B0),
            U(U(U(U(T, B1), B0), B1), B1)
        );
    }

    #[test]
    fn self_bitxor_other_assumes_unchanged_msb_bits_for_self_when_shorter() {
        assert_eq!(
            U(U(U(T, B1), B0), B0) ^ U(U(U(U(T, B1), B1), B1), B1),
            U(U(U(U(T, B1), B0), B1), B1)
        );
    }

    #[test]
    fn self_plus_zero_is_self() {
        assert_eq!(U(T, B0) + U(T, B0), U(T, B0));
        assert_eq!(U(T, B1) + U(T, B0), U(T, B1));
        assert_eq!(U(T, B0) + U(T, B1), U(T, B1));
        assert_eq!(U(U(T, B1), B0) + U(T, B0), U(U(T, B1), B0));
        assert_eq!(U(T, B0) + U(U(T, B1), B0), U(U(T, B1), B0));
        assert_eq!(U(U(T, B1), B1) + U(T, B0), U(U(T, B1), B1));
        assert_eq!(U(T, B0) + U(U(T, B1), B1), U(U(T, B1), B1));
    }

    #[test]
    fn one_plus_one_caries_bit_to_get_two() {
        assert_eq!(U(T, B1) + U(T, B1), U(U(T, B1), B0));
    }

    #[test]
    fn carry_bit_propogates() {
        assert_eq!(U(U(T, B1), B1) + U(T, B1), U(U(U(T, B1), B0), B0));
        assert_eq!(U(T, B1) + U(U(T, B1), B1), U(U(U(T, B1), B0), B0));
        assert_eq!(U(U(U(T, B1), B0), B1) + U(T, B1), U(U(U(T, B1), B1), B0));
    }

    #[test]
    fn one_is_equal_to_one() {
        assert_eq!(U(T, B1), U(T, B1));
    }

    #[test]
    fn zero_is_equal_to_zero() {
        assert_eq!(U(T, B0), U(T, B0));
    }

    #[test]
    fn one_is_not_equal_to_zero() {
        assert_ne!(U(T, B1), U(T, B0));
        assert_ne!(U(T, B0), U(T, B1));
    }

    #[test]
    fn two_is_not_equal_to_zero() {
        assert_ne!(U(U(T, B1), B0), U(T, B0));
        assert_ne!(U(T, B0), U(U(T, B1), B0));
    }

    #[test]
    fn two_is_equal_to_two() {
        assert_eq!(U(U(T, B1), B0), U(U(T, B1), B0));
    }

    #[test]
    fn zero_is_less_than_one() {
        assert!(U(T, B0) < U(T, B1));
        assert!(U(T, B1) > U(T, B0));
    }

    #[test]
    fn zero_is_less_than_two() {
        assert!(U(T, B0) < U(U(T, B1), B0));
        assert!(U(U(T, B1), B0) > U(T, B0));
    }

    #[test]
    fn one_is_less_than_two() {
        assert!(U(T, B1) < U(U(T, B1), B0));
        assert!(U(U(T, B1), B0) > U(T, B1));
    }

    #[test]
    fn ordering_types_implement_same() {
        assert_eq!(<Less as Same<Less>>::Output::default(), True);
        assert_eq!(<Less as Same<Equal>>::Output::default(), False);
        assert_eq!(<Less as Same<Greater>>::Output::default(), False);
        assert_eq!(<Equal as Same<Less>>::Output::default(), False);
        assert_eq!(<Equal as Same<Equal>>::Output::default(), True);
        assert_eq!(<Equal as Same<Greater>>::Output::default(), False);
        assert_eq!(<Greater as Same<Less>>::Output::default(), False);
        assert_eq!(<Greater as Same<Equal>>::Output::default(), False);
        assert_eq!(<Greater as Same<Greater>>::Output::default(), True);
    }
}
