//! Types and traits for representing booleans as types.

use crate::ops::*;

mod internal {
    pub trait Choice {}
    impl Choice for super::True {}
    impl Choice for super::False {}
    impl Choice for crate::usize::B1 {}
    impl Choice for crate::usize::B0 {}
}

/// Trait bound for the boolean types `True` and `False`.
pub trait Bool: internal::Choice + Default + Not + std::convert::Into<bool> {}

/// Type representing `true`.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct True;
impl Bool for True {}

/// Type representing `false`.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct False;
impl Bool for False {}

#[macro_export]
macro_rules! impl_bool_ops {
    ($false:ident, $true:ident) => {
        pub use std::ops::{BitAnd, BitOr, BitXor, Not};

        impl Same<$false> for $false {
            type Output = $true;
        }
        impl Same<$false> for $true {
            type Output = False;
        }
        impl Same<$true> for $false {
            type Output = False;
        }
        impl Same<$true> for $true {
            type Output = True;
        }

        impl crate::Compare<$false> for $false {
            type Output = crate::Equal;
        }
        impl crate::Compare<$false> for $true {
            type Output = crate::Greater;
        }
        impl crate::Compare<$true> for $false {
            type Output = crate::Less;
        }
        impl crate::Compare<$true> for $true {
            type Output = crate::Equal;
        }

        impl crate::And<$false> for $false {
            type Output = $false;
        }

        impl crate::And<$false> for $true {
            type Output = $false;
        }
        impl crate::And<$true> for $false {
            type Output = $false;
        }

        impl crate::And<$true> for $true {
            type Output = $true;
        }

        impl crate::Or<$false> for $false {
            type Output = $false;
        }
        impl crate::Or<$false> for $true {
            type Output = $true;
        }
        impl crate::Or<$true> for $false {
            type Output = $true;
        }
        impl crate::Or<$true> for $true {
            type Output = $true;
        }

        impl From<$true> for bool {
            fn from(_: $true) -> Self {
                true
            }
        }

        impl From<$false> for bool {
            fn from(_: $false) -> Self {
                false
            }
        }

        impl std::convert::TryFrom<bool> for $true {
            type Error = &'static str;
            fn try_from(maybe_true: bool) -> Result<Self, Self::Error> {
                if maybe_true {
                    Ok(Self)
                } else {
                    Err("cannot convert false into True")
                }
            }
        }

        impl std::convert::TryFrom<bool> for $false {
            type Error = &'static str;
            fn try_from(maybe_false: bool) -> Result<Self, Self::Error> {
                if !maybe_false {
                    Ok(Self)
                } else {
                    Err("cannot convert true into False")
                }
            }
        }

        impl<Rhs: Bool> BitAnd<Rhs> for $false {
            type Output = Self;
            fn bitand(self, _: Rhs) -> Self::Output {
                Default::default()
            }
        }

        impl BitAnd<$false> for $true {
            type Output = $false;
            fn bitand(self, _: Self::Output) -> Self::Output {
                Default::default()
            }
        }

        impl BitAnd<$true> for $true {
            type Output = Self;
            fn bitand(self, _: Self::Output) -> Self::Output {
                Default::default()
            }
        }

        impl std::ops::BitAndAssign for $false {
            fn bitand_assign(&mut self, _: Self) {}
        }

        impl std::ops::BitAndAssign for $true {
            fn bitand_assign(&mut self, _: Self) {}
        }

        impl<Rhs: Bool> BitOr<Rhs> for $false {
            type Output = Rhs;
            fn bitor(self, _: Rhs) -> Self::Output {
                Default::default()
            }
        }

        impl<Rhs: Bool> BitOr<Rhs> for $true {
            type Output = Self;
            fn bitor(self, _: Rhs) -> Self::Output {
                Default::default()
            }
        }

        impl std::ops::BitOrAssign for $false {
            fn bitor_assign(&mut self, _: Self) {}
        }

        impl std::ops::BitOrAssign for $true {
            fn bitor_assign(&mut self, _: Self) {}
        }

        impl<Rhs: Bool> BitXor<Rhs> for $false {
            type Output = Rhs;
            fn bitxor(self, _: Rhs) -> Self::Output {
                Default::default()
            }
        }

        impl BitXor<$false> for $true {
            type Output = Self;
            fn bitxor(self, _: $false) -> Self::Output {
                Default::default()
            }
        }

        impl BitXor<$true> for $true {
            type Output = $false;
            fn bitxor(self, _: Self) -> Self::Output {
                Default::default()
            }
        }

        impl std::ops::BitXorAssign<$false> for $false {
            fn bitxor_assign(&mut self, _: Self) {}
        }

        impl PartialEq<$true> for $false {
            fn eq(&self, _: &$true) -> bool {
                false
            }
        }

        impl PartialEq<$false> for $true {
            fn eq(&self, _: &$false) -> bool {
                false
            }
        }

        impl std::fmt::Display for $false {
            fn fmt(&self, out: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(out, "false")
            }
        }

        impl std::fmt::Display for $true {
            fn fmt(&self, out: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(out, "true")
            }
        }

        impl std::str::FromStr for $false {
            type Err = String;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                let value = s.parse::<bool>().map_err(|e| format!("{}", e))?;
                std::convert::TryInto::<$false>::try_into(value).map_err(ToString::to_string)
            }
        }

        impl std::str::FromStr for $true {
            type Err = String;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                let value = s.parse::<bool>().map_err(|e| format!("{}", e))?;
                std::convert::TryInto::<$true>::try_into(value).map_err(ToString::to_string)
            }
        }

        impl std::hash::Hash for $false {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                let value: bool = (*self).into();
                value.hash(state)
            }
        }

        impl std::hash::Hash for $true {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                let value: bool = (*self).into();
                value.hash(state)
            }
        }

        impl Not for $false {
            type Output = $true;
            fn not(self) -> Self::Output {
                Default::default()
            }
        }

        impl Not for $true {
            type Output = $false;
            fn not(self) -> Self::Output {
                Default::default()
            }
        }

        impl<Rhs: Bool> PartialOrd<Rhs> for $false
        where
            Self: PartialEq<Rhs>,
            Rhs: Into<bool> + Copy,
        {
            fn partial_cmp(&self, rhs: &Rhs) -> Option<std::cmp::Ordering> {
                let lhs: bool = (*self).into();
                let rhs: bool = (*rhs).into();
                lhs.partial_cmp(&rhs)
            }
        }

        impl<Rhs: Bool> PartialOrd<Rhs> for $true
        where
            Self: PartialEq<Rhs>,
            Rhs: Into<bool> + Copy,
        {
            fn partial_cmp(&self, rhs: &Rhs) -> Option<std::cmp::Ordering> {
                let lhs: bool = (*self).into();
                let rhs: bool = (*rhs).into();
                lhs.partial_cmp(&rhs)
            }
        }
    };
}

impl_bool_ops!(False, True);

#[cfg(test)]
#[allow(non_snake_case)]
mod tests {
    use super::*;
    use crate::Same;

    use std::convert::TryFrom;

    fn is_true<B: Bool + Same<True>>() -> bool {
        true
    }

    fn is_false<B: Bool + Same<False>>() -> bool {
        true
    }

    #[test]
    fn True_and_False_are_Bools() {
        fn is_bool<B: Bool>(_: B) -> bool {
            true
        }
        assert!(is_bool(True));
        assert!(is_bool(False));
    }

    #[test]
    fn bool_from_True_returns_true() {
        assert!(bool::from(True));
    }

    #[test]
    fn bool_from_False_returns_false() {
        assert!(!bool::from(False));
    }

    #[test]
    fn True_try_from_true_is_true() {
        assert_eq!(True::try_from(true), Ok(True));
    }

    #[test]
    fn True_try_from_true_is_Err() {
        assert_eq!(True::try_from(false), Err("cannot convert false into True"));
    }

    #[test]
    fn False_try_from_true_is_true() {
        assert_eq!(False::try_from(false), Ok(False));
    }

    #[test]
    fn False_try_from_true_is_Err() {
        assert_eq!(False::try_from(true), Err("cannot convert true into False"));
    }

    #[test]
    fn False_BitAnd_False_is_False() {
        assert_eq!(False & False, False);
    }

    #[test]
    fn False_BitAnd_True_is_False() {
        assert_eq!(False & True, False);
    }

    #[test]
    fn True_BitAnd_False_is_False() {
        assert_eq!(True & False, False);
    }

    #[test]
    fn True_BitAnd_True_is_True() {
        assert_eq!(True & True, True);
    }

    #[test]
    fn False_BitAndAssign_False_is_False() {
        let mut f = False;
        f &= False;
        assert_eq!(f, False);
    }

    #[test]
    fn True_BitAndAssign_True_is_False() {
        let mut t = True;
        t &= True;
        assert_eq!(t, True);
    }

    #[test]
    fn False_BitOr_False_is_False() {
        assert_eq!(False | False, False);
    }

    #[test]
    fn False_BitOr_True_is_True() {
        assert_eq!(False | True, True);
    }

    #[test]
    fn True_BitOr_False_is_True() {
        assert_eq!(True | False, True);
    }

    #[test]
    fn True_BitOr_True_is_True() {
        assert_eq!(True | True, True);
    }

    #[test]
    fn False_BitOrAssign_False_is_False() {
        let mut f = False;
        f |= False;
        assert_eq!(f, False);
    }

    #[test]
    fn True_BitOrAssign_True_is_False() {
        let mut t = True;
        t |= True;
        assert_eq!(t, True);
    }

    #[test]
    fn False_BitXor_False_is_False() {
        assert_eq!(False ^ False, False);
    }

    #[test]
    fn False_BitXor_True_is_True() {
        assert_eq!(False ^ True, True);
    }

    #[test]
    fn True_BitXor_False_is_True() {
        assert_eq!(True ^ False, True);
    }

    #[test]
    fn True_BitXor_True_is_False() {
        assert_eq!(True ^ True, False);
    }

    #[test]
    fn False_BitXorAssign_False_is_False() {
        let mut f = False;
        f ^= False;
        assert_eq!(f, False);
    }

    #[test]
    fn False_PartialEq_True_is_false() {
        assert!(!<False as PartialEq<True>>::eq(&False, &True));
    }

    #[test]
    fn True_PartialEq_False_is_false() {
        assert!(!<True as PartialEq<False>>::eq(&True, &False));
    }

    #[test]
    fn False_displays_as_false() {
        assert_eq!(format!("{}", False), "false");
    }

    #[test]
    fn True_displays_as_true() {
        assert_eq!(format!("{}", True), "true");
    }

    #[test]
    fn False_from_str_matches_bool_from_str() {
        assert_eq!("false".parse(), Ok(False));
        assert!("f".parse::<False>().is_err());
        assert_eq!(
            "true".parse::<False>(),
            Err("cannot convert true into False".to_string())
        );
    }

    #[test]
    fn True_from_str_matches_bool_from_str() {
        assert_eq!("true".parse(), Ok(True));
        assert!("t".parse::<True>().is_err());
        assert_eq!(
            "false".parse::<True>(),
            Err("cannot convert false into True".to_string())
        );
    }

    #[test]
    fn False_Hash_is_same_as_false_Hash() {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let actual = {
            let mut hasher = DefaultHasher::new();
            False.hash(&mut hasher);
            hasher.finish()
        };
        let expected = {
            let mut hasher = DefaultHasher::new();
            false.hash(&mut hasher);
            hasher.finish()
        };
        assert_eq!(actual, expected);
    }

    #[test]
    fn True_Hash_is_same_as_true_Hash() {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let actual = {
            let mut hasher = DefaultHasher::new();
            True.hash(&mut hasher);
            hasher.finish()
        };
        let expected = {
            let mut hasher = DefaultHasher::new();
            true.hash(&mut hasher);
            hasher.finish()
        };
        assert_eq!(actual, expected);
    }

    #[test]
    fn Not_False_is_True() {
        assert_eq!(!False, True);
    }

    #[test]
    fn Not_True_is_False() {
        assert_eq!(!True, False);
    }

    #[test]
    fn False_is_less_than_True() {
        assert!(False < True);
    }

    #[test]
    fn True_is_greater_than_False() {
        assert!(True > False);
    }

    #[test]
    fn False_And_False_is_False() {
        assert!(is_false::<<False as crate::And<False>>::Output>());
    }

    #[test]
    fn False_And_True_is_False() {
        assert!(is_false::<<False as crate::And<True>>::Output>());
    }

    #[test]
    fn True_And_False_is_False() {
        assert!(is_false::<<True as crate::And<False>>::Output>());
    }

    #[test]
    fn True_And_True_is_True() {
        assert!(is_true::<<True as crate::And<True>>::Output>());
    }

    #[test]
    fn False_Or_False_is_False() {
        assert!(is_false::<<False as crate::Or<False>>::Output>());
    }

    #[test]
    fn False_Or_True_is_True() {
        assert!(is_true::<<False as crate::Or<True>>::Output>());
    }

    #[test]
    fn True_Or_False_is_True() {
        assert!(is_true::<<True as crate::Or<False>>::Output>());
    }

    #[test]
    fn True_Or_True_is_True() {
        assert!(is_true::<<True as crate::Or<True>>::Output>());
    }

    #[test]
    fn False_Xor_False_is_False() {
        assert!(is_false::<<False as BitXor<False>>::Output>());
    }

    #[test]
    fn False_Xor_True_is_True() {
        assert!(is_true::<<False as BitXor<True>>::Output>());
    }

    #[test]
    fn True_Xor_False_is_True() {
        assert!(is_true::<<True as BitXor<False>>::Output>());
    }

    #[test]
    fn True_Xor_True_is_False() {
        assert!(is_false::<<True as BitXor<True>>::Output>());
    }

    #[test]
    fn expr_And_False_is_False() {
        assert!(is_false::<
            <<True as crate::And<True>>::Output as crate::And<False>>::Output,
        >());
        assert!(is_false::<
            <<True as crate::Or<True>>::Output as crate::And<False>>::Output,
        >());
        assert!(is_false::<
            <<True as BitXor<False>>::Output as crate::And<False>>::Output,
        >());
    }

    #[test]
    fn expr_And_True_is_expr() {
        assert!(is_false::<
            <<True as crate::And<False>>::Output as crate::And<True>>::Output,
        >());
        assert!(is_false::<
            <<False as crate::Or<False>>::Output as crate::And<True>>::Output,
        >());
        assert!(is_false::<
            <<True as BitXor<True>>::Output as crate::And<True>>::Output,
        >());
        assert!(is_true::<
            <<True as crate::And<True>>::Output as crate::And<True>>::Output,
        >());
        assert!(is_true::<
            <<False as crate::Or<True>>::Output as crate::And<True>>::Output,
        >());
        assert!(is_true::<
            <<True as BitXor<False>>::Output as crate::And<True>>::Output,
        >());
    }

    #[test]
    fn False_and_expr_is_False() {
        assert!(is_false::<
            <False as crate::And<<True as crate::And<True>>::Output>>::Output,
        >());
        assert!(is_false::<
            <False as crate::And<<True as crate::Or<True>>::Output>>::Output,
        >());
        assert!(is_false::<
            <False as crate::And<<True as BitXor<False>>::Output>>::Output,
        >());
    }

    #[test]
    fn True_And_expr_is_expr() {
        assert!(is_false::<
            <True as crate::And<<True as crate::And<False>>::Output>>::Output,
        >());
        assert!(is_false::<
            <True as crate::And<<False as crate::Or<False>>::Output>>::Output,
        >());
        assert!(is_false::<
            <True as crate::And<<True as BitXor<True>>::Output>>::Output,
        >());
        assert!(is_true::<
            <True as crate::And<<True as crate::And<True>>::Output>>::Output,
        >());
        assert!(is_true::<
            <True as crate::And<<False as crate::Or<True>>::Output>>::Output,
        >());
        assert!(is_true::<
            <True as crate::And<<True as BitXor<False>>::Output>>::Output,
        >());
    }
}
