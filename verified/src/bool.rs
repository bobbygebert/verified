pub use std::ops::Not;

mod internal {
    pub trait Choice<A, B> {}
    impl Choice<super::False, super::True> for super::True {}
    impl Choice<super::False, super::True> for super::False {}
}

pub trait Bool: internal::Choice<False, True> + Default + Not {}
pub trait And<Rhs: Bool> {
    type Output: Bool;
}
pub trait Or<Rhs: Bool> {
    type Output: Bool;
}
pub trait Xor<Rhs: Bool> {
    type Output: Bool;
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct True;
impl Bool for True {}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct False;
impl Bool for False {}

impl And<False> for False {
    type Output = False;
}

impl And<False> for True {
    type Output = False;
}
impl And<True> for False {
    type Output = False;
}

impl And<True> for True {
    type Output = True;
}

impl Or<False> for False {
    type Output = False;
}
impl Or<False> for True {
    type Output = True;
}
impl Or<True> for False {
    type Output = True;
}
impl Or<True> for True {
    type Output = True;
}

impl Xor<False> for False {
    type Output = False;
}
impl Xor<False> for True {
    type Output = True;
}
impl Xor<True> for False {
    type Output = True;
}
impl Xor<True> for True {
    type Output = False;
}

impl From<True> for bool {
    fn from(_: True) -> Self {
        true
    }
}

impl From<False> for bool {
    fn from(_: False) -> Self {
        false
    }
}

impl std::convert::TryFrom<bool> for True {
    type Error = &'static str;
    fn try_from(maybe_true: bool) -> Result<Self, Self::Error> {
        if maybe_true {
            Ok(Self)
        } else {
            Err("cannot convert false into True")
        }
    }
}

impl std::convert::TryFrom<bool> for False {
    type Error = &'static str;
    fn try_from(maybe_false: bool) -> Result<Self, Self::Error> {
        if !maybe_false {
            Ok(Self)
        } else {
            Err("cannot convert true into False")
        }
    }
}

impl<Rhs: Bool> std::ops::BitAnd<Rhs> for False {
    type Output = Self;
    fn bitand(self, _: Rhs) -> Self::Output {
        Default::default()
    }
}

impl std::ops::BitAnd<False> for True {
    type Output = False;
    fn bitand(self, _: Self::Output) -> Self::Output {
        Default::default()
    }
}

impl std::ops::BitAnd<True> for True {
    type Output = Self;
    fn bitand(self, _: Self::Output) -> Self::Output {
        Default::default()
    }
}

impl std::ops::BitAndAssign for False {
    fn bitand_assign(&mut self, _: Self) {}
}

impl std::ops::BitAndAssign for True {
    fn bitand_assign(&mut self, _: Self) {}
}

impl<Rhs: Bool> std::ops::BitOr<Rhs> for False {
    type Output = Rhs;
    fn bitor(self, _: Rhs) -> Self::Output {
        Default::default()
    }
}

impl<Rhs: Bool> std::ops::BitOr<Rhs> for True {
    type Output = Self;
    fn bitor(self, _: Rhs) -> Self::Output {
        Default::default()
    }
}

impl std::ops::BitOrAssign for False {
    fn bitor_assign(&mut self, _: Self) {}
}

impl std::ops::BitOrAssign for True {
    fn bitor_assign(&mut self, _: Self) {}
}

impl<Rhs: Bool> std::ops::BitXor<Rhs> for False {
    type Output = Rhs;
    fn bitxor(self, _: Rhs) -> Self::Output {
        Default::default()
    }
}

impl std::ops::BitXor<False> for True {
    type Output = Self;
    fn bitxor(self, _: False) -> Self::Output {
        Default::default()
    }
}

impl std::ops::BitXor<True> for True {
    type Output = False;
    fn bitxor(self, _: Self) -> Self::Output {
        Default::default()
    }
}

impl std::ops::BitXorAssign<False> for False {
    fn bitxor_assign(&mut self, _: Self) {}
}

impl PartialEq<True> for False {
    fn eq(&self, _: &True) -> bool {
        false
    }
}

impl PartialEq<False> for True {
    fn eq(&self, _: &False) -> bool {
        false
    }
}

impl std::fmt::Display for False {
    fn fmt(&self, out: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(out, "false")
    }
}

impl std::fmt::Display for True {
    fn fmt(&self, out: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(out, "true")
    }
}

impl std::str::FromStr for False {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let value = s.parse::<bool>().map_err(|e| format!("{}", e))?;
        std::convert::TryInto::<False>::try_into(value).map_err(ToString::to_string)
    }
}

impl std::str::FromStr for True {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let value = s.parse::<bool>().map_err(|e| format!("{}", e))?;
        std::convert::TryInto::<True>::try_into(value).map_err(ToString::to_string)
    }
}

impl std::hash::Hash for False {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let value: bool = (*self).into();
        value.hash(state)
    }
}

impl std::hash::Hash for True {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let value: bool = (*self).into();
        value.hash(state)
    }
}

impl Not for False {
    type Output = True;
    fn not(self) -> Self::Output {
        Default::default()
    }
}

impl Not for True {
    type Output = False;
    fn not(self) -> Self::Output {
        Default::default()
    }
}

impl<Rhs: Bool> PartialOrd<Rhs> for False
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

impl<Rhs: Bool> PartialOrd<Rhs> for True
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
        assert!(is_false::<<False as And<False>>::Output>());
    }

    #[test]
    fn False_And_True_is_False() {
        assert!(is_false::<<False as And<True>>::Output>());
    }

    #[test]
    fn True_And_False_is_False() {
        assert!(is_false::<<True as And<False>>::Output>());
    }

    #[test]
    fn True_And_True_is_True() {
        assert!(is_true::<<True as And<True>>::Output>());
    }

    #[test]
    fn False_Or_False_is_False() {
        assert!(is_false::<<False as Or<False>>::Output>());
    }

    #[test]
    fn False_Or_True_is_True() {
        assert!(is_true::<<False as Or<True>>::Output>());
    }

    #[test]
    fn True_Or_False_is_True() {
        assert!(is_true::<<True as Or<False>>::Output>());
    }

    #[test]
    fn True_Or_True_is_True() {
        assert!(is_true::<<True as Or<True>>::Output>());
    }

    #[test]
    fn False_Xor_False_is_False() {
        assert!(is_false::<<False as Xor<False>>::Output>());
    }

    #[test]
    fn False_Xor_True_is_True() {
        assert!(is_true::<<False as Xor<True>>::Output>());
    }

    #[test]
    fn True_Xor_False_is_True() {
        assert!(is_true::<<True as Xor<False>>::Output>());
    }

    #[test]
    fn True_Xor_True_is_False() {
        assert!(is_false::<<True as Xor<True>>::Output>());
    }

    #[test]
    fn expr_And_False_is_False() {
        assert!(is_false::<
            <<True as And<True>>::Output as And<False>>::Output,
        >());
        assert!(is_false::<<<True as Or<True>>::Output as And<False>>::Output>());
        assert!(is_false::<
            <<True as Xor<False>>::Output as And<False>>::Output,
        >());
    }

    #[test]
    fn expr_And_True_is_expr() {
        assert!(is_false::<
            <<True as And<False>>::Output as And<True>>::Output,
        >());
        assert!(is_false::<
            <<False as Or<False>>::Output as And<True>>::Output,
        >());
        assert!(is_false::<<<True as Xor<True>>::Output as And<True>>::Output>());
        assert!(is_true::<<<True as And<True>>::Output as And<True>>::Output>());
        assert!(is_true::<<<False as Or<True>>::Output as And<True>>::Output>());
        assert!(is_true::<<<True as Xor<False>>::Output as And<True>>::Output>());
    }

    #[test]
    fn False_and_expr_is_False() {
        assert!(is_false::<
            <False as And<<True as And<True>>::Output>>::Output,
        >());
        assert!(is_false::<<False as And<<True as Or<True>>::Output>>::Output>());
        assert!(is_false::<
            <False as And<<True as Xor<False>>::Output>>::Output,
        >());
    }

    #[test]
    fn True_And_expr_is_expr() {
        assert!(is_false::<
            <True as And<<True as And<False>>::Output>>::Output,
        >());
        assert!(is_false::<
            <True as And<<False as Or<False>>::Output>>::Output,
        >());
        assert!(is_false::<<True as And<<True as Xor<True>>::Output>>::Output>());
        assert!(is_true::<<True as And<<True as And<True>>::Output>>::Output>());
        assert!(is_true::<<True as And<<False as Or<True>>::Output>>::Output>());
        assert!(is_true::<<True as And<<True as Xor<False>>::Output>>::Output>());
    }
}
