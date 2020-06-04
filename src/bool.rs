mod internal {
    pub trait Choice<A, B> {}
    impl Choice<super::False, super::True> for super::True {}
    impl Choice<super::False, super::True> for super::False {}
}

pub trait Bool: internal::Choice<False, True> {}

#[derive(Debug, Eq, PartialEq)]
pub struct True;
impl Bool for True {}

#[derive(Debug, Eq, PartialEq)]
pub struct False;
impl Bool for False {}

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

#[cfg(test)]
mod tests {
    use super::*;
    use std::convert::TryFrom;

    #[test]
    #[allow(non_snake_case)]
    fn True_and_False_are_Bools() {
        fn is_bool<B: Bool>(_: B) -> bool {
            true
        }
        assert!(is_bool(True));
        assert!(is_bool(False));
    }

    #[test]
    #[allow(non_snake_case)]
    fn bool_from_True_returns_true() {
        assert!(bool::from(True));
    }

    #[test]
    #[allow(non_snake_case)]
    fn bool_from_False_returns_false() {
        assert!(!bool::from(False));
    }

    #[test]
    #[allow(non_snake_case)]
    fn True_try_from_true_is_true() {
        assert_eq!(True::try_from(true), Ok(True));
    }

    #[test]
    #[allow(non_snake_case)]
    fn True_try_from_true_is_Err() {
        assert_eq!(True::try_from(false), Err("cannot convert false into True"));
    }

    #[test]
    #[allow(non_snake_case)]
    fn False_try_from_true_is_true() {
        assert_eq!(False::try_from(false), Ok(False));
    }

    #[test]
    #[allow(non_snake_case)]
    fn False_try_from_true_is_Err() {
        assert_eq!(False::try_from(true), Err("cannot convert true into False"));
    }
}
