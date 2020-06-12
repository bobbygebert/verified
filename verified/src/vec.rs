use crate::*;
use std::vec as raw;
use std::vec::Vec as Raw;

#[macro_export]
macro_rules! vec {
    (count $(,)?) => { $crate::U0 };
    (count $e:expr$(, $tail:expr)*$(,)?) => { <$crate::U1 as $crate::Add<$crate::vec!(count $($tail),*)>>::Output };
    ($($e:expr),*$(,)?) => {
        $crate::vec::Vec(
            <$crate::vec!(count $($e),*)>::new(),
            raw![$($e),*],
        )
    };
}

// TODO: derive other ops
#[derive(Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
pub struct Vec<Size: Unsigned, Element>(Size, Raw<Element>);

impl<Element> Vec<U0, Element> {
    pub fn new() -> Self {
        Vec(U0::default(), raw![])
    }
}

impl<Size: Unsigned, Element> Vec<Size, Element> {
    // TODO: Implement support for logic in types outside of where clauses.
    #[verify]
    pub fn append<OtherSize: Unsigned, NewSize: Unsigned>(
        self,
        other: Vec<OtherSize, Element>,
    ) -> Vec<NewSize, Element>
    where
        _: Verify<{ NewSize == Size + OtherSize }>,
    {
        self + other
    }

    // TODO: Implement support for logic in types outside of where clauses.
    #[verify]
    pub fn pop<NewSize: Unsigned>(self) -> (Vec<NewSize, Element>, Element)
    where
        _: Verify<{ Size > 0 }, { NewSize == Size - 1 }>,
    {
        self.into()
    }

    // TODO: Implement support for logic in types outside of where clauses.
    #[verify]
    pub fn push<NewSize: Unsigned>(self, e: Element) -> Vec<NewSize, Element>
    where
        _: Verify<{ NewSize == Size + 1 }>,
    {
        (self, e).into()
    }

    // TODO: Implement support for logic in types outside of where clauses.
    #[verify]
    pub fn insert<Index: Unsigned, NewSize: Unsigned>(
        self,
        _: Index,
        e: Element,
    ) -> Vec<NewSize, Element>
    where
        _: Verify<{ NewSize == Size + 1 }>,
    {
        let Self(_, mut v) = self;
        v.insert(Index::to_usize(), e);
        Vec(Default::default(), v)
    }

    // TODO: Implement support for logic in types outside of where clauses.
    #[verify]
    pub fn remove<Index: Unsigned, NewSize: Unsigned>(
        self,
        _: Index,
    ) -> (Vec<NewSize, Element>, Element)
    where
        _: Verify<{ Size > 0 }, { NewSize == Size - 1 }>,
    {
        let Self(_, mut v) = self;
        let e = v.remove(Index::to_usize());
        (Vec(Default::default(), v), e)
    }

    // TODO: Implement support for logic in types outside of where clauses.
    #[verify]
    pub fn truncate<NewSize: Unsigned>(self, _: NewSize) -> Vec<NewSize, Element>
    where
        _: Verify<{ NewSize < Size }>,
    {
        let Self(_, mut v) = self;
        v.truncate(NewSize::to_usize());
        Vec(Default::default(), v)
    }
}

impl<Size: Unsigned, Element: Clone> Vec<Size, Element> {
    // TODO: Implement support for logic in types outside of where clauses.
    pub fn resize<NewSize: Unsigned>(self, _: NewSize, default: Element) -> Vec<NewSize, Element> {
        let Self(_, mut v) = self;
        v.resize(NewSize::to_usize(), default);
        Vec(Default::default(), v)
    }
}

#[verify]
impl<Size: Unsigned, Element, I: Unsigned> std::ops::Index<I> for Vec<Size, Element>
where
    _: Verify<{ I < Size }>,
{
    type Output = Element;
    fn index(&self, _: I) -> &Self::Output {
        &self.1[I::to_usize()]
    }
}

#[verify]
impl<Size: Unsigned, Element, I: Unsigned> std::ops::IndexMut<I> for Vec<Size, Element>
where
    _: Verify<{ I < Size }>,
{
    fn index_mut(&mut self, _: I) -> &mut Self::Output {
        &mut self.1[I::to_usize()]
    }
}

impl<Size: Unsigned, Element> std::convert::TryFrom<Raw<Element>> for Vec<Size, Element> {
    type Error = String;
    fn try_from(raw: Raw<Element>) -> Result<Self, Self::Error> {
        let expected = Size::to_usize();
        let actual = raw.len();
        if actual == expected {
            Ok(Self(Size::default(), raw))
        } else {
            Err(format!(
                "expected length of {} but got {}",
                expected, actual
            ))
        }
    }
}

impl<SizeL: Unsigned, SizeR: Unsigned, Element> std::ops::Add<Vec<SizeR, Element>>
    for Vec<SizeL, Element>
where
    SizeL: Add<SizeR>,
    <SizeL as Add<SizeR>>::Output: Unsigned,
{
    type Output = Vec<<SizeL as Add<SizeR>>::Output, Element>;
    fn add(self, Vec(os, mut ov): Vec<SizeR, Element>) -> Self::Output {
        let Self(s, mut v) = self;
        v.append(&mut ov);
        Vec(s + os, v)
    }
}

#[verify]
impl<Size: Unsigned, NewSize: Unsigned, Element> std::convert::From<(Vec<Size, Element>, Element)>
    for Vec<NewSize, Element>
where
    _: Verify<{ NewSize == Size + 1 }>,
{
    fn from((Vec(_, mut v), e): (Vec<Size, Element>, Element)) -> Self {
        v.push(e);
        Self(Default::default(), v)
    }
}

#[verify]
impl<Size: Unsigned, NewSize: Unsigned, Element> std::convert::From<Vec<Size, Element>>
    for (Vec<NewSize, Element>, Element)
where
    _: Verify<{ Size > 0 }, { NewSize == Size - 1 }>,
{
    fn from(Vec(_, mut v): Vec<Size, Element>) -> Self {
        let e = v.pop().unwrap();
        (Vec(Default::default(), v), e)
    }
}

//  _____         _
// |_   _|__  ___| |_ ___
//   | |/ _ \/ __| __/ __|
//   | |  __/\__ \ |_\__ \
//   |_|\___||___/\__|___/
//  FIGLET: Tests

#[cfg(test)]
mod tests {
    use super::*;

    struct UInts<C0: Fn() -> U0, C1: Fn() -> U1, C2: Fn() -> U2, C3: Fn() -> U3>(C0, C1, C2, C3);
    const U: UInts<fn() -> U0, fn() -> U1, fn() -> U2, fn() -> U3> =
        UInts(|| U0::new(), || U1::new(), || U2::new(), || U3::new());
    macro_rules! U {
        ($i:tt) => {
            U.$i()
        };
    }

    #[test]
    fn default_eq_ord_clone_and_try_from_are_implemented() {
        let new_vec: Vec<U0, usize> = Vec::new();
        let default_vec: Vec<_, usize> = Default::default();
        let from_raw_ok: Result<Vec<U1, usize>, _> = std::convert::TryFrom::try_from(raw![1]);
        let from_raw_err: Result<Vec<U2, usize>, _> = std::convert::TryFrom::try_from(raw![1]);
        assert_eq!(new_vec, default_vec);
        assert!(new_vec <= default_vec);
        assert!(new_vec.clone() <= default_vec);
        assert_eq!(from_raw_ok.ok().unwrap()[U!(0)], 1);
        assert_eq!(
            from_raw_err,
            Err("expected length of 2 but got 1".to_string())
        );
    }

    #[test]
    fn elements_can_be_modified_via_index() {
        let mut v = vec!["a", "b"];
        v[U1::new()] = "c";
        assert_eq!(v[U!(1)], "c");
    }

    #[test]
    fn append_joins_vecs() {
        let a = vec!["a", "b"];
        let b = vec!["c", "d"];

        let c: Vec<U4, _> = a.append(b);

        assert_eq!(c[U!(0)], "a");
        assert_eq!(c[U!(1)], "b");
        assert_eq!(c[U!(2)], "c");
        assert_eq!(c[U!(3)], "d");
    }

    #[test]
    fn push_increments_size_and_adds_element() {
        let v = Vec::new();

        let v = v.push("a");
        let v = v.push("b");
        assert_eq!(v, vec!["a", "b"]);
    }

    #[test]
    fn insert_increments_size_and_adds_element() {
        let v = vec!["a", "c"];
        let v = v.insert(U!(1), "b");
        assert_eq!(v, vec!["a", "b", "c"]);
    }

    #[test]
    fn pop_decrements_size_and_removes_and_returns_element() {
        let v = vec!["a", "b"];
        let (v, popped): (Vec<U1, _>, _) = v.pop();
        assert_eq!(popped, "b");
        assert_eq!(v, vec!["a"]);
    }

    #[test]
    fn remove_decrements_size_and_removes_and_returns_element() {
        let v = vec!["a", "b", "c"];
        let (v, removed): (Vec<U2, _>, _) = v.remove(U!(1));
        assert_eq!(removed, "b");
        assert_eq!(v, vec!["a", "c"]);
    }

    #[test]
    fn resize_can_shorten_vec() {
        let v = vec!["a", "b", "c"];
        let v: Vec<U1, _> = v.resize(U!(1), "");
        assert_eq!(v, vec!["a"]);
    }

    #[test]
    fn resize_can_lengthen_vec() {
        let v = vec!["a"];
        let v: Vec<U3, _> = v.resize(U!(3), "b");
        assert_eq!(v, vec!["a", "b", "b"]);
    }

    #[test]
    fn truncate_shortens_vec() {
        let v = vec!["a", "b", "c"];
        let v: Vec<U1, _> = v.truncate(U!(1));
        assert_eq!(v, vec!["a"]);
    }

    #[test]
    fn add_joins_vecs() {
        let a = vec!["a", "b"];
        let b = vec!["c", "d"];
        assert_eq!(a + b, vec!["a", "b", "c", "d"]);
    }
}
