#![feature(proc_macro_hygiene)]
mod compile;
use compile::Compile;
use verified::*;

#[test]
fn can_verify_single_bool_identity_clause() {
    #[verify]
    fn f<B: Bool>()
    where
        _: Verify<{ B }>,
    {
    }

    f::<True>();
}

#[test]
fn can_verify_multiple_bool_identity_clauses() {
    #[verify]
    fn f<A: Bool, B: Bool>()
    where
        _: Verify<{ A }, { B }>,
    {
    }

    f::<True, True>();
}

#[test]
#[allow(non_snake_case)]
fn compilation_tests() {
    Compile(
        "False_is_not_true.rs",
        "
        use verified::*;
        #[verify]
        fn f<B: Bool>()
        where
            _: Verify<{ B }>
        {}

        f::<False>();
        ",
    )
    .and_expect(
        "
        error[E0277]: the trait bound `verified::bool::False: verified::Same<verified::bool::True>` is not satisfied
         --> $DIR/False_is_not_true.rs:9:9
          |
        3 |     #[verify]
          |     --------- required by this bound in `main::f`
        4 |     fn f<B: Bool>()
          |        -
        ...
        9 |     f::<False>();
          |         ^^^^^ the trait `verified::Same<verified::bool::True>` is not implemented for `verified::bool::False`
        "
    );
}
