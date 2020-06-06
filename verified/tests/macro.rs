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
fn can_verify_bool_equality_clause() {
    #[verify]
    fn f<A: Bool, B: Bool>()
    where
        _: Verify<{ A == B }>,
    {
    }
    f::<False, False>();
}

#[test]
fn can_verify_bool_and_clause() {
    #[verify]
    fn f<A: Bool, B: Bool>()
    where
        _: Verify<{ A && B }, { A & B }>,
    {
    }
    f::<True, True>();
}

#[test]
fn can_verify_bool_or_clause() {
    #[verify]
    fn f<A: Bool, B: Bool>()
    where
        _: Verify<{ A || B }, { A | B }>,
    {
    }
    f::<False, True>();
    f::<True, False>();
    f::<True, True>();
}

#[test]
fn can_verify_bool_xor_clause() {
    #[verify]
    fn f<A: Bool, B: Bool>()
    where
        _: Verify<{ A ^ B }>,
    {
    }
    f::<False, True>();
    f::<True, False>();
}

#[test]
fn can_verify_bool_not_clause() {
    #[verify]
    fn f<B: Bool>()
    where
        _: Verify<{ !B }>,
    {
    }
    f::<False>();
}

#[test]
#[ignore] // TODO: figure out how to make this test pass in automation.
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

    Compile(
        "Error_when_no_Verify_clause_found.rs",
        "
        use verified::*;
        #[verify]
        fn _f<B>()
        where
            B: Bool
        {}
        ",
    )
    .and_expect(
        "
        error: expected `_: Verify<_>`
         --> $DIR/Error_when_no_Verify_clause_found.rs:5:5
          |
        5 | /     where
        6 | |         B: Bool
          | |_______________^
        ",
    );

    Compile(
        "Error_when_multiple_inferred_bounds_are_supplied.rs",
        "
        use verified::*;
        #[verify]
        fn _f<B: Bool>()
        where
            _: Verify<{ B }>,
            _: Verify<{ B }>,
        {}
        ",
    )
    .and_expect(
        "
        error: did not expect to find second `Verify` bound
         --> $DIR/Error_when_multiple_inferred_bounds_are_supplied.rs:7:9
          |
        7 |         _: Verify<{ B }>,
          |         ^^^^^^^^^^^^^^^^
        ",
    );

    Compile(
        "Error_when_inferred_bound_is_not_Verify.rs",
        "
        use verified::*;
        #[verify]
        fn _f<B: Bool>()
        where
            _: SomethingElse<{ B }>,
        {}
        ",
    )
    .and_expect(
        "
        error: expected `Verify<_>`
         --> $DIR/Error_when_inferred_bound_is_not_Verify.rs:6:12
          |
        6 |         _: SomethingElse<{ B }>,
          |            ^^^^^^^^^^^^^^^^^^^^
        ",
    );

    Compile(
        "Error_when_clauses_are_not_angle_bracketed.rs",
        "
        use verified::*;
        #[verify]
        fn _f<B: Bool>()
        where
            _: Verify(B),
        {}
        ",
    )
    .and_expect(
        "
        error: expected `<_>`
         --> $DIR/Error_when_clauses_are_not_angle_bracketed.rs:6:18
          |
        6 |         _: Verify(B),
          |                  ^^^
        ",
    );

    Compile(
        "Error_on_unsupported_expression_in_clause.rs",
        "
        use verified::*;
        #[verify]
        fn _f<A: Bool, B: Bool>()
        where
            _: Verify<{ A * B }>,
        {}
        ",
    )
    .and_expect(
        "
        error: unsupported logical expression
         --> $DIR/Error_on_unsupported_expression_in_clause.rs:6:21
          |
        6 |         _: Verify<{ A * B }>,
          |                     ^^^^^
        ",
    );
}
