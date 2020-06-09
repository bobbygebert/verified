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
fn can_verify_parenthesised_clause() {
    #[verify]
    fn f<B: Bool>()
    where
        _: Verify<{ (B) }>,
    {
    }
    f::<True>();
}

#[test]
fn can_verify_nested_binary_clauses() {
    #[verify]
    fn f<A: Bool, B: Bool, C: Bool>()
    where
        _: Verify<{ (A && (B || C)) == C }>,
    {
    }
    f::<True, False, False>();
}

#[test]
fn can_verify_nested_unary_clause() {
    #[verify]
    fn f<A: Bool, B: Bool>()
    where
        _: Verify<{ !(A && B) }>,
    {
    }
    f::<True, False>();
}

#[test]
fn can_verify_bool_not_equal_clauses() {
    #[verify]
    fn f<A: Bool, B: Bool>()
    where
        _: Verify<{ A != B }>,
    {
    }
    f::<True, False>();
}

#[test]
fn can_verify_usize_bitand_clauses() {
    #[verify]
    fn f<A: Usize, B: Usize, C: Usize>()
    where
        _: Verify<{ (A & B) == C }>,
    {
    }
    f::<U<T, B1>, U<T, B0>, U<T, B0>>();
    f::<U<T, B1>, U<T, B1>, U<T, B1>>();
}

#[test]
fn can_verify_usize_bitor_clauses() {
    #[verify]
    fn f<A: Usize, B: Usize, C: Usize>()
    where
        _: Verify<{ (A | B) == C }>,
    {
    }
    f::<U<T, B1>, U<T, B0>, U<T, B1>>();
    f::<U<T, B0>, U<T, B0>, U<T, B0>>();
}

#[test]
fn can_verify_usize_bitxor_clauses() {
    #[verify]
    fn f<A: Usize, B: Usize, C: Usize>()
    where
        _: Verify<{ (A ^ B) == C }>,
    {
    }
    f::<U<T, B1>, U<T, B0>, U<T, B1>>();
    f::<U<T, B0>, U<T, B0>, U<T, B0>>();
    f::<U<T, B1>, U<T, B1>, U<T, B0>>();
}

#[test]
fn can_verify_bool_literals() {
    #[verify]
    fn f<B: Bool>()
    where
        _: Verify<{ B == false }, { B == !true }>,
    {
    }
    f::<False>();
}

#[test]
fn can_verify_usize_literals() {
    #[verify]
    fn f<Six: Usize, Zero: Usize>()
    where
        _: Verify<{ Six == 6 }, { Zero == 0 }>,
    {
    }
    f::<U<U<U<T, B1>, B1>, B0>, U<T, B0>>();
}

#[test]
fn can_verify_usize_addition_clauses() {
    #[verify]
    fn f<A: Usize, B: Usize>()
    where
        _: Verify<{ (A + B) == 5 }>,
    {
    }
    f::<U<U<T, B1>, B0>, U<U<T, B1>, B1>>();
}

#[test]
fn can_verify_usize_less_than_clauses() {
    #[verify]
    fn f<A: Usize, B: Usize>()
    where
        _: Verify<{ A < B }>,
    {
    }
    f::<U<U<T, B1>, B0>, U<U<T, B1>, B1>>();
}

#[test]
fn can_verify_usize_greater_than_clauses() {
    #[verify]
    fn f<A: Usize, B: Usize>()
    where
        _: Verify<{ A > B }>,
    {
    }
    f::<U<U<T, B1>, B1>, U<U<T, B1>, B0>>();
}

#[test]
fn can_verify_usize_less_equal_clauses() {
    #[verify]
    fn f<A: Usize, B: Usize>()
    where
        _: Verify<{ A <= B }>,
    {
    }
    f::<U<U<T, B1>, B0>, U<U<T, B1>, B1>>();
    f::<U<U<T, B1>, B1>, U<U<T, B1>, B1>>();
}

#[test]
fn can_verify_usize_greater_equal_clauses() {
    #[verify]
    fn f<A: Usize, B: Usize>()
    where
        _: Verify<{ A >= B }>,
    {
    }
    f::<U<U<T, B1>, B1>, U<U<T, B1>, B0>>();
    f::<U<U<T, B1>, B1>, U<U<T, B1>, B1>>();
}

#[test]
fn can_verify_usize_not_equal_clauses() {
    #[verify]
    fn f<A: Usize, B: Usize>()
    where
        _: Verify<{ A != B }>,
    {
    }
    f::<U<U<T, B1>, B1>, U<U<T, B1>, B0>>();
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
        error[E0271]: type mismatch resolving `<verified::bool::False as verified::Same<verified::bool::True>>::Output == verified::bool::True`
         --> $DIR/False_is_not_true.rs:9:5
          |
        3 |     #[verify]
          |     --------- required by this bound in `main::f`
        4 |     fn f<B: Bool>()
          |        -
        ...
        9 |     f::<False>();
          |     ^^^^^^^^^^ expected struct `verified::bool::True`, found struct `verified::bool::False`
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

    Compile(
        "Error_on_unsupported_literal_in_clause.rs",
        "
        use verified::*;
        #[verify]
        fn _f<N: Usize>()
        where
            _: Verify<{ N == \"abc\" }>,
        {}
        ",
    )
    .and_expect(
        "
        error: only bool and int literals are supported here
         --> $DIR/Error_on_unsupported_literal_in_clause.rs:6:26
          |
        6 |         _: Verify<{ N == \"abc\" }>,
          |                          ^^^^^
        ",
    );
}
