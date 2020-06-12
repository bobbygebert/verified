mod compile;
use compile::Compile;
use verified::*;

const TRUE: B1 = B1;
const FALSE: B0 = B0;

#[test]
fn can_verify_single_bool_identity_clause() {
    #[verify]
    fn f<B: Bit>(_: B)
    where
        _: Verify<{ B }>,
    {
    }
    f(TRUE);
}

#[test]
fn can_verify_multiple_bool_identity_clauses() {
    #[verify]
    fn f<A: Bit, B: Bit>(_: A, _: B)
    where
        _: Verify<{ A }, { B }>,
    {
    }
    f(TRUE, TRUE);
}

#[test]
fn can_verify_bool_equality_clause() {
    #[verify]
    fn f<A: Bit, B: Bit>(_: A, _: B)
    where
        _: Verify<{ A == B }>,
    {
    }
    f(FALSE, FALSE);
}

#[test]
fn can_verify_bool_and_clause() {
    #[verify]
    fn f<A: Bit, B: Bit>(_: A, _: B)
    where
        _: Verify<{ A & B }>,
    {
    }
    f(TRUE, TRUE);
}

#[test]
fn can_verify_bool_or_clause() {
    #[verify]
    fn f<A: Bit, B: Bit>(_: A, _: B)
    where
        _: Verify<{ A | B }>,
    {
    }
    f(FALSE, TRUE);
    f(TRUE, TRUE);
    f(TRUE, TRUE);
}

#[test]
fn can_verify_bool_xor_clause() {
    #[verify]
    fn f<A: Bit, B: Bit>(_: A, _: B)
    where
        _: Verify<{ A ^ B }>,
    {
    }
    f(FALSE, TRUE);
    f(TRUE, FALSE);
}

#[test]
fn can_verify_bool_not_clause() {
    #[verify]
    fn f<B: Bit>(_: B)
    where
        _: Verify<{ !B }>,
    {
    }
    f(FALSE);
}

#[test]
fn can_verify_parenthesised_clause() {
    #[verify]
    fn f<B: Bit>(_: B)
    where
        _: Verify<{ (B) }>,
    {
    }
    f(TRUE);
}

#[test]
fn can_verify_nested_binary_clauses() {
    #[verify]
    fn f<A: Bit, B: Bit, C: Bit>(_: A, _: B, _: C)
    where
        _: Verify<{ (A & (B | C)) == C }>,
    {
    }
    f(TRUE, FALSE, FALSE);
}

#[test]
fn can_verify_nested_unary_clause() {
    #[verify]
    fn f<A: Bit, B: Bit>(_: A, _: B)
    where
        _: Verify<{ !(A & B) }>,
    {
    }
    f(TRUE, FALSE);
}

#[test]
fn can_verify_bool_less_than_clauses() {
    #[verify]
    fn f<A: Bit, B: Bit>(_: A, _: B)
    where
        _: Verify<{ A < B }>,
    {
    }
    f(FALSE, TRUE);
}

#[test]
fn can_verify_bool_greater_than_clauses() {
    #[verify]
    fn f<A: Bit, B: Bit>(_: A, _: B)
    where
        _: Verify<{ A > B }>,
    {
    }
    f(TRUE, FALSE);
}

#[test]
fn can_verify_bool_less_equal_clauses() {
    #[verify]
    fn f<A: Bit, B: Bit>(_: A, _: B)
    where
        _: Verify<{ A <= B }>,
    {
    }
    f(FALSE, TRUE);
    f(FALSE, FALSE);
    f(TRUE, TRUE);
}

#[test]
fn can_verify_bool_greater_equal_clauses() {
    #[verify]
    fn f<A: Bit, B: Bit>(_: A, _: B)
    where
        _: Verify<{ A >= B }>,
    {
    }
    f(TRUE, FALSE);
    f(FALSE, FALSE);
    f(TRUE, TRUE);
}

#[test]
fn can_verify_bool_not_equal_clauses() {
    #[verify]
    fn f<A: Bit, B: Bit>(_: A, _: B)
    where
        _: Verify<{ A != B }>,
    {
    }
    f(TRUE, FALSE);
}

#[test]
fn can_verify_usize_bitand_clauses() {
    #[verify]
    fn f<A: Unsigned, B: Unsigned, C: Unsigned>(_: A, _: B, _: C)
    where
        _: Verify<{ (A & B) == C }>,
    {
    }
    f(U2::default(), U1::default(), U0::default());
    f(U2::default(), U2::default(), U2::default());
}

#[test]
fn can_verify_usize_bitor_clauses() {
    #[verify]
    fn f<A: Unsigned, B: Unsigned, C: Unsigned>(_: A, _: B, _: C)
    where
        _: Verify<{ (A | B) == C }>,
    {
    }
    f(U2::default(), U1::default(), U3::default());
    f(U2::default(), U2::default(), U2::default());
}

#[test]
fn can_verify_usize_bitxor_clauses() {
    #[verify]
    fn f<A: Unsigned, B: Unsigned, C: Unsigned>(_: A, _: B, _: C)
    where
        _: Verify<{ (A ^ B) == C }>,
    {
    }
    f(U2::default(), U1::default(), U3::default());
    f(U0::default(), U0::default(), U0::default());
    f(U2::default(), U2::default(), U0::default());
}

#[test]
fn can_verify_bool_literals() {
    #[verify]
    fn f<B: Bit>(_: B)
    where
        _: Verify<{ B == false }, { B == !true }>,
    {
    }
    f(FALSE);
}

#[test]
fn can_verify_usize_literals() {
    #[verify]
    fn f<Six: Unsigned, Zero: Unsigned>(_: Six, _: Zero)
    where
        _: Verify<{ Six == 6 }, { Zero == 0 }>,
    {
    }
    f(U6::default(), U0::default());
}

#[test]
fn can_verify_usize_addition_clauses() {
    #[verify]
    fn f<A: Unsigned, B: Unsigned>(_: A, _: B)
    where
        _: Verify<{ (A + B) == 5 }>,
    {
    }
    f(U2::default(), U3::default());
}

#[test]
fn can_verify_usize_subtraction_clauses() {
    #[verify]
    fn f<A: Unsigned, B: Unsigned>(_: A, _: B)
    where
        _: Verify<{ (A - B) == 3 }>,
    {
    }
    f(U5::default(), U2::default());
}

#[test]
fn can_verify_usize_multiplication_clauses() {
    #[verify]
    fn f<A: Unsigned, B: Unsigned>(_: A, _: B)
    where
        _: Verify<{ (A * B) == 8 }>,
    {
    }
    f(U2::default(), U4::default());
}

#[test]
fn can_verify_usize_division_clauses() {
    #[verify]
    fn f<A: Unsigned, B: Unsigned>(_: A, _: B)
    where
        _: Verify<{ (A / B) == 4 }>,
    {
    }
    f(U8::default(), U2::default());
}

#[test]
fn can_verify_usize_rem_clauses() {
    #[verify]
    fn f<A: Unsigned, B: Unsigned>(_: A, _: B)
    where
        _: Verify<{ (A % B) == 2 }>,
    {
    }
    f(U8::default(), U3::default());
}

#[test]
fn can_verify_usize_shl_clauses() {
    #[verify]
    fn f<A: Unsigned>(_: A)
    where
        _: Verify<{ (A << 2) == 4 }>,
    {
    }
    f(U1::default());
}

#[test]
fn can_verify_usize_shr_clauses() {
    #[verify]
    fn f<A: Unsigned>(_: A)
    where
        _: Verify<{ (A >> 2) == 1 }>,
    {
    }
    f(U4::default());
}

#[test]
fn can_verify_usize_less_than_clauses() {
    #[verify]
    fn f<A: Unsigned, B: Unsigned>(_: A, _: B)
    where
        _: Verify<{ A < B }>,
    {
    }
    f(U2::default(), U3::default());
}

#[test]
fn can_verify_usize_greater_than_clauses() {
    #[verify]
    fn f<A: Unsigned, B: Unsigned>(_: A, _: B)
    where
        _: Verify<{ A > B }>,
    {
    }
    f(U3::default(), U2::default());
}

#[test]
fn can_verify_usize_less_equal_clauses() {
    #[verify]
    fn f<A: Unsigned, B: Unsigned>(_: A, _: B)
    where
        _: Verify<{ A <= B }>,
    {
    }
    f(U2::default(), U3::default());
    f(U3::default(), U3::default());
}

#[test]
fn can_verify_usize_greater_equal_clauses() {
    #[verify]
    fn f<A: Unsigned, B: Unsigned>(_: A, _: B)
    where
        _: Verify<{ A >= B }>,
    {
    }
    f(U3::default(), U2::default());
    f(U3::default(), U3::default());
}

#[test]
fn can_verify_usize_not_equal_clauses() {
    #[verify]
    fn f<A: Unsigned, B: Unsigned>(_: A, _: B)
    where
        _: Verify<{ A != B }>,
    {
    }
    f(U3::default(), U2::default());
}

#[test]
fn can_verify_result_simple_addition_in_function_body_lhs() {
    #[verify]
    fn f<A: Unsigned, B: Unsigned, C: Unsigned>(a: A, b: B) -> C
    where
        _: Verify<{ A + B == C }>,
    {
        a + b
    }
    let _: U3 = f(U1::default(), U2::default());
}

#[test]
fn can_verify_result_simple_addition_in_function_body_rhs() {
    #[verify]
    fn f<A: Unsigned, B: Unsigned, C: Unsigned>(a: A, b: B) -> C
    where
        _: Verify<{ C == A + B }>,
    {
        a + b
    }
    let _: U3 = f(U1::default(), U2::default());
}

#[test]
fn can_verify_bools_without_braces() {
    #[verify]
    fn f<B: Bit>(_: B)
    where
        _: Verify<B>,
    {
    }
    f(TRUE);
}

#[test]
#[ignore] // TODO: figure out how to make this test pass in automation.
#[allow(non_snake_case)]
fn compilation_tests() {
    // TODO: Fix error handling.

    Compile(
        "Error_when_multiple_inferred_bounds_are_supplied.rs",
        "
        use verified::*;
        #[verify]
        fn _f<B: Bit>()
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
        fn _f<B: Bit>()
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
        "Error_on_unsupported_expression_in_clause.rs",
        "
        use verified::*;
        #[verify]
        fn _f<A: Bit, B: Bit>()
        where
            _: Verify<{ A *= B }>,
        {}
        ",
    )
    .and_expect(
        "
        error: unsupported logical expression
         --> $DIR/Error_on_unsupported_expression_in_clause.rs:6:21
          |
        6 |         _: Verify<{ A *= B }>,
          |                     ^^^^^^
        ",
    );

    Compile(
        "Error_on_unsupported_literal_in_clause.rs",
        "
        use verified::*;
        #[verify]
        fn _f<N: Unsigned>()
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
