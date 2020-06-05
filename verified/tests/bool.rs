mod compile;
use compile::Compile;

#[test]
#[allow(non_snake_case)]
fn compilation_tests() {
    Compile(
        "Bool_may_not_be_implemented.rs",
        "
        use verified::bool::Bool;
        #[derive(Default)]
        struct Foo;
        impl Bool for Foo {}
        ",
    )
    .and_expect(
        "
        error[E0277]: the trait bound `main::Foo: verified::bool::internal::Choice<verified::bool::False, verified::bool::True>` is not satisfied
         --> $DIR/Bool_may_not_be_implemented.rs:5:10
          |
        5 |     impl Bool for Foo {}
          |          ^^^^ the trait `verified::bool::internal::Choice<verified::bool::False, verified::bool::True>` is not implemented for `main::Foo`
        "
    );

    Compile(
        "False_and_True_are_incompatible.rs",
        "
        use verified::bool::*;
        let _: True = False;
        ",
    )
    .and_expect(
        "
        error[E0308]: mismatched types
         --> $DIR/False_and_True_are_incompatible.rs:3:19
          |
        3 |     let _: True = False;
          |            ----   ^^^^^ expected struct `verified::bool::True`, found struct `verified::bool::False`
          |            |
          |            expected due to this
        "
    );
}
