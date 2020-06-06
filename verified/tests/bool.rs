mod compile;
use compile::Compile;

#[test]
#[allow(non_snake_case)]
fn compilation_tests() {
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
