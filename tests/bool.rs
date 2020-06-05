pub struct Compile(&'static str, &'static str);

impl Compile {
    pub fn and_expect(self, expected_error: &'static str) {
        let test_dir = tempfile::tempdir().unwrap();

        let code_filename = test_dir.as_ref().join(self.0);
        let mut code_file = std::fs::File::create(&code_filename).unwrap();
        std::io::Write::write_all(
            &mut code_file,
            format!(
                "fn main() {{\n{}\n}}",
                textwrap::indent(&textwrap::dedent(&self.1[1..]), "    ")
            )
            .as_bytes(),
        )
        .unwrap();

        let err_filename = test_dir
            .as_ref()
            .join(format!("{}.stderr", &self.0[..self.0.len() - 3]));
        let mut err_file = std::fs::File::create(&err_filename).unwrap();
        std::io::Write::write_all(
            &mut err_file,
            textwrap::dedent(&expected_error[1..]).as_bytes(),
        )
        .unwrap();
        trybuild::TestCases::new().compile_fail(code_filename);
    }
}

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
