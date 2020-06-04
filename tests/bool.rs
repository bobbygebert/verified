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
        );
        trybuild::TestCases::new().compile_fail(code_filename);
    }
}

#[test]
#[allow(non_snake_case)]
fn Bool_may_not_be_implemented() {
    Compile(
        "example.rs",
        "
        use verified::bool::Bool;
        struct Foo;
        impl Bool for Foo {}
        ",
    )
    .and_expect(
        "
        error[E0277]: the trait bound `main::Foo: verified::bool::internal::Choice<verified::bool::False, verified::bool::True>` is not satisfied
         --> $DIR/example.rs:4:10
          |
        4 |     impl Bool for Foo {}
          |          ^^^^ the trait `verified::bool::internal::Choice<verified::bool::False, verified::bool::True>` is not implemented for `main::Foo`
        "
    );
}
