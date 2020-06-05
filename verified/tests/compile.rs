pub struct Compile(pub &'static str, pub &'static str);

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
