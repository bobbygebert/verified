use verify_macro::verify;

#[test]
fn it() {
    #[verify]
    fn _f() {}
}
