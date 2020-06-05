extern crate proc_macro;
use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn verify(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
