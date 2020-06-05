extern crate proc_macro;
use proc_macro::{Span, TokenStream, TokenTree};
use quote::ToTokens;

#[proc_macro_attribute]
pub fn verify(_attr: TokenStream, item: TokenStream) -> TokenStream {
    match generate_verifiable_item(item) {
        Ok(verfiable_item) => verfiable_item,
        Err(e) => e.to_compile_error().into(),
    }
}

fn generate_verifiable_item(item: TokenStream) -> syn::Result<TokenStream> {
    let mut tokens = item.into_iter();

    let mut function_tokens = tokens
        .by_ref()
        .take_while(|tt| match tt {
            proc_macro::TokenTree::Ident(ident) if ident.to_string() == "_" => false,
            _ => true,
        })
        .chain(
            vec![
                TokenTree::Ident(proc_macro::Ident::new(
                    "ForAll",
                    proc_macro::Span::call_site(),
                )),
                TokenTree::Punct(proc_macro::Punct::new(':', proc_macro::Spacing::Alone)),
                TokenTree::Ident(proc_macro::Ident::new("Into", Span::call_site())),
                TokenTree::Punct(proc_macro::Punct::new('<', proc_macro::Spacing::Alone)),
                TokenTree::Ident(proc_macro::Ident::new("ForAll", Span::call_site())),
                TokenTree::Punct(proc_macro::Punct::new('>', proc_macro::Spacing::Alone)),
            ]
            .into_iter(),
        )
        .collect::<Vec<_>>();

    // Verify<
    let _verify_ident = tokens.next();
    let _colon = tokens.next();
    let _left_angle = tokens.next();

    // {},..
    let _logic: Logic = syn::parse(std::iter::FromIterator::from_iter(
        tokens.next().into_iter(),
    ))?;

    // >
    let _right_angle = tokens.next();

    function_tokens.extend(tokens);

    let item: syn::Item = syn::parse(std::iter::FromIterator::from_iter(function_tokens))?;
    Ok(item.into_token_stream().into())
}

#[derive(Debug)]
struct Logic {}

impl syn::parse::Parse for Logic {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let _ = syn::punctuated::Punctuated::<syn::Block, syn::Token!(,)>::parse_terminated(input)?;
        Ok(Self {})
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
