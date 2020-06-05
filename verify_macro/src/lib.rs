extern crate proc_macro;
use proc_macro::{TokenStream, TokenTree};
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
        .collect::<Vec<_>>();

    // Verify<
    let _verify_ident = tokens.next();
    let _colon = tokens.next();
    let _left_angle = tokens.next();

    // {},..
    let logic: Logic = syn::parse(std::iter::FromIterator::from_iter(
        tokens.by_ref().take_while(|t| match t {
            // >
            TokenTree::Punct(p) if p.to_string() == ">" => false,
            _ => true,
        }),
    ))?;

    // ,{}
    let block = match tokens.next() {
        Some(TokenTree::Punct(p)) if p.to_string() == "," => tokens.next().into_iter(),
        t => t.into_iter(),
    };

    function_tokens.extend(tokens.chain(block));

    // Extend the where clause with trait-bound-encoded logical clauses.
    let mut item: syn::ItemFn = syn::parse(std::iter::FromIterator::from_iter(function_tokens))?;
    let where_clause = item.sig.generics.make_where_clause();
    where_clause
        .predicates
        .extend(logic.clauses.into_iter().map(|clause| {
            syn::WherePredicate::Type(syn::PredicateType {
                bounded_ty: clause.0,
                bounds: syn::parse_quote! { Same<True> },
                lifetimes: Default::default(),
                colon_token: Default::default(),
            })
        }));

    Ok(item.into_token_stream().into())
}

#[derive(Debug)]
struct Logic {
    clauses: Vec<Clause>,
}

impl syn::parse::Parse for Logic {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let clauses =
            syn::punctuated::Punctuated::<Clause, syn::Token!(,)>::parse_terminated(input)?
                .into_iter()
                .collect();
        Ok(Self { clauses })
    }
}

#[derive(Debug)]
struct Clause(syn::Type);

impl syn::parse::Parse for Clause {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let clause;
        let _ = syn::braced!(clause in input);
        Ok(Self(clause.parse()?))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
