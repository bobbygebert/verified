extern crate proc_macro;
use proc_macro::TokenStream;
use quote::ToTokens;
use syn::spanned::Spanned;

#[proc_macro_attribute]
pub fn verify(_attr: TokenStream, item: TokenStream) -> TokenStream {
    match generate_verifiable_item(item) {
        Ok(verfiable_item) => verfiable_item,
        Err(e) => e.to_compile_error().into(),
    }
}

fn generate_verifiable_item(item: TokenStream) -> syn::Result<TokenStream> {
    let verified_fn: VerifiedFn = syn::parse(item.clone())?;
    Ok(verified_fn.item.into_token_stream().into())
}

#[derive(Debug)]
struct VerifiedFn {
    item: syn::ItemFn,
}

impl syn::parse::Parse for VerifiedFn {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let mut item: syn::ItemFn = input.parse()?;
        let where_clause = item.sig.generics.make_where_clause();
        let (mut predicates, inferred_bounds): (Vec<_>, Vec<_>) = where_clause
            .clone()
            .predicates
            .into_iter()
            .partition(|clause| match clause {
                syn::WherePredicate::Type(syn::PredicateType {
                    bounded_ty: syn::Type::Infer(_),
                    ..
                }) => false,
                _ => true,
            });

        if inferred_bounds.len() > 1 {
            return Err(syn::Error::new(
                inferred_bounds[1].span(),
                "did not expect to find second `Verify` bound",
            ));
        }

        let bounds = if let Some(syn::WherePredicate::Type(syn::PredicateType { bounds, .. })) =
            inferred_bounds.first()
        {
            Ok(bounds)
        } else {
            Err(syn::Error::new(
                where_clause.span(),
                "expected `_: Verify<_>`",
            ))
        }?;

        let syn::TraitBound { ref path, .. } =
            if let Some(syn::TypeParamBound::Trait(bound)) = bounds.first() {
                Ok(bound)
            } else {
                Err(syn::Error::new(where_clause.span(), "expected `Verify<_>`"))
            }?;

        let generics = if path.segments.len() == 1
            && path.segments.last().unwrap().ident.to_string() == "Verify"
        {
            Ok(&path.segments.last().unwrap().arguments)
        } else {
            Err(syn::Error::new(path.span(), "expected `Verify<_>`"))
        }?;

        let clauses =
            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                ref args,
                ..
            }) = generics
            {
                Ok(args)
            } else {
                Err(syn::Error::new(generics.span(), "expected `<_>`"))
            }?;

        let logic: Logic = syn::parse2(clauses.to_token_stream())?;
        predicates.extend(logic.clauses.into_iter().map(|clause| {
            syn::WherePredicate::Type(syn::PredicateType {
                bounded_ty: clause.0,
                bounds: syn::parse_quote! { Same<True> },
                lifetimes: Default::default(),
                colon_token: Default::default(),
            })
        }));
        where_clause.predicates = std::iter::FromIterator::from_iter(predicates);

        Ok(Self { item })
    }
}

impl quote::ToTokens for VerifiedFn {
    fn to_tokens(&self, out: &mut proc_macro2::TokenStream) {
        self.item.to_tokens(out)
    }
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
    use super::*;

    macro_rules! parse_test {
        (
            parse: $type_in:ty {
                $in:item
            },
            expect: $type_out:ty {
                $out:item
            },
        ) => {
            let code_in: $type_in = syn::parse_quote! {
                $in
            };
            let code_out: $type_out = syn::parse2(code_in.into_token_stream()).unwrap();
            let expected: $type_out = syn::parse_quote! {
                $out
            };
            assert_eq!(code_out, expected);
        };
    }

    #[test]
    #[allow(non_snake_case)]
    fn Bool_identity_clause_is_converted_bound_of_Same_True() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<B: Bool>()
                where
                    _: Verify<{ B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<B: Bool>()
                where
                    B: Same<True>
                {
                }
            },
        }
    }

    #[test]
    #[allow(non_snake_case)]
    fn Multiple_Bool_identity_clauses_are_converted_to_multiple_bounds() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Bool, B: Bool>()
                where
                    _: Verify<{ A }, { B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Bool, B: Bool>()
                where
                    A: Same<True>,
                    B: Same<True>
                {
                }
            },
        }
    }
}
