extern crate proc_macro;
use proc_macro::TokenStream;
use quote::ToTokens;
use std::convert::{TryFrom, TryInto};
use syn::spanned::Spanned;

//                 _  __
// __   _____ _ __(_)/ _|_   _
// \ \ / / _ \ '__| | |_| | | |
//  \ V /  __/ |  | |  _| |_| |
//   \_/ \___|_|  |_|_|  \__, |
//                       |___/
//  FIGLET: verify

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

//  _     _ _                 _
// | |   (_) |_ ___ _ __ __ _| |
// | |   | | __/ _ \ '__/ _` | |
// | |___| | ||  __/ | | (_| | |
// |_____|_|\__\___|_|  \__,_|_|
//  FIGLET: Literal

#[allow(non_snake_case)]
#[proc_macro]
pub fn Literal(arg: TokenStream) -> TokenStream {
    match syn::parse::<syn::Lit>(arg)
        .and_then(Op::try_from)
        .and_then(syn::Type::try_from)
    {
        Ok(item) => item.to_token_stream().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

//  ____                _
// |  _ \ __ _ _ __ ___(_)_ __   __ _
// | |_) / _` | '__/ __| | '_ \ / _` |
// |  __/ (_| | |  \__ \ | | | | (_| |
// |_|   \__,_|_|  |___/_|_| |_|\__, |
//                              |___/
//  FIGLET: Parsing

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

        for clause in logic.clauses.into_iter() {
            predicates.extend(
                TryInto::<Vec<_>>::try_into(clause)?
                    .into_iter()
                    .map(|p| syn::WherePredicate::Type(p)),
            );
        }

        where_clause.predicates = std::iter::FromIterator::from_iter(predicates);

        Ok(Self { item })
    }
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

impl syn::parse::Parse for Clause {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let clause;
        let _ = syn::braced!(clause in input);
        Ok(Self(clause.parse()?))
    }
}

impl quote::ToTokens for VerifiedFn {
    fn to_tokens(&self, out: &mut proc_macro2::TokenStream) {
        self.item.to_tokens(out)
    }
}

//  _____                    _       _   _
// |_   _| __ __ _ _ __  ___| | __ _| |_(_) ___  _ __  ___
//   | || '__/ _` | '_ \/ __| |/ _` | __| |/ _ \| '_ \/ __|
//   | || | | (_| | | | \__ \ | (_| | |_| | (_) | | | \__ \
//   |_||_|  \__,_|_| |_|___/_|\__,_|\__|_|\___/|_| |_|___/
//  FIGLET: Translations

macro_rules! expression {
    (lit: $lit:ident) => {
        syn::Expr::Lit(syn::ExprLit {
            $lit,
            ..
        })
    };
    (path: $path:ident) => {
        syn::Expr::Path(syn::ExprPath {
            $path,
            ..
        })
    };
    ($expr_ty:ident, $expr:ident) => {
        syn::Expr::Unary(syn::ExprUnary {
            op: syn::UnOp::$expr_ty(_),
            $expr,
            ..
        })
    };
    ($expr_ty:ident, $left:ident, $right:ident) => {
        syn::Expr::Binary(syn::ExprBinary {
            $left,
            op: syn::BinOp::$expr_ty(_),
            $right,
            ..
        })
    }
}

macro_rules! predicate {
    ({$($left:tt)*}: {$($right:tt)*}) => {
        syn::PredicateType {
            bounded_ty: syn::parse_quote! { $($left)* },
            bounds: syn::parse_quote! { $($right)* },
            lifetimes: Default::default(),
            colon_token: Default::default(),
        }
    };
}

impl TryFrom<Clause> for Vec<syn::PredicateType> {
    type Error = syn::Error;
    fn try_from(clause: Clause) -> syn::Result<Self> {
        let op: Op = clause.try_into()?;
        Ok(vec![op.clone().try_into()?]
            .into_iter()
            .chain(TryInto::<Self>::try_into(ImplPredicate(op))?.into_iter())
            .collect())
    }
}

impl TryFrom<ImplPredicate> for Vec<syn::PredicateType> {
    type Error = syn::Error;
    fn try_from(ImplPredicate(from): ImplPredicate) -> syn::Result<Self> {
        Ok(match from {
            Op::Op {
                op,
                left,
                right: Some(right),
            } => {
                let left_ty: syn::Type = (*left.clone()).try_into()?;
                let right_ty: syn::Type = (*right.clone()).try_into()?;
                vec![predicate! {{ #left_ty }: { #op<#right_ty> }}]
                    .into_iter()
                    .chain(TryInto::<Self>::try_into(ImplPredicate(*left))?.into_iter())
                    .chain(TryInto::<Self>::try_into(ImplPredicate(*right))?.into_iter())
                    .collect()
            }
            Op::Op {
                op,
                left,
                right: None,
            } => {
                let left_ty: syn::Type = (*left.clone()).try_into()?;
                vec![predicate! {{ #left_ty }: { #op }}]
                    .into_iter()
                    .chain(TryInto::<Self>::try_into(ImplPredicate(*left))?.into_iter())
                    .collect()
            }
            Op::Path(_) => vec![],
        })
    }
}

impl TryFrom<Op> for syn::PredicateType {
    type Error = syn::Error;
    fn try_from(from: Op) -> syn::Result<Self> {
        match from {
            Op::Op { op, left, right } => {
                let left: syn::Type = (*left).try_into()?;
                Ok(match right {
                    Some(right) => {
                        let right: syn::Type = (*right).try_into()?;
                        predicate! {{ #left }: { #op<#right, Output = True> }}
                    }
                    None => predicate! {{ #left }: { #op<Output = True> }},
                })
            }
            Op::Path(path) => Ok(predicate! {{ #path }: { Same<True, Output = True> }}),
        }
    }
}

impl TryFrom<Op> for syn::Type {
    type Error = syn::Error;
    fn try_from(from: Op) -> syn::Result<Self> {
        match from {
            Op::Op { op, left, right } => {
                let left: syn::Type = (*left).try_into()?;
                Ok(match right {
                    Some(right) => {
                        let right: syn::Type = (*right).try_into()?;
                        syn::parse_quote! { <#left as #op<#right>>::Output }
                    }
                    None => syn::parse_quote! { <#left as #op>::Output },
                })
            }
            Op::Path(path) => Ok(syn::parse_quote!(#path)),
        }
    }
}

macro_rules! op {
    ($op:ident, $arg:ident) => {
        Op::Op {
            op: syn::parse_quote!($op),
            left: Box::new(Clause(*$arg).try_into()?),
            right: None,
        }
    };
    ($op:ident, $left:ident, $right:ident) => {
        Op::Op {
            op: syn::parse_quote!($op),
            left: Box::new(Clause(*$left).try_into()?),
            right: Some(Box::new(Clause(*$right).try_into()?)),
        }
    };
}

impl TryFrom<Clause> for Op {
    type Error = syn::Error;
    fn try_from(clause: Clause) -> syn::Result<Self> {
        match clause.0 {
            expression!(Add, left, right) => Ok(op!(Add, left, right)),
            expression!(And, left, right) => Ok(op!(And, left, right)),
            expression!(BitAnd, left, right) => Ok(op!(BitAnd, left, right)),
            expression!(BitOr, left, right) => Ok(op!(BitOr, left, right)),
            expression!(BitXor, left, right) => Ok(op!(BitXor, left, right)),
            expression!(Eq, left, right) => Ok(op!(Same, left, right)),
            expression!(Ge, left, right) => Ok(op!(Ge, left, right)),
            expression!(Gt, left, right) => Ok(op!(Gt, left, right)),
            expression!(Le, left, right) => Ok(op!(Le, left, right)),
            expression!(Lt, left, right) => Ok(op!(Lt, left, right)),
            expression!(Ne, left, right) => Ok(op!(Ne, left, right)),
            expression!(Not, expr) => Ok(op!(Not, expr)),
            expression!(Or, left, right) => Ok(op!(Or, left, right)),
            expression!(lit: lit) => Ok(lit.try_into()?),
            expression!(path: path) => Ok(Op::Path(path)),
            syn::Expr::Paren(syn::ExprParen { expr, .. }) => Ok(Clause(*expr).try_into()?),
            unsupported_expr => Err(syn::Error::new(
                unsupported_expr.span(),
                "unsupported logical expression",
            )),
        }
    }
}

impl TryFrom<syn::Lit> for Op {
    type Error = syn::Error;
    fn try_from(lit: syn::Lit) -> syn::Result<Self> {
        match lit {
            syn::Lit::Bool(syn::LitBool { value, .. }) => Ok(Op::Path(if value {
                syn::parse_quote!(True)
            } else {
                syn::parse_quote!(False)
            })),
            syn::Lit::Int(value) => Ok(Op::Path(match value.base10_parse::<usize>()? {
                0 => syn::parse_quote!(U<T, B0>),
                int => format!("{:b}", int)
                    .bytes()
                    .skip_while(|bit| *bit == '0' as u8)
                    .fold(syn::parse_quote!(T), |msb: syn::Path, bit| {
                        let lsb = syn::Ident::new(&format!("B{}", bit as char), value.span());
                        syn::parse_quote!(U<#msb, #lsb>)
                    }),
            })),
            unsupported_expr => Err(syn::Error::new(
                unsupported_expr.span(),
                "only bool and int literals are supported here",
            )),
        }
    }
}

//  _____
// |_   _|   _ _ __   ___  ___
//   | || | | | '_ \ / _ \/ __|
//   | || |_| | |_) |  __/\__ \
//   |_| \__, | .__/ \___||___/
//       |___/|_|
//  FIGLET: Types

#[derive(Debug)]
struct VerifiedFn {
    item: syn::ItemFn,
}

#[derive(Debug)]
struct Logic {
    clauses: Vec<Clause>,
}

#[derive(Debug)]
struct Clause(syn::Expr);

struct ImplPredicate(Op);

#[derive(Clone, Debug)]
enum Op {
    Op {
        op: syn::Path,
        left: Box<Self>,
        right: Option<Box<Self>>,
    },
    Path(syn::Path),
}

//  _____         _
// |_   _|__  ___| |_ ___
//   | |/ _ \/ __| __/ __|
//   | |  __/\__ \ |_\__ \
//   |_|\___||___/\__|___/
//  FIGLET: Tests

#[cfg(test)]
#[allow(non_snake_case)]
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
            if code_out != expected {
                assert_eq!(
                    code_out.into_token_stream().to_string(),
                    expected.into_token_stream().to_string(),
                );
            }
        };
    }

    #[test]
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
                    B: Same<True, Output = True>
                {
                }
            },
        }
    }

    #[test]
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
                    A: Same<True, Output = True>,
                    B: Same<True, Output = True>
                {
                }
            },
        }
    }

    #[test]
    fn Bool_equality_clause_is_converted_to_Same_bound() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Bool, B: Bool>()
                where
                    _: Verify<{ A == B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Bool, B: Bool>()
                where
                    A: Same<B, Output = True>,
                    A: Same<B>,
                {
                }
            },
        }
    }

    #[test]
    fn Bool_and_clause_is_converted_to_And_bound_on_BinOp() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Bool, B: Bool>()
                where
                    _: Verify<{ A && B }, { A & B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Bool, B: Bool>()
                where
                    A: And<B, Output = True>,
                    A: And<B>,
                    A: BitAnd<B, Output = True>,
                    A: BitAnd<B>,
                {
                }
            },
        }
    }

    #[test]
    fn Bool_or_clause_is_converted_to_Or_bound_on_BinOp() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Bool, B: Bool>()
                where
                    _: Verify<{ A || B }, { A | B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Bool, B: Bool>()
                where
                    A: Or<B, Output = True>,
                    A: Or<B>,
                    A: BitOr<B, Output = True>,
                    A: BitOr<B>,
                {
                }
            },
        }
    }

    #[test]
    fn Bool_xor_clause_is_converted_to_Xor_bound_on_BinOp() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Bool, B: Bool>()
                where
                    _: Verify<{ A ^ B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Bool, B: Bool>()
                where
                    A: BitXor<B, Output = True>,
                    A: BitXor<B>,
                {
                }
            },
        }
    }

    #[test]
    fn Bool_not_clause_is_converted_to_Same_bound_on_BitXor_Output() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<B: Bool>()
                where
                    _: Verify<{ !B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<B: Bool>()
                where
                    B: Not<Output = True>,
                    B: Not,
                {
                }
            },
        }
    }

    #[test]
    fn parenthesized_Bool_clauses_are_unwrapped() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<B: Bool>()
                where
                    _: Verify<{ (!B) }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<B: Bool>()
                where
                    B: Not<Output = True>,
                    B: Not,
                {
                }
            },
        }
    }

    #[test]
    fn unary_op_can_be_applied_to_nested_clause() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Bool, B: Bool>()
                where
                    _: Verify<{ !(A && B) }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Bool, B: Bool>()
                where
                    <A as And<B>>::Output: Not<Output = True>,
                    <A as And<B>>::Output: Not,
                    A: And<B>
                {
                }
            },
        }
    }

    #[test]
    fn binary_op_can_be_applied_to_nested_clause() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Bool, B: Bool, C: Bool>()
                where
                    _: Verify<{ (A && (B || C)) == C }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Bool, B: Bool, C: Bool>()
                where
                    <A as And<<B as Or<C>>::Output>>::Output: Same<C, Output = True>,
                    <A as And<<B as Or<C>>::Output>>::Output: Same<C>,
                    A: And<<B as Or<C>>::Output>,
                    B: Or<C>,
                {
                }
            },
        }
    }

    #[test]
    fn bool_literals_converted_to_False_or_True() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<B: Bool>()
                where
                    _: Verify<{ false == !B }, { true == B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<B: Bool>()
                where
                    False: Same<<B as Not>::Output, Output = True>,
                    False: Same<<B as Not>::Output>,
                    B: Not,
                    True: Same<B, Output = True>,
                    True: Same<B>,
                {
                }
            },
        }
    }

    #[test]
    fn usize_literals_converted_to_Usize_U_T_B1_B0() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<Six: Usize, Zero: Usize>()
                where
                    _: Verify<{ 6 == Six }, { 0 == Zero }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<Six: Usize, Zero: Usize>()
                where
                    U<U<U<T, B1>, B1>, B0>: Same<Six, Output = True>,
                    U<U<U<T, B1>, B1>, B0>: Same<Six>,
                    U<T, B0>: Same<Zero, Output = True>,
                    U<T, B0>: Same<Zero>,
                {
                }
            },
        }
    }

    #[test]
    fn can_verify_usize_addition_clauses() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Usize, B: Usize>()
                where
                    _: Verify<{ (A + B) == 3 }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Usize, B: Usize>()
                where
                    <A as Add<B>>::Output: Same<U<U<T, B1>, B1>, Output = True>,
                    <A as Add<B>>::Output: Same<U<U<T, B1>, B1>>,
                    A: Add<B>,
                {
                }
            },
        }
    }

    #[test]
    fn can_verify_usize_less_than_clauses() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Usize, B: Usize>()
                where
                    _: Verify<{ A < B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Usize, B: Usize>()
                where
                    A: Lt<B, Output = True>,
                    A: Lt<B>,
                {
                }
            },
        }
    }

    #[test]
    fn can_verify_usize_greater_than_clauses() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Usize, B: Usize>()
                where
                    _: Verify<{ A > B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Usize, B: Usize>()
                where
                    A: Gt<B, Output = True>,
                    A: Gt<B>,
                {
                }
            },
        }
    }

    #[test]
    fn can_verify_usize_less_equal_clauses() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Usize, B: Usize>()
                where
                    _: Verify<{ A <= B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Usize, B: Usize>()
                where
                    A: Le<B, Output = True>,
                    A: Le<B>,
                {
                }
            },
        }
    }

    #[test]
    fn can_verify_usize_greater_equal_clauses() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Usize, B: Usize>()
                where
                    _: Verify<{ A >= B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Usize, B: Usize>()
                where
                    A: Ge<B, Output = True>,
                    A: Ge<B>,
                {
                }
            },
        }
    }

    #[test]
    fn can_verify_usize_not_equal_clauses() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Usize, B: Usize>()
                where
                    _: Verify<{ A != B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Usize, B: Usize>()
                where
                    A: Ne<B, Output = True>,
                    A: Ne<B>,
                {
                }
            },
        }
    }
}
