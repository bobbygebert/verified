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
        let expr: syn::Expr = input.parse()?;
        Ok(Self(match expr {
            syn::Expr::Block(syn::ExprBlock {
                block: syn::Block { stmts, .. },
                ..
            }) if stmts.len() == 1 => {
                let stmt = stmts.first().unwrap();
                syn::parse_quote!(#stmt)
            }
            expr => expr,
        }))
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
    ($op:ident, $left:ident, $right:ident) => {
        syn::Expr::Binary(syn::ExprBinary {
            $left,
            $op,
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
        let op_name = from.get_op_name();
        Ok(match from {
            Op::BinOp { left, right, .. } => {
                let op = op_name?;
                let left_ty: syn::Type = (*left.clone()).try_into()?;
                let right_ty: syn::Type = (*right.clone()).try_into()?;
                vec![predicate! {{ #left_ty }: { #op<#right_ty> }}]
                    .into_iter()
                    .chain(TryInto::<Self>::try_into(ImplPredicate(*left))?.into_iter())
                    .chain(TryInto::<Self>::try_into(ImplPredicate(*right))?.into_iter())
                    .collect()
            }
            Op::UnOp { op, left } => {
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
        let op_name = from.get_op_name();
        match from {
            Op::BinOp {
                op: syn::BinOp::Eq(_),
                left,
                right,
            } => {
                let op = op_name?;
                let left_op = left.get_op_name();
                let right_op = right.get_op_name();
                match (*left, *right) {
                    (
                        Op::BinOp {
                            left: left_left,
                            right: left_right,
                            ..
                        },
                        right,
                    ) => {
                        let left_op = left_op?;
                        let left_left: syn::Type = (*left_left).try_into()?;
                        let left_right: syn::Type = (*left_right).try_into()?;
                        let right: syn::Type = (right).try_into()?;
                        Ok(predicate! {{ #left_left }: { #left_op<#left_right, Output = #right> }})
                    }
                    (
                        left,
                        Op::BinOp {
                            left: right_left,
                            right: right_right,
                            ..
                        },
                    ) => {
                        let right_op = right_op?;
                        let right_left: syn::Type = (*right_left).try_into()?;
                        let right_right: syn::Type = (*right_right).try_into()?;
                        let left: syn::Type = (left).try_into()?;
                        Ok(
                            predicate! {{ #right_left }: { #right_op<#right_right, Output = #left> }},
                        )
                    }
                    (left, right) => {
                        let left: syn::Type = (left).try_into()?;
                        let right: syn::Type = (right).try_into()?;
                        Ok(predicate! {{ #left }: { #op<#right, Output = B1> }})
                    }
                }
            }
            Op::BinOp { left, right, .. } => {
                let op = op_name?;
                let left: syn::Type = (*left).try_into()?;
                let right: syn::Type = (*right).try_into()?;
                Ok(predicate! {{ #left }: { #op<#right, Output = B1> }})
            }
            Op::UnOp { op, left } => {
                let left: syn::Type = (*left).try_into()?;
                Ok(predicate! {{ #left }: { #op<Output = B1> }})
            }
            Op::Path(path) => Ok(predicate! {{ #path }: { IsEqual<B1, Output = B1> }}),
        }
    }
}

impl TryFrom<Op> for syn::Type {
    type Error = syn::Error;
    fn try_from(from: Op) -> syn::Result<Self> {
        let op_name = from.get_op_name();
        match from {
            Op::BinOp { left, right, .. } => {
                let op = op_name?;
                let left: syn::Type = (*left).try_into()?;
                let right: syn::Type = (*right).try_into()?;
                Ok(syn::parse_quote! { <#left as #op<#right>>::Output })
            }
            Op::UnOp { op, left } => {
                let left: syn::Type = (*left).try_into()?;
                Ok(syn::parse_quote! { <#left as #op>::Output })
            }
            Op::Path(path) => Ok(syn::parse_quote!(#path)),
        }
    }
}

macro_rules! op {
    ($op:ident, $arg:ident) => {
        Op::UnOp {
            op: syn::parse_quote!($op),
            left: Box::new(Clause(*$arg).try_into()?),
        }
    };
    ($op:ident, $left:ident, $right:ident) => {
        Op::BinOp {
            $op,
            left: Box::new(Clause(*$left).try_into()?),
            right: Box::new(Clause(*$right).try_into()?),
        }
    };
}

impl TryFrom<Clause> for Op {
    type Error = syn::Error;
    fn try_from(clause: Clause) -> syn::Result<Self> {
        match clause.0 {
            expression!(op, left, right) => Ok(op!(op, left, right)),
            expression!(Not, expr) => Ok(op!(Not, expr)),
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
                syn::parse_quote!(B1)
            } else {
                syn::parse_quote!(B0)
            })),
            syn::Lit::Int(value) => Ok(Op::Path({
                let ty = syn::Ident::new(
                    &format!("U{}", value.base10_parse::<usize>()?),
                    value.span(),
                );
                syn::parse_quote!(#ty)
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
    BinOp {
        op: syn::BinOp,
        left: Box<Self>,
        right: Box<Self>,
    },
    UnOp {
        op: syn::Path,
        left: Box<Self>,
    },
    Path(syn::Path),
}

impl Op {
    fn get_op_name(&self) -> syn::Result<syn::Path> {
        if let Op::BinOp { op, .. } = self {
            match op {
                syn::BinOp::Add(_) => Ok(syn::parse_quote!(Add)),
                syn::BinOp::BitAnd(_) => Ok(syn::parse_quote!(BitAnd)),
                syn::BinOp::BitOr(_) => Ok(syn::parse_quote!(BitOr)),
                syn::BinOp::BitXor(_) => Ok(syn::parse_quote!(BitXor)),
                syn::BinOp::Eq(_) => Ok(syn::parse_quote!(IsEqual)),
                syn::BinOp::Ge(_) => Ok(syn::parse_quote!(IsGreaterOrEqual)),
                syn::BinOp::Gt(_) => Ok(syn::parse_quote!(IsGreater)),
                syn::BinOp::Le(_) => Ok(syn::parse_quote!(IsLessOrEqual)),
                syn::BinOp::Lt(_) => Ok(syn::parse_quote!(IsLess)),
                syn::BinOp::Ne(_) => Ok(syn::parse_quote!(IsNotEqual)),
                unsupported_expr => Err(syn::Error::new(
                    unsupported_expr.span(),
                    "unsupported logical expression",
                )),
            }
        } else {
            Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                "unimplemented",
            ))
        }
    }
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
    fn Bit_identity_clause_is_converted_bound_of_IsEqual_B1() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<B: Bit>()
                where
                    _: Verify<{ B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<B: Bit>()
                where
                    B: IsEqual<B1, Output = B1>
                {
                }
            },
        }
    }

    #[test]
    fn Multiple_Bit_identity_clauses_are_converted_to_multiple_bounds() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Bit, B: Bit>()
                where
                    _: Verify<{ A }, { B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Bit, B: Bit>()
                where
                    A: IsEqual<B1, Output = B1>,
                    B: IsEqual<B1, Output = B1>
                {
                }
            },
        }
    }

    #[test]
    fn Bit_equality_clause_is_converted_to_IsEqual_bound() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Bit, B: Bit>()
                where
                    _: Verify<{ A == B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Bit, B: Bit>()
                where
                    A: IsEqual<B, Output = B1>,
                    A: IsEqual<B>,
                {
                }
            },
        }
    }

    #[test]
    fn Bit_and_clause_is_converted_to_And_bound_on_BinOp() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Bit, B: Bit>()
                where
                    _: Verify<{ A & B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Bit, B: Bit>()
                where
                    A: BitAnd<B, Output = B1>,
                    A: BitAnd<B>,
                {
                }
            },
        }
    }

    #[test]
    fn Bit_or_clause_is_converted_to_Or_bound_on_BinOp() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Bit, B: Bit>()
                where
                    _: Verify<{ A | B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Bit, B: Bit>()
                where
                    A: BitOr<B, Output = B1>,
                    A: BitOr<B>,
                {
                }
            },
        }
    }

    #[test]
    fn Bit_xor_clause_is_converted_to_Xor_bound_on_BinOp() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<A: Bit, B: Bit>()
                where
                    _: Verify<{ A ^ B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Bit, B: Bit>()
                where
                    A: BitXor<B, Output = B1>,
                    A: BitXor<B>,
                {
                }
            },
        }
    }

    #[test]
    fn Bit_not_clause_is_converted_to_IsEqual_bound_on_BitXor_Output() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<B: Bit>()
                where
                    _: Verify<{ !B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<B: Bit>()
                where
                    B: Not<Output = B1>,
                    B: Not,
                {
                }
            },
        }
    }

    #[test]
    fn parenthesized_Bit_clauses_are_unwrapped() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<B: Bit>()
                where
                    _: Verify<{ (!B) }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<B: Bit>()
                where
                    B: Not<Output = B1>,
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
                fn f<A: Bit, B: Bit>()
                where
                    _: Verify<{ !(A & B) }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Bit, B: Bit>()
                where
                    <A as BitAnd<B>>::Output: Not<Output = B1>,
                    <A as BitAnd<B>>::Output: Not,
                    A: BitAnd<B>
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
                fn f<A: Bit, B: Bit, C: Bit>()
                where
                    _: Verify<{ (A & (B | C)) == C }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Bit, B: Bit, C: Bit>()
                where
                    A: BitAnd<<B as BitOr<C>>::Output, Output = C>,
                    <A as BitAnd<<B as BitOr<C>>::Output>>::Output: IsEqual<C>,
                    A: BitAnd<<B as BitOr<C>>::Output>,
                    B: BitOr<C>,
                {
                }
            },
        }
    }

    #[test]
    fn bool_literals_converted_to_B0_or_B1() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<B: Bit>()
                where
                    _: Verify<{ false == !B }, { true == B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<B: Bit>()
                where
                    B0: IsEqual<<B as Not>::Output, Output = B1>,
                    B0: IsEqual<<B as Not>::Output>,
                    B: Not,
                    B1: IsEqual<B, Output = B1>,
                    B1: IsEqual<B>,
                {
                }
            },
        }
    }

    #[test]
    fn usize_literals_converted_to_U() {
        parse_test! {
            parse: VerifiedFn
            {
                fn f<Six: Unsigned, Zero: Unsigned>()
                where
                    _: Verify<{ 6 == Six }, { 0 == Zero }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<Six: Unsigned, Zero: Unsigned>()
                where
                    U6: IsEqual<Six, Output = B1>,
                    U6: IsEqual<Six>,
                    U0: IsEqual<Zero, Output = B1>,
                    U0: IsEqual<Zero>,
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
                fn f<A: Unsigned, B: Unsigned>()
                where
                    _: Verify<{ (A + B) == 3 }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Unsigned, B: Unsigned>()
                where
                    A: Add<B, Output = U3>,
                    <A as Add<B>>::Output: IsEqual<U3>,
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
                fn f<A: Unsigned, B: Unsigned>()
                where
                    _: Verify<{ A < B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Unsigned, B: Unsigned>()
                where
                    A: IsLess<B, Output = B1>,
                    A: IsLess<B>,
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
                fn f<A: Unsigned, B: Unsigned>()
                where
                    _: Verify<{ A > B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Unsigned, B: Unsigned>()
                where
                    A: IsGreater<B, Output = B1>,
                    A: IsGreater<B>,
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
                fn f<A: Unsigned, B: Unsigned>()
                where
                    _: Verify<{ A <= B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Unsigned, B: Unsigned>()
                where
                    A: IsLessOrEqual<B, Output = B1>,
                    A: IsLessOrEqual<B>,
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
                fn f<A: Unsigned, B: Unsigned>()
                where
                    _: Verify<{ A >= B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Unsigned, B: Unsigned>()
                where
                    A: IsGreaterOrEqual<B, Output = B1>,
                    A: IsGreaterOrEqual<B>,
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
                fn f<A: Unsigned, B: Unsigned>()
                where
                    _: Verify<{ A != B }>,
                {
                }
            },
            expect: syn::ItemFn
            {
                fn f<A: Unsigned, B: Unsigned>()
                where
                    A: IsNotEqual<B, Output = B1>,
                    A: IsNotEqual<B>,
                {
                }
            },
        }
    }
}
