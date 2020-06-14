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

// TODO: Clean up the branching logic here.
fn generate_verifiable_item(item: TokenStream) -> syn::Result<TokenStream> {
    let item: VerifiableItem = syn::parse(item)?;
    Ok(item.0.into_token_stream().into())
}

//  ____                _
// |  _ \ __ _ _ __ ___(_)_ __   __ _
// | |_) / _` | '__/ __| | '_ \ / _` |
// |  __/ (_| | |  \__ \ | | | | (_| |
// |_|   \__,_|_|  |___/_|_| |_|\__, |
//                              |___/
//  FIGLET: Parsing

struct VerifiableItem(syn::Item);

impl syn::parse::Parse for VerifiableItem {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let mut item: syn::Item = input.parse()?;
        item.translate()?;
        Ok(Self(item))
    }
}

impl quote::ToTokens for VerifiableItem {
    fn to_tokens(&self, out: &mut proc_macro2::TokenStream) {
        self.0.to_tokens(out)
    }
}

impl From<Vec<syn::GenericArgument>> for Logic {
    fn from(generics: Vec<syn::GenericArgument>) -> Self {
        Self {
            clauses: generics
                .into_iter()
                .map(|arg| syn::parse_quote! { #arg })
                .collect(),
        }
    }
}

impl From<syn::PathArguments> for Logic {
    fn from(generics: syn::PathArguments) -> Self {
        match generics {
            syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                args,
                ..
            }) => args.into_iter().collect::<Vec<_>>().into(),
            _ => Logic { clauses: vec![] },
        }
    }
}

impl syn::parse::Parse for Logic {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let clauses =
            syn::punctuated::Punctuated::<syn::Expr, syn::Token!(,)>::parse_terminated(input)?
                .into_iter()
                .collect();
        Ok(Self { clauses })
    }
}

//  _____                    _       _
// |_   _| __ __ _ _ __  ___| | __ _| |_ ___  _ __
//   | || '__/ _` | '_ \/ __| |/ _` | __/ _ \| '__|
//   | || | | (_| | | | \__ \ | (_| | || (_) | |
//   |_||_|  \__,_|_| |_|___/_|\__,_|\__\___/|_|
//  FIGLET: Translator

trait Translate {
    fn translate(&mut self) -> syn::Result<()>;
}

impl Translate for syn::Item {
    fn translate(&mut self) -> syn::Result<()> {
        // TODO: support translation of other item types.
        match self {
            // TODO: support translation of rest of function;
            syn::Item::Fn(item) => item.translate(),
            // TODO: support translation of rest of impl;
            syn::Item::Impl(item) => item.translate(),
            item => Err(syn::Error::new(
                item.span(),
                "expected `fn` or `impl`".to_string(),
            )),
        }
    }
}

impl Translate for syn::ItemFn {
    fn translate(&mut self) -> syn::Result<()> {
        let mut _unused_where_clause = syn::parse_quote! { where };
        Translator::new(&mut _unused_where_clause).translate(&mut self.sig)?;
        Ok(())
    }
}

impl Translate for syn::ItemImpl {
    fn translate(&mut self) -> syn::Result<()> {
        let mut _unused_where_clause = syn::parse_quote! { where };
        Translator::new(&mut _unused_where_clause).translate(self)?;

        let mut where_clause = self.generics.make_where_clause();
        let mut translator = Translator::new(&mut where_clause);
        for item in &mut self.items {
            translator.translate(item)?;
        }
        Ok(())
    }
}

struct Translator<'g> {
    where_clause: &'g mut syn::WhereClause,
}

impl<'g> Translator<'g> {
    fn new(where_clause: &'g mut syn::WhereClause) -> Self {
        Self { where_clause }
    }

    fn translate(&mut self, item: &mut impl Generics) -> syn::Result<()> {
        let verify_predicates =
            remove_verify_bound(item.where_clause().unwrap_or(self.where_clause))?;

        let logic = item
            .generics()
            .iter()
            .clone()
            .map(|generics| Into::<Logic>::into((*generics).clone()))
            .collect::<Vec<_>>();

        for (generics, logic) in item.generics().into_iter().zip(logic.iter()) {
            let generics: &mut syn::PathArguments = generics;
            let new_generics: &Vec<syn::Type> = &logic
                .clauses
                .clone()
                .into_iter()
                .map(|expr| {
                    TryInto::<Op>::try_into(expr).and_then(|op| TryInto::<syn::Type>::try_into(op))
                })
                .collect::<syn::Result<Vec<_>>>()?;
            if let syn::PathArguments::AngleBracketed(generic_args) = generics {
                generic_args.args = std::iter::FromIterator::<syn::GenericArgument>::from_iter(
                    new_generics.into_iter().map(|ty| syn::parse_quote!(#ty)),
                );
            }
        }

        let where_clause = item.where_clause().unwrap_or(self.where_clause);
        for logic in logic {
            where_clause.predicates.extend(
                &mut logic
                    .clauses
                    .into_iter()
                    .filter(
                        // Unwrap because we would have errored earlier.
                        |expr| match TryInto::<Op>::try_into(expr.clone()).unwrap() {
                            Op::BinOp { .. } | Op::UnOp { .. } => true,
                            Op::Path(_) => false,
                        },
                    )
                    .map(|expr| {
                        Ok(
                            TryInto::<Vec<syn::PredicateType>>::try_into(TryInto::<Op>::try_into(
                                expr,
                            )?)?
                            .into_iter()
                            .map(|ty| syn::WherePredicate::Type(ty)),
                        )
                    })
                    .collect::<syn::Result<Vec<_>>>()?
                    .into_iter()
                    .flatten(),
            );
        }
        where_clause.predicates.extend(verify_predicates);
        Ok(())
    }
}

trait Generics {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments>;
    fn where_clause<'g>(&'g mut self) -> Option<&'g mut syn::WhereClause> {
        None
    }
}

impl Generics for syn::Signature {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        let mut generics = self.generics.generics();
        generics.append(&mut self.inputs.generics());
        generics.append(&mut self.output.generics());
        generics
    }

    fn where_clause<'g>(&'g mut self) -> Option<&'g mut syn::WhereClause> {
        self.generics.where_clause()
    }
}

impl Generics for syn::Generics {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.make_where_clause();
        let where_clause = self.where_clause.as_mut().unwrap();
        let mut generics = where_clause.generics();
        generics.append(
            &mut self
                .params
                .iter_mut()
                .map(|param| match param {
                    syn::GenericParam::Type(item) => item.bounds.generics(),
                    // TODO: add support for other cases
                    _ => vec![],
                })
                .flatten()
                .collect::<Vec<_>>(),
        );
        generics
    }

    fn where_clause<'g>(&'g mut self) -> Option<&'g mut syn::WhereClause> {
        Some(self.make_where_clause())
    }
}

impl Generics for syn::WhereClause {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.predicates
            .iter_mut()
            .map(|predicate| match predicate {
                syn::WherePredicate::Type(item) => {
                    let mut generics = item.bounded_ty.generics();
                    generics.append(&mut item.bounds.generics());
                    generics
                }
                // TODO: add support for other cases
                _ => vec![],
            })
            .flatten()
            .collect()
    }
}

impl Generics for syn::punctuated::Punctuated<syn::TypeParamBound, syn::Token!(+)> {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.iter_mut()
            .map(|bound| match bound {
                syn::TypeParamBound::Trait(trait_bound) => trait_bound.path.generics(),
                syn::TypeParamBound::Lifetime(_) => vec![],
            })
            .flatten()
            .collect()
    }
}

impl Generics for syn::punctuated::Punctuated<syn::FnArg, syn::Token!(,)> {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.iter_mut()
            .map(|arg| arg.generics())
            .flatten()
            .collect()
    }
}

impl Generics for syn::ItemFn {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.sig.generics()
    }

    fn where_clause<'g>(&'g mut self) -> Option<&'g mut syn::WhereClause> {
        self.sig.where_clause()
    }
}

impl Generics for syn::ItemImpl {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        let mut generics = self.generics.generics();
        generics.append(&mut self.self_ty.generics());
        generics
    }

    fn where_clause<'g>(&'g mut self) -> Option<&'g mut syn::WhereClause> {
        self.generics.where_clause()
    }
}

impl Generics for syn::FnArg {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        match self {
            syn::FnArg::Receiver(_) => vec![],
            syn::FnArg::Typed(pat) => pat.ty.generics(),
        }
    }
}

impl Generics for syn::BareFnArg {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.ty.generics()
    }
}

impl Generics for syn::Path {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.segments
            .iter_mut()
            .map(|segment| segment.generics())
            .flatten()
            .filter(|generics| **generics != syn::PathArguments::None)
            .collect()
    }
}

impl Generics for syn::ReturnType {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        match self {
            syn::ReturnType::Default => vec![],
            syn::ReturnType::Type(_, ty) => ty.generics(),
        }
    }
}

impl Generics for syn::ImplItem {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        // TODO: implement support for other items.
        match self {
            syn::ImplItem::Method(item) => item.generics(),
            syn::ImplItem::Type(item) => item.generics(),
            _ => vec![],
        }
    }

    fn where_clause<'g>(&'g mut self) -> Option<&'g mut syn::WhereClause> {
        match self {
            syn::ImplItem::Method(item) => item.where_clause(),
            syn::ImplItem::Type(item) => item.where_clause(),
            _ => None,
        }
    }
}

impl Generics for syn::ImplItemMethod {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.sig.generics()
    }

    fn where_clause<'g>(&'g mut self) -> Option<&'g mut syn::WhereClause> {
        self.sig.where_clause()
    }
}

impl Generics for syn::ImplItemType {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.ty.generics()
    }
}

impl Generics for syn::Type {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        match self {
            syn::Type::Array(ty) => ty.generics(),
            syn::Type::BareFn(ty) => ty.generics(),
            syn::Type::ImplTrait(ty) => ty.generics(),
            syn::Type::Paren(ty) => ty.generics(),
            syn::Type::Path(ty) => ty.generics(),
            syn::Type::Ptr(ty) => ty.generics(),
            syn::Type::Reference(ty) => ty.generics(),
            syn::Type::Slice(ty) => ty.generics(),
            syn::Type::TraitObject(ty) => ty.generics(),
            syn::Type::Tuple(ty) => ty.generics(),
            _ => vec![],
        }
    }
}

impl Generics for syn::TypeArray {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.elem.generics()
    }
}

impl Generics for syn::TypeBareFn {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.inputs
            .iter_mut()
            .map(|arg| arg.generics())
            .flatten()
            .chain(self.output.generics())
            .collect()
    }
}

impl Generics for syn::TypeImplTrait {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.bounds
            .iter_mut()
            .map(|bound| match bound {
                syn::TypeParamBound::Trait(trait_bound) => trait_bound.path.generics(),
                syn::TypeParamBound::Lifetime(_) => vec![],
            })
            .flatten()
            .collect()
    }
}

impl Generics for syn::TypeParen {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.elem.generics()
    }
}

impl Generics for syn::TypePath {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.path.generics()
    }
}

impl Generics for syn::TypePtr {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.elem.generics()
    }
}

impl Generics for syn::TypeReference {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.elem.generics()
    }
}

impl Generics for syn::TypeSlice {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.elem.generics()
    }
}

impl Generics for syn::TypeTraitObject {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.bounds
            .iter_mut()
            .map(|bound| match bound {
                syn::TypeParamBound::Trait(trait_bound) => trait_bound.path.generics(),
                syn::TypeParamBound::Lifetime(_) => vec![],
            })
            .flatten()
            .collect()
    }
}

impl Generics for syn::TypeTuple {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.elems
            .iter_mut()
            .map(|ty| ty.generics())
            .flatten()
            .collect()
    }
}

impl Generics for syn::PathSegment {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        self.arguments.generics()
    }
}

impl Generics for syn::PathArguments {
    fn generics<'g>(&'g mut self) -> Vec<&'g mut syn::PathArguments> {
        vec![self]
    }
}

//  _____                    _       _   _
// |_   _| __ __ _ _ __  ___| | __ _| |_(_) ___  _ __  ___
//   | || '__/ _` | '_ \/ __| |/ _` | __| |/ _ \| '_ \/ __|
//   | || | | (_| | | | \__ \ | (_| | |_| | (_) | | | \__ \
//   |_||_|  \__,_|_| |_|___/_|\__,_|\__|_|\___/|_| |_|___/
//  FIGLET: Translations

fn remove_verify_bound(
    where_clause: &mut syn::WhereClause,
) -> syn::Result<Vec<syn::WherePredicate>> {
    let (predicates, inferred_bounds): (Vec<_>, Vec<_>) = where_clause
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
    where_clause.predicates = predicates.into_iter().collect();

    if inferred_bounds.len() == 0 {
        return Ok(vec![]);
    }

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

    let logic: Logic = generics.clone().into();

    let predicates = logic
        .clauses
        .into_iter()
        .map(|clause| {
            Ok(vec![syn::WherePredicate::Type(TryInto::try_into(
                TryInto::<Op>::try_into(clause.clone())?,
            )?)]
            .into_iter()
            .chain(
                TryInto::<Vec<_>>::try_into(TryInto::<Op>::try_into(clause)?)?
                    .into_iter()
                    .map(|p| syn::WherePredicate::Type(p)),
            ))
        })
        .collect::<syn::Result<Vec<_>>>()?;
    Ok(predicates.into_iter().flatten().collect())
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

// This conversion does not include the "truthiness" bound, which can be obtained by converting the
// Op into a single PredicateType.
impl TryFrom<Op> for Vec<syn::PredicateType> {
    type Error = syn::Error;
    fn try_from(from: Op) -> syn::Result<Self> {
        let op_name = from.get_op_name();
        Ok(match from {
            Op::BinOp { op, left, right } => {
                let op_name = op_name?;
                let left_ty: syn::Type = (*left.clone()).try_into()?;
                let right_ty: syn::Type = (*right.clone()).try_into()?;
                match op {
                    syn::BinOp::Eq(_)
                    | syn::BinOp::Ne(_)
                    | syn::BinOp::Le(_)
                    | syn::BinOp::Ge(_) => vec![predicate! {{ #left_ty }: { Cmp<#right_ty> }}],
                    syn::BinOp::Lt(_) => vec![
                        predicate! {{ #left_ty }: { Cmp<#right_ty> }},
                        predicate! {{ #left_ty }: { IsLessOrEqual<#right_ty> }},
                    ],
                    syn::BinOp::Gt(_) => vec![
                        predicate! {{ #left_ty }: { Cmp<#right_ty> }},
                        predicate! {{ #left_ty }: { IsGreaterOrEqual<#right_ty> }},
                    ],
                    syn::BinOp::Add(_)
                    | syn::BinOp::Div(_)
                    | syn::BinOp::Mul(_)
                    | syn::BinOp::Rem(_)
                    | syn::BinOp::Shl(_)
                    | syn::BinOp::Shr(_)
                    | syn::BinOp::Sub(_) => vec![
                        predicate! {{ <#left_ty as #op_name<#right_ty>>::Output }: { Unsigned }},
                        predicate! {{ <#left_ty as #op_name<#right_ty>>::Output }: { Cmp }},
                        predicate! {{
                            <#left_ty as #op_name<#right_ty>>::Output }:
                                { IsEqual<<#left_ty as #op_name<#right_ty>>::Output>
                        }},
                    ],
                    _ => vec![],
                }
                .into_iter()
                .chain(vec![predicate! {{ #left_ty }: { #op_name<#right_ty> }}])
                .chain(TryInto::<Self>::try_into(*left)?.into_iter())
                .chain(TryInto::<Self>::try_into(*right)?.into_iter())
                .collect()
            }
            Op::UnOp { left, .. } => {
                let op_name = op_name?;
                let left_ty: syn::Type = (*left.clone()).try_into()?;
                vec![predicate! {{ #left_ty }: { #op_name }}]
                    .into_iter()
                    .chain(TryInto::<Self>::try_into(*left)?.into_iter())
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
            Op::UnOp { left, .. } => {
                let op_name = op_name?;
                let left: syn::Type = (*left).try_into()?;
                Ok(predicate! {{ #left }: { #op_name<Output = B1> }})
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
            Op::UnOp { left, .. } => {
                let op_name = op_name?;
                let left: syn::Type = (*left).try_into()?;
                Ok(syn::parse_quote! { <#left as #op_name>::Output })
            }
            Op::Path(path) => Ok(syn::parse_quote!(#path)),
        }
    }
}

impl TryFrom<syn::Expr> for Op {
    type Error = syn::Error;
    fn try_from(expr: syn::Expr) -> syn::Result<Self> {
        match expr {
            syn::Expr::Binary(syn::ExprBinary {
                op, left, right, ..
            }) => Ok(Op::BinOp {
                op,
                left: Box::new((*left).try_into()?),
                right: Box::new((*right).try_into()?),
            }),
            syn::Expr::Unary(syn::ExprUnary { op, expr, .. }) => Ok(Op::UnOp {
                op,
                left: Box::new((*expr).try_into()?),
            }),
            syn::Expr::Lit(syn::ExprLit { lit, .. }) => Ok(lit.try_into()?),
            syn::Expr::Path(syn::ExprPath { path, .. }) => Ok(Op::Path(path)),
            syn::Expr::Paren(syn::ExprParen { expr, .. }) => Ok((*expr).try_into()?),
            syn::Expr::Block(syn::ExprBlock {
                block: syn::Block { stmts, .. },
                ..
            }) if stmts.len() == 1 => {
                let stmt = stmts.first().unwrap();
                let expr: syn::Expr = syn::parse_quote!(#stmt);
                expr.try_into()
            }
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
struct Logic {
    clauses: Vec<syn::Expr>,
}

#[derive(Clone, Debug)]
enum Op {
    BinOp {
        op: syn::BinOp,
        left: Box<Self>,
        right: Box<Self>,
    },
    UnOp {
        op: syn::UnOp,
        left: Box<Self>,
    },
    Path(syn::Path),
}

impl Op {
    fn get_op_name(&self) -> syn::Result<syn::Path> {
        match self {
            Op::BinOp { op, .. } => {
                macro_rules! op_names {
                ($op:ident { $($from:ident => $to:tt,)* }) => {
                    match $op {
                        $(syn::BinOp::$from(_) => Ok(syn::parse_quote!($to)),)*
                        unsupported_expr => Err(syn::Error::new(
                            unsupported_expr.span(),
                            "unsupported logical expression",
                        )),
                    }
                }
            }
                op_names! {
                    op {
                        Add => Add,
                        BitAnd => BitAnd,
                        BitOr => BitOr,
                        BitXor => BitXor,
                        Div => Div,
                        Eq => IsEqual,
                        Ge => IsGreaterOrEqual,
                        Gt => IsGreater,
                        Le => IsLessOrEqual,
                        Lt => IsLess,
                        Mul => Mul,
                        Rem => Rem,
                        Ne => IsNotEqual,
                        Shl => Shl,
                        Shr => Shr,
                        Sub => Sub,
                    }
                }
            }
            Op::UnOp { op, .. } => match op {
                syn::UnOp::Not(_) => Ok(syn::parse_quote!(Not)),
                unsupported_expr => Err(syn::Error::new(
                    unsupported_expr.span(),
                    "unsupported logical expression",
                )),
            },
            _ => Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                "unimplemented",
            )),
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
    use std::convert::{TryFrom, TryInto};
    use syn::parse_quote;

    macro_rules! op {
        ($($t:tt)*) => {{
            let expr: syn::Expr = parse_quote! { $($t)* };
            Op::try_from(expr).unwrap()
        }};
    }

    macro_rules! ty {
        ($($t:tt)*) => {{
            let ty: syn::Type = parse_quote! { $($t)* };
            ty
        }};
    }

    macro_rules! generics {
        ($(<$($t:tt),*>),*$(,)?) => {
            vec![
                $(&mut syn::PathArguments::AngleBracketed(parse_quote! { <$($t),*> })),*
            ]
        };
        ($(::<$($t:tt),*>),*$(,)?) => {
            vec![
                $(&mut syn::PathArguments::AngleBracketed(parse_quote! { ::<$($t),*> })),*
            ]
        };
    }

    fn assert_into<L: TryInto<R>, R: std::fmt::Debug + Eq>(l: L, r: R)
    where
        <L as TryInto<R>>::Error: std::fmt::Debug,
    {
        assert_eq!(l.try_into().unwrap(), r)
    }

    fn assert_generics<Ty: Generics>(mut ty: Ty, expected: Vec<&mut syn::PathArguments>) {
        assert_eq!(ty.generics(), expected)
    }

    //  _____                       __     ____                      _
    // |_   _|   _ _ __   ___       \ \   / ___| ___ _ __   ___ _ __(_) ___ ___
    //   | || | | | '_ \ / _ \  _____\ \ | |  _ / _ \ '_ \ / _ \ '__| |/ __/ __|
    //   | || |_| | |_) |  __/ |_____/ / | |_| |  __/ | | |  __/ |  | | (__\__ \
    //   |_| \__, | .__/ \___|      /_/   \____|\___|_| |_|\___|_|  |_|\___|___/
    //       |___/|_|
    //  FIGLET: Type -> Generics

    #[test]
    fn ident_path_segment_yields_generic_args() {
        assert_generics::<syn::Type>(parse_quote! { Type<A, B, C> }, generics![<A, B, C>]);
    }

    #[test]
    fn colon_path_segment_yields_generic_args() {
        assert_generics::<syn::Type>(parse_quote! { Type::<A, B, C> }, generics![::<A, B, C>]);
    }

    #[test]
    fn path_yields_generics_from_all_segments() {
        assert_generics::<syn::Type>(
            parse_quote! { a::<A>::b::<B>::c::<C> },
            generics![::<A>, ::<B>, ::<C>],
        );
    }

    #[test]
    fn generics_from_each_tuple_element_are_collected() {
        assert_generics::<syn::Type>(parse_quote! { (L<A, B>, R<C>) }, generics![<A, B>, <C>]);
    }

    #[test]
    fn slice_type_yields_generic_args() {
        assert_generics::<syn::Type>(parse_quote! { [Type<A, B>] }, generics![<A, B>]);
    }

    #[test]
    fn array_type_yields_generic_args() {
        assert_generics::<syn::Type>(parse_quote! { [Type<C>; 0] }, generics![<C>]);
    }

    #[test]
    fn ptr_type_yields_generic_args() {
        assert_generics::<syn::Type>(parse_quote! { *const Type<C> }, generics![<C>]);
    }

    #[test]
    fn ref_type_yields_generic_args() {
        assert_generics::<syn::Type>(parse_quote! { &Type<C> }, generics![<C>]);
    }

    #[test]
    fn paren_type_yields_generic_args() {
        assert_generics::<syn::Type>(parse_quote! { (Type<C>) }, generics![<C>]);
    }

    #[test]
    fn impl_trait_type_yields_generic_args() {
        assert_generics::<syn::Type>(
            parse_quote! { impl Trait0<A> + Trait1<B, C>},
            generics![<A>, <B, C>],
        );
    }

    #[test]
    fn trait_object_type_yields_generic_args() {
        assert_generics::<syn::Type>(
            parse_quote! { dyn Trait0<A, B> + Trait1<C>},
            generics![<A, B>, <C>],
        );
    }

    #[test]
    fn bare_fn_type_yields_generic_args_for_inputs_and_outputs() {
        assert_generics::<syn::Type>(
            parse_quote! { fn(l: Left<A, B>, r: Right<C>) -> Out<D> },
            generics![<A, B>, <C>, <D>],
        );
    }

    #[test]
    fn impl_item_method_yields_generic_args_for_inputs_and_outputs() {
        assert_generics::<syn::ImplItem>(
            parse_quote! { fn f(l: Left<A, B>, r: Right<C>) -> Out<D> { Default::default() } },
            generics![<A, B>, <C>, <D>],
        );
    }

    #[test]
    fn impl_item_type_yields_generic_args_for_inputs_and_outputs() {
        assert_generics::<syn::ImplItem>(parse_quote! { type Type = Impl<D>; }, generics![<D>]);
    }

    #[test]
    fn item_fn_yields_generic_args_for_inputs_outputs_and_generics() {
        assert_generics::<syn::ItemFn>(
            parse_quote! {
                fn f<G0: Trait<A>, G1: Trait<B>>(a0: A0<G0, C>, a1: A1<G1, D>) -> R<E>
                where
                    P0<F>: Trait<G>,
                {
                }
            },
            generics![<F>, <G>, <A>, <B>, <G0, C>, <G1, D>, <E>],
        );
    }

    #[test]
    fn item_impl_yields_generic_args_for_self_and_generics() {
        assert_generics::<syn::ItemImpl>(
            parse_quote! {
                impl<G0: Trait<A>, G1: Trait<B>> Type<G0, G1>
                where
                    P0<F>: Trait<G>,
                {
                }
            },
            generics![<F>, <G>, <A>, <B>, <G0, G1>],
        );
    }

    //   ___              __    _____
    //  / _ \ _ __        \ \  |_   _|   _ _ __   ___
    // | | | | '_ \   _____\ \   | || | | | '_ \ / _ \
    // | |_| | |_) | |_____/ /   | || |_| | |_) |  __/
    //  \___/| .__/       /_/    |_| \__, | .__/ \___|
    //       |_|                     |___/|_|
    //  FIGLET: Op -> Type

    #[test]
    fn binary_op_as_type_yields_output_of_op_trait() {
        assert_into(op! { L + R }, ty! { <L as Add<R>>::Output });
        assert_into(op! { L & R }, ty! { <L as BitAnd<R>>::Output });
        assert_into(op! { L | R }, ty! { <L as BitOr<R>>::Output });
        assert_into(op! { L ^ R }, ty! { <L as BitXor<R>>::Output });
        assert_into(op! { L / R }, ty! { <L as Div<R>>::Output });
        assert_into(op! { L == R }, ty! { <L as IsEqual<R>>::Output });
        assert_into(op! { L >= R }, ty! { <L as IsGreaterOrEqual<R>>::Output });
        assert_into(op! { L > R }, ty! { <L as IsGreater<R>>::Output });
        assert_into(op! { L <= R }, ty! { <L as IsLessOrEqual<R>>::Output });
        assert_into(op! { L < R }, ty! { <L as IsLess<R>>::Output });
        assert_into(op! { L * R }, ty! { <L as Mul<R>>::Output });
        assert_into(op! { L % R }, ty! { <L as Rem<R>>::Output });
        assert_into(op! { L != R }, ty! { <L as IsNotEqual<R>>::Output });
        assert_into(op! { L << R }, ty! { <L as Shl<R>>::Output });
        assert_into(op! { L >> R }, ty! { <L as Shr<R>>::Output });
        assert_into(op! { L - R }, ty! { <L as Sub<R>>::Output });
    }

    #[test]
    fn unary_op_as_type_yields_output_of_op_trait() {
        assert_into(op! { !V }, ty! { <V as Not>::Output });
    }

    #[test]
    fn path_op_as_type_yields_path() {
        assert_into(op! { V }, ty! { V });
    }

    //   ___              __    ____               _ _           _
    //  / _ \ _ __        \ \  |  _ \ _ __ ___  __| (_) ___ __ _| |_ ___
    // | | | | '_ \   _____\ \ | |_) | '__/ _ \/ _` | |/ __/ _` | __/ _ \
    // | |_| | |_) | |_____/ / |  __/| | |  __/ (_| | | (_| (_| | ||  __/
    //  \___/| .__/       /_/  |_|   |_|  \___|\__,_|_|\___\__,_|\__\___|
    //       |_|
    //  FIGLET: Op -> Predicate

    #[test]
    fn path_as_predicate_yeilds_is_equal_to_true_bound() {
        assert_into(op! { V }, predicate! {{ V }: { IsEqual<B1, Output = B1> }});
    }

    #[test]
    fn binary_ops_with_boolean_output_yield_op_on_right_with_true_output_bound() {
        // NB: == is special, and is tested separately
        assert_into(
            op! { A & B },
            predicate! {{ A }: { BitAnd<B, Output = B1> }},
        );
        assert_into(op! { A | B }, predicate! {{ A }: { BitOr<B, Output = B1> }});
        assert_into(
            op! { A ^ B },
            predicate! {{ A }: { BitXor<B, Output = B1> }},
        );
        assert_into(
            op! { A >= B },
            predicate! {{ A }: { IsGreaterOrEqual<B, Output = B1> }},
        );
        assert_into(
            op! { A > B },
            predicate! {{ A }: { IsGreater<B, Output = B1> }},
        );
        assert_into(
            op! { A <= B },
            predicate! {{ A }: { IsLessOrEqual<B, Output = B1> }},
        );
        assert_into(
            op! { A < B },
            predicate! {{ A }: { IsLess<B, Output = B1> }},
        );
        assert_into(
            op! { A != B },
            predicate! {{ A }: { IsNotEqual<B, Output = B1> }},
        );
    }

    #[test]
    fn binary_eq_op_with_path_or_unary_on_both_sides_yields_equality_bound_with_output_of_true() {
        assert_into(
            op! { A == B },
            predicate! {{ A }: { IsEqual<B, Output = B1> }},
        );
        assert_into(
            op! { A == !B },
            predicate! {{ A }: { IsEqual<<B as Not>::Output, Output = B1> }},
        );
        assert_into(
            op! { !A == B },
            predicate! {{ <A as Not>::Output }: { IsEqual<B, Output = B1> }},
        );
        assert_into(
            op! { !A == !B },
            predicate! {{ <A as Not>::Output }: { IsEqual<<B as Not>::Output, Output = B1> }},
        );
    }

    #[test]
    fn binary_eq_op_with_binary_op_on_left_yields_bound_of_left_op_with_output_of_right_op() {
        assert_into(
            op! { A + B == C },
            predicate! {{ A }: { Add<B, Output = C> }},
        );
    }

    #[test]
    fn binary_eq_op_with_binary_op_on_right_yields_bound_of_right_op_with_output_of_left_op() {
        assert_into(
            op! { C == A + B },
            predicate! {{ A }: { Add<B, Output = C> }},
        );
    }

    //   ___              __
    //  / _ \ _ __        \ \
    // | | | | '_ \   _____\ \
    // | |_| | |_) | |_____/ /
    //  \___/| .__/       /_/
    //       |_|
    // __     __         ______               _ _           _      __
    // \ \   / /__  ___ / /  _ \ _ __ ___  __| (_) ___ __ _| |_ ___\ \
    //  \ \ / / _ \/ __/ /| |_) | '__/ _ \/ _` | |/ __/ _` | __/ _ \\ \
    //   \ V /  __/ (__\ \|  __/| | |  __/ (_| | | (_| (_| | ||  __// /
    //    \_/ \___|\___|\_\_|   |_|  \___|\__,_|_|\___\__,_|\__\___/_/
    //  FIGLET: Op -> Vec<Predicate>

    #[test]
    fn path_op_yields_empty() {
        assert_into(op! { V }, vec![]);
    }

    #[test]
    fn unary_op_yields_unary_op_bound_and_bounds_for_operand() {
        assert_into(op! { !V }, vec![predicate! {{ V }: { Not }}]);
        assert_into(
            op! { !(A | (B & C)) },
            vec![
                predicate! {{ <A as BitOr<<B as BitAnd<C>>::Output>>::Output }: { Not }},
                predicate! {{ A }: { BitOr<<B as BitAnd<C>>::Output> }},
                predicate! {{ B }: { BitAnd<C> }},
            ],
        );
    }

    #[test]
    fn binary_eq_ne_le_and_ge_add_extra_cmp_bound() {
        assert_into(
            op! { A == B },
            vec![
                predicate! {{ A }: { Cmp<B> }},
                predicate! {{ A }: { IsEqual<B> }},
            ],
        );
        assert_into(
            op! { A != B },
            vec![
                predicate! {{ A }: { Cmp<B> }},
                predicate! {{ A }: { IsNotEqual<B> }},
            ],
        );
        assert_into(
            op! { A <= B },
            vec![
                predicate! {{ A }: { Cmp<B> }},
                predicate! {{ A }: { IsLessOrEqual<B> }},
            ],
        );
        assert_into(
            op! { A >= B },
            vec![
                predicate! {{ A }: { Cmp<B> }},
                predicate! {{ A }: { IsGreaterOrEqual<B> }},
            ],
        );
    }

    #[test]
    fn binary_lt_and_gt_add_extra_cmp_and_le_or_ge_bound() {
        assert_into(
            op! { A < B },
            vec![
                predicate! {{ A }: { Cmp<B> }},
                predicate! {{ A }: { IsLessOrEqual<B> }},
                predicate! {{ A }: { IsLess<B> }},
            ],
        );
        assert_into(
            op! { A > B },
            vec![
                predicate! {{ A }: { Cmp<B> }},
                predicate! {{ A }: { IsGreaterOrEqual<B> }},
                predicate! {{ A }: { IsGreater<B> }},
            ],
        );
    }

    #[test]
    fn binary_add_div_mul_rem_shl_shr_sub_add_extra_unsigned_cmp_and_eq_bounds() {
        assert_into(
            op! { A + B },
            vec![
                predicate! {{ <A as Add<B>>::Output }: { Unsigned }},
                predicate! {{ <A as Add<B>>::Output }: { Cmp }},
                predicate! {{ <A as Add<B>>::Output }: { IsEqual<<A as Add<B>>::Output> }},
                predicate! {{ A }: { Add<B> }},
            ],
        );
        assert_into(
            op! { A / B },
            vec![
                predicate! {{ <A as Div<B>>::Output }: { Unsigned }},
                predicate! {{ <A as Div<B>>::Output }: { Cmp }},
                predicate! {{ <A as Div<B>>::Output }: { IsEqual<<A as Div<B>>::Output> }},
                predicate! {{ A }: { Div<B> }},
            ],
        );
        assert_into(
            op! { A * B },
            vec![
                predicate! {{ <A as Mul<B>>::Output }: { Unsigned }},
                predicate! {{ <A as Mul<B>>::Output }: { Cmp }},
                predicate! {{ <A as Mul<B>>::Output }: { IsEqual<<A as Mul<B>>::Output> }},
                predicate! {{ A }: { Mul<B> }},
            ],
        );
        assert_into(
            op! { A % B },
            vec![
                predicate! {{ <A as Rem<B>>::Output }: { Unsigned }},
                predicate! {{ <A as Rem<B>>::Output }: { Cmp }},
                predicate! {{ <A as Rem<B>>::Output }: { IsEqual<<A as Rem<B>>::Output> }},
                predicate! {{ A }: { Rem<B> }},
            ],
        );
        assert_into(
            op! { A << B },
            vec![
                predicate! {{ <A as Shl<B>>::Output }: { Unsigned }},
                predicate! {{ <A as Shl<B>>::Output }: { Cmp }},
                predicate! {{ <A as Shl<B>>::Output }: { IsEqual<<A as Shl<B>>::Output> }},
                predicate! {{ A }: { Shl<B> }},
            ],
        );
        assert_into(
            op! { A >> B },
            vec![
                predicate! {{ <A as Shr<B>>::Output }: { Unsigned }},
                predicate! {{ <A as Shr<B>>::Output }: { Cmp }},
                predicate! {{ <A as Shr<B>>::Output }: { IsEqual<<A as Shr<B>>::Output> }},
                predicate! {{ A }: { Shr<B> }},
            ],
        );
        assert_into(
            op! { A - B },
            vec![
                predicate! {{ <A as Sub<B>>::Output }: { Unsigned }},
                predicate! {{ <A as Sub<B>>::Output }: { Cmp }},
                predicate! {{ <A as Sub<B>>::Output }: { IsEqual<<A as Sub<B>>::Output> }},
                predicate! {{ A }: { Sub<B> }},
            ],
        );
    }

    //  ____
    // |  _ \ __ _ _ __ ___  ___
    // | |_) / _` | '__/ __|/ _ \
    // |  __/ (_| | |  \__ \  __/
    // |_|   \__,_|_|  |___/\___|
    //  FIGLET: Parse

    macro_rules! parse_test {
        (
            parse {
                $in:item
            },
            expect {
                $out:item
            },
        ) => {
            let code_in: VerifiableItem = parse_quote! {
                $in
            };
            let code_out = code_in.to_token_stream();
            let expected: syn::Item = parse_quote! {
                $out
            };
            assert_eq!(
                code_out.to_string(),
                expected.into_token_stream().to_string(),
            );
        };
    }

    #[test]
    fn Multiple_Bit_identity_clauses_are_converted_to_multiple_bounds() {
        parse_test! {
            parse {
                fn f<A: Bit, B: Bit>()
                where
                    _: Verify<{ A }, { B }>,
                {
                }
            },
            expect {
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
    fn Bit_equality_clause_is_converted_to_IsEqual_bound_with_added_output_of_true() {
        parse_test! {
            parse {
                fn f<A: Bit, B: Bit>()
                where
                    _: Verify<{ A == B }>,
                {
                }
            },
            expect {
                fn f<A: Bit, B: Bit>()
                where
                    A: IsEqual<B, Output = B1>,
                    A: Cmp<B>,
                    A: IsEqual<B>,
                {
                }
            },
        }
    }

    #[test]
    fn parenthesized_Bit_clauses_are_unwrapped() {
        parse_test! {
            parse {
                fn f<B: Bit>()
                where
                    _: Verify<{ (!B) }>,
                {
                }
            },
            expect {
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
    fn bool_literals_converted_to_B0_or_B1() {
        parse_test! {
            parse {
                fn f<B: Bit>()
                where
                    _: Verify<{ false == !B }, { true == B }>,
                {
                }
            },
            expect {
                fn f<B: Bit>()
                where
                    B0: IsEqual<<B as Not>::Output, Output = B1>,
                    B0: Cmp<<B as Not>::Output>,
                    B0: IsEqual<<B as Not>::Output>,
                    B: Not,
                    B1: IsEqual<B, Output = B1>,
                    B1: Cmp<B>,
                    B1: IsEqual<B>,
                {
                }
            },
        }
    }

    #[test]
    fn usize_literals_converted_to_U() {
        parse_test! {
            parse {
                fn f<Six: Unsigned, Zero: Unsigned>()
                where
                    _: Verify<{ 6 == Six }, { 0 == Zero }>,
                {
                }
            },
            expect {
                fn f<Six: Unsigned, Zero: Unsigned>()
                where
                    U6: IsEqual<Six, Output = B1>,
                    U6: Cmp<Six>,
                    U6: IsEqual<Six>,
                    U0: IsEqual<Zero, Output = B1>,
                    U0: Cmp<Zero>,
                    U0: IsEqual<Zero>,
                {
                }
            },
        }
    }

    #[test]
    fn can_verify_type_construction_in_fn_return_value() {
        parse_test! {
            parse {
                fn f<A: Unsigned>() -> Container<{A + 1}>
                {
                }
            },
            expect {
                fn f<A: Unsigned>() -> Container<<A as Add<U1>>::Output>
                where
                    <A as Add<U1>>::Output: Unsigned,
                    <A as Add<U1>>::Output: Cmp,
                    <A as Add<U1>>::Output: IsEqual<<A as Add<U1>>::Output>,
                    A: Add<U1>,
                {
                }
            },
        }
    }

    #[test]
    fn can_verify_impl_bounds() {
        parse_test! {
            parse {
                impl<A: Unsigned, B: Unsigned> Trait for Struct<A, B>
                where
                    _: Verify<{ A + B == 3 }>,
                {
                }
            },
            expect {
                impl<A: Unsigned, B: Unsigned> Trait for Struct<A, B>
                where
                    A: Add<B, Output = U3>,
                    <A as Add<B>>::Output: Cmp<U3>,
                    <A as Add<B>>::Output: IsEqual<U3>,
                    <A as Add<B>>::Output: Unsigned,
                    <A as Add<B>>::Output: Cmp,
                    <A as Add<B>>::Output: IsEqual<<A as Add<B>>::Output>,
                    A: Add<B>,
                {
                }
            },
        }
    }

    #[test]
    fn can_verify_type_construction_in_associate_type() {
        parse_test! {
            parse {
                impl<A: Unsigned, B: Unsigned> Trait for Struct<A, B>
                {
                    type Type = Output<{A + B}>;
                }
            },
            expect {
                impl<A: Unsigned, B: Unsigned> Trait for Struct<A, B>
                where
                    <A as Add<B>>::Output: Unsigned,
                    <A as Add<B>>::Output: Cmp,
                    <A as Add<B>>::Output: IsEqual<<A as Add<B>>::Output>,
                    A: Add<B>,
                {
                    type Type = Output<<A as Add<B>>::Output>;
                }
            },
        }
    }
}
