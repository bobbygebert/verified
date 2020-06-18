use crate::{op, token, ty};
use combine::error::ParseError;
use combine::parser::byte::{alpha_num, byte, bytes, spaces};
use combine::parser::repeat::iterate;
use combine::stream::position;
use combine::stream::{Stream, StreamOnce};
use combine::{
    any, attempt, between, choice, many, many1, optional, parser, satisfy, sep_by, seq_parser_expr,
    seq_parser_impl, seq_parser_pattern, struct_parser, EasyParser, Parser,
};

#[derive(Debug, Eq, PartialEq)]
pub struct Fancy<Item> {
    pub code: Vec<u8>,
    pub item: Item,
}

impl<Item> From<Item> for Fancy<Item> {
    fn from(item: Item) -> Self {
        Self {
            code: "".into(),
            item,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ValueBit {
    B0,
    B1,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ValueUnsigned {
    UInt {
        msb: Box<ValueUnsigned>,
        lsb: ValueBit,
    },
    UTerm,
}

#[derive(Debug, Eq, PartialEq)]
pub enum GenericArg {
    Expr(Expr),
    AssociatedType { name: PathComponent, ty: Expr },
}

#[derive(Debug, Eq, PartialEq)]
pub enum PathComponent {
    Ident(Fancy<Vec<u8>>),
    Colon,
    Args(Vec<GenericArg>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct ValuePath(pub Vec<PathComponent>);

#[derive(Debug, Eq, PartialEq)]
pub enum ExprValue {
    Bit(ValueBit),
    Unsigned(ValueUnsigned),
    Path(ValuePath),
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct ExprNot {
    pub result: Option<Box<Expr>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ExprBinary {
    pub op: Vec<u8>,
    pub expr: Box<Expr>,
    pub result: Option<Box<Expr>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ExprApplication {
    pub lhs: Box<Expr>,
    pub application: Box<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Value(Fancy<ExprValue>),
    Unary(Fancy<ExprNot>),
    Binary(Fancy<ExprBinary>),
    Application(Fancy<ExprApplication>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Chunk {
    Parsed(Expr),
    Unparsed(u8),
}

fn control<'b, Input>() -> impl Parser<Input, Output = Vec<u8>> + 'b
where
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
    Input: Stream + 'b,
    Input: Stream<Token = u8, Range = &'b [u8]>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let mut prev = '.';
    many(satisfy(move |t: u8| {
        let t = t as char;
        let res = (t.is_ascii_control() && !t.is_ascii_whitespace())
            || t == '['
            || (t == 'm' && (prev == '[' || prev.is_ascii_digit()))
            || t == ';'
            || t.is_ascii_digit();
        prev = t;
        res
    }))
}

fn fancy<'b, Input, Item>(
    uncolored: impl Parser<Input, Output = Item> + 'b,
) -> impl Parser<Input, Output = Fancy<Item>> + 'b
where
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
    Input: Stream + 'b,
    Input: Stream<Token = u8, Range = &'b [u8]>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (control(), uncolored).map(|(code, item)| Fancy { code, item })
}

parser! {
    fn fancy_ty['b, Input](ty: &'static [u8])(Input) -> Vec<Fancy<&'b [u8]>>
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        iterate(
            ty
                .split(|b| *b == ':' as u8)
                .filter(|segment| segment != b"")
                .map(|segment: &[u8]| vec![&b"::"[..], segment])
                .flatten()
                .skip(1),
            |p, _| fancy(bytes(p)))
    }
}

parser! {
    fn bit['b, Input]()(Input) -> ValueBit
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        choice((
            attempt(fancy_ty(ty!(B0)).map(|_| ValueBit::B0)),
            fancy_ty(ty!(B1)).map(|_| ValueBit::B1),
        ))
    }
}

parser! {
    fn unsigned['b, Input]()(Input) -> ValueUnsigned
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        use ValueUnsigned::UInt;
        choice((
            attempt(fancy_ty(
                ty!(UTerm)).map(|_| ValueUnsigned::UTerm),
            ),
            fancy_ty(
                ty!(UInt),
            ).with(between(fancy(bytes(token!(<))), fancy(bytes(token!(>))),
                struct_parser! {
                    UInt {
                        msb: unsigned().map(Box::new),
                        _: (fancy(spaces()), fancy(bytes(token!(,))), fancy(spaces())),
                        lsb: attempt(bit()),
                    }
                }
            )),
        ))
    }
}

parser! {
    fn type_path['b, Input]()(Input) -> ValuePath
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        use GenericArg::AssociatedType;
        let segment =
            || fancy(many1(choice((alpha_num(), byte(token!(_)[0]))))).map(PathComponent::Ident);
        let colon2 = || bytes(token!(::)).map(|_| PathComponent::Colon);
        let args = || {
            between(
                fancy(bytes(token!(<))),
                fancy(bytes(token!(>))),
                sep_by(
                    attempt(
                        struct_parser! {
                            AssociatedType {
                                name: segment(),
                                _: (spaces(), bytes(token!(=)), spaces()),
                                ty: expr(),
                            }
                        }
                    ).or(expr().map(GenericArg::Expr)),
                    (spaces(), bytes(token!(,)), spaces()),
                )
            )
            .map(PathComponent::Args)
        };
        many1(choice((attempt(segment()), attempt(colon2()), args()))).map(ValuePath)
    }
}

parser! {
    fn value['b, Input]()(Input) -> ExprValue
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        choice((
            attempt(unsigned().map(ExprValue::Unsigned)),
            attempt(bit().map(ExprValue::Bit)),
            type_path().map(ExprValue::Path),
        ))
    }
}

parser! {
    fn unary['b, Input]()(Input) -> ExprNot
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        struct_parser! {
            ExprNot {
                _: bytes(op!(!)),
                result: optional(between(
                    fancy(bytes(token!(<))),
                    fancy(bytes(token!(>))),
                    (bytes(token!(Output)), spaces(), bytes(token!(=)), spaces()).with(
                        expr().map(Box::new)
                    ),
                ))
            }
        }
    }
}

macro_rules! op_parser {
    ($($op:tt),*) => {
        choice(($(attempt(bytes(op!($op))).map(|_| stringify!($op).into()),)*))
    }
}

parser! {
    fn binary['b, Input]()(Input) -> ExprBinary
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        struct Args {
            expr: Box<Expr>,
            result: Option<Box<Expr>>,
        }
        (
            op_parser![+, &, |, ^, /, ==, >=, >, <=, <, *, %, !=, <<, >>, -],
            between(
                fancy(bytes(token!(<))),
                fancy(bytes(token!(>))),
                struct_parser! {
                    Args {
                        expr: expr().map(Box::new),
                        result: optional((
                            fancy(bytes(token!(,))),
                            fancy(spaces()),
                            fancy(bytes(token!(Output))),
                            fancy(spaces()),
                            fancy(bytes(token!(=))),
                            fancy(spaces())
                        ).with(expr().map(Box::new))),
                    }
                }
            ),
        ).map(|(op, Args { expr, result })| ExprBinary { op, expr, result })
    }
}

parser! {
    fn application['b, Input]()(Input) -> ExprApplication
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        between(
            fancy(bytes(token!(<))),
            fancy(bytes(token!(>))),
            struct_parser! {
                ExprApplication {
                    lhs: expr().map(Box::new),
                    _: bytes(b" as "),
                    application: expr().map(Box::new),
                }
            }
        ).skip(
            optional((fancy(bytes(token!(::))), fancy(bytes(token!(Output)))))
        )
    }
}

fn parse_expr<'b, Input>() -> impl Parser<Input, Output = Expr> + 'b
where
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
    Input: Stream + 'b,
    Input: Stream<Token = u8, Range = &'b [u8]>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice((
        attempt(fancy(application()).map(Expr::Application)),
        attempt(fancy(binary()).map(Expr::Binary)),
        attempt(fancy(unary()).map(Expr::Unary)),
        attempt(fancy(value()).map(Expr::Value)),
    ))
}

parser! {
    fn expr['b, Input]()(Input) -> Expr
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        parse_expr()
    }
}

parser! {
    fn chunk['b, Input]()(Input) -> Chunk
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        choice((
            attempt(expr().map(Chunk::Parsed)),
            any().map(Chunk::Unparsed),
        ))
    }
}

pub struct Chunks;

impl Chunks {
    pub fn new<'b>(input: &'b [u8]) -> position::Stream<&'b [u8], position::IndexPositioner> {
        position::Stream::new(input.as_ref())
    }
}

pub trait ChunkIter: Sized {
    fn next(self) -> Option<(Chunk, Self)>;
}

impl<'b> ChunkIter for position::Stream<&'b [u8], position::IndexPositioner> {
    fn next(self) -> Option<(Chunk, Self)> {
        chunk().easy_parse(self).ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Chunk::*;
    use Expr::*;
    use ExprValue::*;
    use ValueBit::*;
    use ValueUnsigned::*;

    macro_rules! tokens {
        ($($t:tt)*) => {{
            vec![$($t)*].into_iter().flatten().map(|b| *b).collect::<Vec<_>>().as_ref()
        }}
    }

    #[test]
    fn chunks_iter_yields_next_char_when_unsigned_cannot_be_parsed() {
        assert_eq!(
            Chunks::new(token!(>)).next().unwrap().0,
            Unparsed('>' as u8)
        );
    }

    #[test]
    fn chunks_iter_yields_multiple_items() {
        let iter = Chunks::new(b">ty");
        let (a, iter) = iter.next().unwrap();
        let (b, iter) = iter.next().unwrap();
        let c = iter.next();
        assert_eq!(a, Unparsed('>' as u8));
        assert_eq!(
            b,
            Parsed(Value(
                Path(ValuePath(vec![PathComponent::Ident(
                    Into::<Vec<u8>>::into("ty").into()
                )]))
                .into()
            ))
        );
        assert_eq!(c, None);
    }

    #[test]
    fn chunks_iter_yields_b0_when_next_in_stream() {
        assert_eq!(
            Chunks::new(ty!(B0)).next().unwrap().0,
            Parsed(Value(Bit(B0).into()))
        );
    }

    #[test]
    fn chunks_iter_yields_b1_when_next_in_stream() {
        assert_eq!(
            Chunks::new(ty!(B1)).next().unwrap().0,
            Parsed(Value(Bit(B1).into()))
        );
    }

    #[test]
    fn chunks_iter_yields_uterm_when_uterm_is_next_in_stream() {
        assert_eq!(
            Chunks::new(ty!(UTerm)).next().unwrap().0,
            Parsed(Value(Unsigned(UTerm).into()))
        );
    }

    #[test]
    fn chunks_iter_yields_u0_when_u0_is_next_in_stream() {
        assert_eq!(
            Chunks::new(tokens![
                ty!(UInt),
                token!(<),
                ty!(UTerm),
                token!(,),
                ty!(B0),
                token!(>)
            ])
            .next()
            .unwrap()
            .0,
            Parsed(Value(
                Unsigned(UInt {
                    msb: Box::new(UTerm.into()),
                    lsb: B0.into(),
                })
                .into()
            ))
        );
    }

    #[test]
    fn chunks_iter_yields_u1_when_u1_is_next_in_stream() {
        assert_eq!(
            Chunks::new(tokens![
                ty!(UInt),
                token!(<),
                ty!(UTerm),
                token!(,),
                ty!(B1),
                token!(>)
            ])
            .next()
            .unwrap()
            .0,
            Parsed(Value(
                Unsigned(UInt {
                    msb: Box::new(UTerm.into()),
                    lsb: B1.into(),
                })
                .into()
            ))
        );
    }

    #[test]
    fn chunks_iter_yields_u2_when_u2_is_next_in_stream() {
        assert_eq!(
            Chunks::new(tokens![
                ty!(UInt),
                token!(<),
                ty!(UInt),
                token!(<),
                ty!(UTerm),
                token!(,),
                ty!(B1),
                token!(>),
                token!(,),
                ty!(B0),
                token!(>)
            ])
            .next()
            .unwrap()
            .0,
            Parsed(Value(
                Unsigned(UInt {
                    msb: Box::new(
                        UInt {
                            msb: Box::new(UTerm.into()),
                            lsb: B1.into(),
                        }
                        .into()
                    ),
                    lsb: B0.into(),
                })
                .into()
            ))
        );
    }

    #[test]
    fn chunks_iter_yields_unary_expr_when_next_in_stream() {
        assert_eq!(
            Chunks::new(op!(!)).next().unwrap().0,
            Parsed(Unary(ExprNot { result: None }.into()))
        );
    }

    #[test]
    fn chunks_iter_yields_unary_expr_with_equals_when_next_in_stream() {
        assert_eq!(
            Chunks::new(tokens![
                op!(!),
                token!(<),
                token!(Output),
                token!(=),
                ty!(B1),
                token!(>)
            ])
            .next()
            .unwrap()
            .0,
            Parsed(Unary(
                ExprNot {
                    result: Some(Box::new(Value(Bit(B1).into()))),
                }
                .into()
            ))
        );
    }

    #[test]
    fn chunks_iter_yields_binary_expr_when_next_in_stream() {
        assert_eq!(
            Chunks::new(tokens![
                op!(+),
                token!(<),
                ty!(UInt),
                token!(<),
                ty!(UTerm),
                token!(,),
                ty!(B1),
                token!(>),
                token!(>),
            ])
            .next()
            .unwrap()
            .0,
            Parsed(Binary(
                ExprBinary {
                    op: "+".into(),
                    expr: Box::new(Value(
                        Unsigned(UInt {
                            msb: Box::new(UTerm.into()),
                            lsb: B1.into(),
                        })
                        .into()
                    )),
                    result: None,
                }
                .into()
            ))
        );
    }

    #[test]
    fn chunks_iter_yields_binary_expr_with_equals_when_next_in_stream() {
        #[rustfmt::skip]
        assert_eq!(
            Chunks::new(tokens![
                op!(+),
                token!(<),
                    ty!(UInt), token!(<), ty!(UTerm), token!(,), ty!(B1), token!(>),
                    token!(,),
                    token!(Output), token!(=),
                        ty!(UInt), token!(<), ty!(UTerm), token!(,), ty!(B1), token!(>),
                token!(>),
            ])
            .next()
            .unwrap()
            .0,
            Parsed(Binary(ExprBinary {
                op: "+".into(),
                expr: Box::new(Value(Unsigned(UInt {
                    msb: Box::new(UTerm.into()),
                    lsb: B1.into(),
                }).into())),
                result: Some(Box::new(Value(Unsigned(UInt {
                    msb: Box::new(UTerm.into()),
                    lsb: B1.into(),
                }).into()))),
            }.into()))
        );
    }

    #[test]
    fn chunks_iter_translates_many_binary_ops() {
        fn parse_binary_expr_and_get_op(input: &[u8]) -> String {
            match Chunks::new(input).next().unwrap().0 {
                Parsed(Binary(expr)) => std::str::from_utf8(&expr.item.op).unwrap().to_string(),
                _ => panic!(),
            }
        }
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(+), token!(<), ty!(UTerm), token!(>)]),
            "+",
        );
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(&), token!(<), ty!(UTerm), token!(>)]),
            "&",
        );
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(|), token!(<), ty!(UTerm), token!(>)]),
            "|",
        );
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(^), token!(<), ty!(UTerm), token!(>)]),
            "^",
        );
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(/), token!(<), ty!(UTerm), token!(>)]),
            "/",
        );
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(==), token!(<), ty!(UTerm), token!(>)]),
            "==",
        );
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(>=), token!(<), ty!(UTerm), token!(>)]),
            ">=",
        );
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(>), token!(<), ty!(UTerm), token!(>)]),
            ">",
        );
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(<=), token!(<), ty!(UTerm), token!(>)]),
            "<=",
        );
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(<), token!(<), ty!(UTerm), token!(>)]),
            "<",
        );
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(*), token!(<), ty!(UTerm), token!(>)]),
            "*",
        );
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(%), token!(<), ty!(UTerm), token!(>)]),
            "%",
        );
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(!=), token!(<), ty!(UTerm), token!(>)]),
            "!=",
        );
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(<<), token!(<), ty!(UTerm), token!(>)]),
            "<<",
        );
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(>>), token!(<), ty!(UTerm), token!(>)]),
            ">>",
        );
        assert_eq!(
            parse_binary_expr_and_get_op(tokens![op!(-), token!(<), ty!(UTerm), token!(>)]),
            "-",
        );
    }

    #[test]
    fn chunks_iter_yields_path_when_necessary() {
        #[rustfmt::skip]
        assert_eq!(
            Chunks::new(tokens![
                op!(+),
                token!(<),
                    ty!(UInt),
                    token!(<),
                        ty!(UTerm), token!(,), ty!(B1),
                    token!(>),
                    token!(,),
                    token!(Output), token!(=),
                        b"mod", token!(::), b"Type",
                        token!(<),
                            ty!(UTerm),
                            token!(,),
                            b"AssociatedType", token!(=), ty!(B1),
                        token!(>),
                token!(>),
            ])
            .next()
            .unwrap()
            .0,
            Parsed(Binary(ExprBinary {
                op: "+".into(),
                expr: Box::new(Value(Unsigned(UInt {
                    msb: Box::new(UTerm.into()),
                    lsb: B1.into(),
                }).into())),
                result: Some(Box::new(Value(Path(ValuePath(vec![
                    PathComponent::Ident(Into::<Vec<u8>>::into("mod").into()),
                    PathComponent::Colon,
                    PathComponent::Ident(Into::<Vec<u8>>::into("Type").into()),
                    PathComponent::Args(vec![
                        GenericArg::Expr(Value(Unsigned(UTerm).into())),
                        GenericArg::AssociatedType{
                            name: PathComponent::Ident(Into::<Vec<u8>>::into("AssociatedType").into()),
                            ty: Value(Bit(B1).into())
                        },
                    ])
                ])).into()))),
            }.into()))
        );
    }

    #[test]
    fn chunks_iter_yields_application_expr_when_next_in_stream() {
        #[rustfmt::skip]
        assert_eq!(
            Chunks::new(tokens![
                token!(<),
                    b"Self as ",
                    op!(+),
                    token!(<),
                        ty!(UInt), token!(<), ty!(UTerm), token!(,), ty!(B1), token!(>),
                    token!(>),
                token!(>),
            ])
            .next()
            .unwrap()
            .0,
            Parsed(
                Application(ExprApplication {
                    lhs: Box::new(Value(Path(ValuePath(vec![
                        PathComponent::Ident(Into::<Vec<u8>>::into("Self").into())
                    ])).into())),
                    application: Box::new(Binary(ExprBinary {
                        op: "+".into(),
                        expr: Box::new(Value(Unsigned(UInt {
                            msb: Box::new(UTerm.into()),
                            lsb: B1.into(),
                        }).into())),
                        result: None,
                    }.into()))
                }.into())
            )
        );
    }

    #[test]
    fn chunks_iter_yields_application_expr_when_next_in_stream_with_output() {
        #[rustfmt::skip]
        assert_eq!(
            Chunks::new(tokens![
                token!(<),
                    b"Self as ",
                    op!(+),
                    token!(<),
                        ty!(UInt), token!(<), ty!(UTerm), token!(,), ty!(B1), token!(>),
                    token!(>),
                token!(>),
                token!(::),
                token!(Output),
            ])
            .next()
            .unwrap()
            .0,
            Parsed(
                Application(ExprApplication {
                    lhs: Box::new(Value(Path(ValuePath(vec![PathComponent::Ident(
                        Into::<Vec<u8>>::into("Self").into())
                    ])).into())),
                    application: Box::new(Binary(ExprBinary {
                        op: "+".into(),
                        expr: Box::new(Value(Unsigned(UInt {
                            msb: Box::new(UTerm.into()),
                            lsb: B1.into(),
                        }).into())),
                        result: None,
                    }.into()))
                }.into())
            )
        );
    }
}
