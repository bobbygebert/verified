use crate::{op, token, ty};
use combine::error::ParseError;
use combine::parser::byte::{alpha_num, byte, bytes, spaces};
use combine::stream::position;
use combine::stream::{Stream, StreamOnce};
use combine::{any, attempt, between, choice, many1, optional, parser, sep_by, EasyParser, Parser};

#[derive(Debug, Eq, PartialEq)]
pub enum ValueBit {
    B0,
    B1,
}

impl From<ValueBit> for usize {
    fn from(bit: ValueBit) -> Self {
        match bit {
            ValueBit::B0 => 0,
            ValueBit::B1 => 1,
        }
    }
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
    AssociatedType(PathComponent, Expr),
}

#[derive(Debug, Eq, PartialEq)]
pub enum PathComponent {
    Ident(Vec<u8>),
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

#[derive(Debug, Eq, PartialEq)]
pub enum ExprUnary {
    Not { result: Option<Box<Expr>> },
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
    Value(ExprValue),
    Unary(ExprUnary),
    Binary(ExprBinary),
    Application(ExprApplication),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Chunk {
    Parsed(Expr),
    Unparsed(u8),
}

fn parse_bit<'b, Input>() -> impl Parser<Input, Output = ValueBit> + 'b
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
        attempt(bytes(ty!(B0)).map(|_| ValueBit::B0)),
        bytes(ty!(B1)).map(|_| ValueBit::B1),
    ))
}

parser! {
    fn bit['b, Input]()(Input) -> ValueBit
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        parse_bit()
    }
}

fn parse_unsigned<'b, Input>() -> impl Parser<Input, Output = ValueUnsigned> + 'b
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
    let uterm = || bytes(ty!(UTerm)).map(|_| ValueUnsigned::UTerm);
    let uint = || bytes(ty!(UInt));
    let generics = || {
        between(
            bytes(token!(<)),
            bytes(token!(>)),
            (
                unsigned(),
                (spaces(), bytes(token!(,)), spaces()),
                attempt(bit()),
            ),
        )
    };
    choice((
        attempt(uterm()),
        (uint(), generics()).map(|(_ty, (msb, _comma, lsb))| ValueUnsigned::UInt {
            msb: Box::new(msb),
            lsb,
        }),
    ))
}

parser! {
    fn unsigned['b, Input]()(Input) -> ValueUnsigned
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        parse_unsigned()
    }
}

fn parse_type_path<'b, Input>() -> impl Parser<Input, Output = ValuePath> + 'b
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
    let segment = || many1(choice((alpha_num(), byte(token!(_)[0])))).map(PathComponent::Ident);
    let colon2 = || bytes(token!(::)).map(|_| PathComponent::Colon);
    let args = || {
        between(
            bytes(token!(<)),
            bytes(token!(>)),
            sep_by(
                choice((
                    attempt(
                        (segment(), spaces(), bytes(token!(=)), spaces(), expr())
                            .map(|(l, _, _, _, r)| GenericArg::AssociatedType(l, r)),
                    ),
                    expr().map(GenericArg::Expr),
                )),
                (spaces(), bytes(token!(,)), spaces()),
            ),
        )
        .map(PathComponent::Args)
    };
    many1(choice((attempt(segment()), attempt(colon2()), args()))).map(ValuePath)
}

parser! {
    fn type_path['b, Input]()(Input) -> ValuePath
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        parse_type_path()
    }
}

fn parse_value<'b, Input>() -> impl Parser<Input, Output = ExprValue> + 'b
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
        attempt(unsigned().map(ExprValue::Unsigned)),
        attempt(bit().map(ExprValue::Bit)),
        type_path().map(ExprValue::Path),
    ))
}

parser! {
    fn value['b, Input]()(Input) -> ExprValue
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        parse_value()
    }
}

fn parse_unary<'b, Input>() -> impl Parser<Input, Output = ExprUnary> + 'b
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
    let not = || bytes(op!(!));
    let generics = || {
        optional(between(
            bytes(token!(<)),
            bytes(token!(>)),
            (
                bytes(token!(Output)),
                spaces(),
                bytes(token!(=)),
                spaces(),
                expr(),
            ),
        ))
    };
    (not(), generics()).map(|(_op, generics)| ExprUnary::Not {
        result: generics.map(|(_, _, _, _, result)| Box::new(result)),
    })
}

parser! {
    fn unary['b, Input]()(Input) -> ExprUnary
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        parse_unary()
    }
}

fn parse_binary<'b, Input>() -> impl Parser<Input, Output = ExprBinary> + 'b
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
    let op = || {
        choice((
            attempt(bytes(op!(+)).map(|_| "+".into())),
            attempt(bytes(op!(&)).map(|_| "&".into())),
            attempt(bytes(op!(|)).map(|_| "|".into())),
            attempt(bytes(op!(^)).map(|_| "^".into())),
            attempt(bytes(op!(/)).map(|_| "/".into())),
            attempt(bytes(op!(==)).map(|_| "==".into())),
            attempt(bytes(op!(>=)).map(|_| ">=".into())),
            attempt(bytes(op!(>)).map(|_| ">".into())),
            attempt(bytes(op!(<=)).map(|_| "<=".into())),
            attempt(bytes(op!(<)).map(|_| "<".into())),
            attempt(bytes(op!(*)).map(|_| "*".into())),
            attempt(bytes(op!(%)).map(|_| "%".into())),
            attempt(bytes(op!(!=)).map(|_| "!=".into())),
            attempt(bytes(op!(<<)).map(|_| "<<".into())),
            attempt(bytes(op!(>>)).map(|_| ">>".into())),
            attempt(bytes(op!(-)).map(|_| "-".into())),
        ))
    };

    let generics = || {
        between(
            bytes(token!(<)),
            bytes(token!(>)),
            (
                expr(),
                optional((
                    bytes(token!(,)),
                    spaces(),
                    bytes(token!(Output)),
                    spaces(),
                    bytes(token!(=)),
                    spaces(),
                    expr(),
                )),
            ),
        )
    };
    (op(), generics()).map(|(op, (expr, result))| ExprBinary {
        op,
        expr: Box::new(expr),
        result: result.map(|(_, _, _, _, _, _, expr)| Box::new(expr)),
    })
}

parser! {
    fn binary['b, Input]()(Input) -> ExprBinary
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        parse_binary()
    }
}

fn parse_application<'b, Input>() -> impl Parser<Input, Output = ExprApplication> + 'b
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
    (
        between(
            bytes(token!(<)),
            bytes(token!(>)),
            (expr(), bytes(b" as "), expr()),
        ),
        optional((bytes(token!(::)), bytes(token!(Output)))),
    )
        .map(|((lhs, _, application), _)| ExprApplication {
            lhs: Box::new(lhs),
            application: Box::new(application),
        })
}

parser! {
    fn application['b, Input]()(Input) -> ExprApplication
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        parse_application()
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
        attempt(application().map(Expr::Application)),
        attempt(binary().map(Expr::Binary)),
        attempt(unary().map(Expr::Unary)),
        attempt(value().map(Expr::Value)),
    ))
}

parser! {
    fn expr['b, Input]()(Input) -> Expr
    where [Input: Stream<Token = u8, Range = &'b [u8]> + 'b]
    {
        parse_expr()
    }
}

pub fn chunk<'b, Input>() -> impl Parser<Input, Output = Chunk> + 'b
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
        attempt(expr().map(Chunk::Parsed)),
        any().map(Chunk::Unparsed),
    ))
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
    use ExprUnary::*;
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
            Parsed(Value(Path(ValuePath(vec![PathComponent::Ident(
                "ty".into()
            )]))))
        );
        assert_eq!(c, None);
    }

    #[test]
    fn chunks_iter_yields_b0_when_next_in_stream() {
        assert_eq!(
            Chunks::new(ty!(B0)).next().unwrap().0,
            Parsed(Value(Bit(B0)))
        );
    }

    #[test]
    fn chunks_iter_yields_b1_when_next_in_stream() {
        assert_eq!(
            Chunks::new(ty!(B1)).next().unwrap().0,
            Parsed(Value(Bit(B1)))
        );
    }

    #[test]
    fn chunks_iter_yields_uterm_when_uterm_is_next_in_stream() {
        assert_eq!(
            Chunks::new(ty!(UTerm)).next().unwrap().0,
            Parsed(Value(Unsigned(UTerm)))
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
            Parsed(Value(Unsigned(UInt {
                msb: Box::new(UTerm),
                lsb: B0,
            })))
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
            Parsed(Value(Unsigned(UInt {
                msb: Box::new(UTerm),
                lsb: B1,
            })))
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
            Parsed(Value(Unsigned(UInt {
                msb: Box::new(UInt {
                    msb: Box::new(UTerm),
                    lsb: B1,
                }),
                lsb: B0,
            })))
        );
    }

    #[test]
    fn chunks_iter_yields_unary_expr_when_next_in_stream() {
        assert_eq!(
            Chunks::new(op!(!)).next().unwrap().0,
            Parsed(Unary(Not { result: None }))
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
            Parsed(Unary(Not {
                result: Some(Box::new(Value(Bit(B1)))),
            }))
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
            Parsed(Binary(ExprBinary {
                op: "+".into(),
                expr: Box::new(Value(Unsigned(UInt {
                    msb: Box::new(UTerm),
                    lsb: B1,
                }))),
                result: None,
            }))
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
                    msb: Box::new(UTerm),
                    lsb: B1,
                }))),
                result: Some(Box::new(Value(Unsigned(UInt {
                    msb: Box::new(UTerm),
                    lsb: B1,
                })))),
            }))
        );
    }

    #[test]
    fn chunks_iter_translates_many_binary_ops() {
        fn parse_binary_expr_and_get_op(input: &[u8]) -> String {
            match Chunks::new(input).next().unwrap().0 {
                Parsed(Binary(expr)) => std::str::from_utf8(&expr.op).unwrap().to_string(),
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
                    msb: Box::new(UTerm),
                    lsb: B1,
                }))),
                result: Some(Box::new(Value(Path(ValuePath(vec![
                    PathComponent::Ident("mod".into()),
                    PathComponent::Colon,
                    PathComponent::Ident("Type".into()),
                    PathComponent::Args(vec![
                        GenericArg::Expr(Value(Unsigned(UTerm))),
                        GenericArg::AssociatedType(
                            PathComponent::Ident("AssociatedType".into()),
                            Value(Bit(B1))
                        ),
                    ])
                ]))))),
            }))
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
                    lhs: Box::new(Value(Path(ValuePath(vec![PathComponent::Ident("Self".into())])))),
                    application: Box::new(Binary(ExprBinary {
                        op: "+".into(),
                        expr: Box::new(Value(Unsigned(UInt {
                            msb: Box::new(UTerm),
                            lsb: B1,
                        }))),
                        result: None,
                    }))
                })
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
                    lhs: Box::new(Value(Path(ValuePath(vec![PathComponent::Ident("Self".into())])))),
                    application: Box::new(Binary(ExprBinary {
                        op: "+".into(),
                        expr: Box::new(Value(Unsigned(UInt {
                            msb: Box::new(UTerm),
                            lsb: B1,
                        }))),
                        result: None,
                    }))
                })
            )
        );
    }
}
