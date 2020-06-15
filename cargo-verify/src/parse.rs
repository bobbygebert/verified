use combine::error::ParseError;
use combine::parser::byte::{byte, bytes, spaces};
use combine::stream::position;
use combine::stream::{Stream, StreamOnce};
use combine::{any, attempt, between, choice, parser, EasyParser, Parser};

#[derive(Debug, Eq, PartialEq)]
pub enum Bit<'b> {
    B0(&'b [u8]),
    B1(&'b [u8]),
}

impl<'b> From<Bit<'b>> for usize {
    fn from(bit: Bit<'b>) -> Self {
        match bit {
            Bit::B0(_) => 0,
            Bit::B1(_) => 1,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Unsigned<'b> {
    UInt {
        ty: &'b [u8],
        msb: Box<Unsigned<'b>>,
        lsb: Bit<'b>,
    },
    UTerm(&'b [u8]),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Chunk<'b> {
    Unsigned(Unsigned<'b>),
    Char(u8),
}

impl<'b> From<Unsigned<'b>> for usize {
    fn from(unsigned: Unsigned<'b>) -> Self {
        match unsigned {
            Unsigned::UInt { msb, lsb, .. } => {
                2 * Into::<Self>::into(*msb) + Into::<Self>::into(lsb)
            }
            Unsigned::UTerm(_) => 0,
        }
    }
}

impl<'b> From<Unsigned<'b>> for Vec<u8> {
    fn from(unsigned: Unsigned<'b>) -> Self {
        let val: usize = unsigned.into();
        format!("U{}", val).into()
    }
}

fn parse_unsigned<'b, Input>() -> impl Parser<Input, Output = Unsigned<'b>>
where
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
    Input: Stream,
    Input: Stream<Token = u8, Range = &'b [u8]>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let b0 = || bytes(b"typenum::bit::B0").map(Bit::B0);
    let b1 = || bytes(b"typenum::bit::B1").map(Bit::B1);
    let uterm = || bytes(b"typenum::uint::UTerm").map(Unsigned::UTerm);
    let uint = || bytes(b"typenum::uint::UInt");
    let generics = || {
        between(
            byte('<' as u8),
            byte('>' as u8),
            (
                unsigned(),
                (spaces(), byte(',' as u8), spaces()),
                attempt(b0()).or(b1()),
            ),
        )
    };
    choice((
        attempt(uterm()),
        (uint(), generics()).map(|(ty, (msb, _comma, lsb))| Unsigned::UInt {
            ty,
            msb: Box::new(msb),
            lsb,
        }),
    ))
}

parser! {
    fn unsigned['b, Input]()(Input) -> Unsigned<'b>
    where [Input: Stream<Token = u8, Range = &'b [u8]>]
    {
        parse_unsigned()
    }
}

pub fn chunk<'b, Input>() -> impl Parser<Input, Output = Chunk<'b>>
where
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
    Input: Stream,
    Input: Stream<Token = u8, Range = &'b [u8]>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice((
        attempt(unsigned().map(Chunk::Unsigned)),
        any().map(Chunk::Char),
    ))
}

pub struct Chunks;

impl Chunks {
    pub fn new<'b>(input: &'b [u8]) -> position::Stream<&'b [u8], position::IndexPositioner> {
        position::Stream::new(input.as_ref())
    }
}

pub trait ChunkIter<'b>: Sized {
    fn next(self) -> Option<(Chunk<'b>, Self)>;
}

impl<'b> ChunkIter<'b> for position::Stream<&'b [u8], position::IndexPositioner> {
    fn next(self) -> Option<(Chunk<'b>, Self)> {
        chunk().easy_parse(self).ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chunks_iter_yields_next_char_when_unsigned_cannot_be_parsed() {
        assert_eq!(
            Chunks::new(b"typenum::bad").next().unwrap().0,
            Chunk::Char('t' as u8)
        );
    }

    #[test]
    fn chunks_iter_yields_multiple_items() {
        let iter = Chunks::new(b"ty");
        let (a, iter) = iter.next().unwrap();
        let (b, iter) = iter.next().unwrap();
        let c = iter.next();
        assert_eq!(a, Chunk::Char('t' as u8));
        assert_eq!(b, Chunk::Char('y' as u8));
        assert_eq!(c, None);
    }

    #[test]
    fn chunks_iter_yields_uterm_when_uterm_is_next_in_stream() {
        assert_eq!(
            Chunks::new(b"typenum::uint::UTerm").next().unwrap().0,
            Chunk::Unsigned(Unsigned::UTerm(b"typenum::uint::UTerm"))
        );
    }

    #[test]
    fn chunks_iter_yields_u0_when_u0_is_next_in_stream() {
        assert_eq!(
            Chunks::new(b"typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B0>")
                .next()
                .unwrap()
                .0,
            Chunk::Unsigned(Unsigned::UInt {
                ty: b"typenum::uint::UInt",
                msb: Box::new(Unsigned::UTerm(b"typenum::uint::UTerm")),
                lsb: Bit::B0(b"typenum::bit::B0"),
            })
        );
    }

    #[test]
    fn chunks_iter_yields_u1_when_u1_is_next_in_stream() {
        assert_eq!(
            Chunks::new(b"typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>")
                .next()
                .unwrap()
                .0,
            Chunk::Unsigned(Unsigned::UInt {
                ty: b"typenum::uint::UInt",
                msb: Box::new(Unsigned::UTerm(b"typenum::uint::UTerm")),
                lsb: Bit::B1(b"typenum::bit::B1"),
            })
        );
    }

    #[test]
    fn chunks_iter_yields_u2_when_u2_is_next_in_stream() {
        assert_eq!(
            Chunks::new(b"typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>, typenum::bit::B0>")
                .next()
                .unwrap()
                .0,
            Chunk::Unsigned(Unsigned::UInt {
                ty: b"typenum::uint::UInt",
                msb: Box::new(Unsigned::UInt {
                    ty: b"typenum::uint::UInt",
                    msb: Box::new(Unsigned::UTerm(b"typenum::uint::UTerm")),
                    lsb: Bit::B1(b"typenum::bit::B1"),
                }),
                lsb: Bit::B0(b"typenum::bit::B0"),
            })
        );
    }
}
