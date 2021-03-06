use crate::parse::{
    Chunk, ChunkIter, Chunks, Expr, ExprApplication, ExprBinary, ExprNot, ExprValue, Fancy,
    GenericArg, PathComponent, ValueBit, ValuePath, ValueUnsigned,
};

pub struct Translator<Output: std::io::Write> {
    output: Output,
}

impl<Output: std::io::Write> Translator<Output> {
    pub fn new(output: Output) -> Self {
        Self { output }
    }
    pub fn translate(mut self, input: impl AsRef<[u8]>) {
        let mut chunks = Chunks::new(input.as_ref());
        let mut buffer = Vec::new();
        while let Some((chunk, rest)) = chunks.next() {
            chunks = rest;
            match chunk {
                Chunk::Unparsed(c) => {
                    buffer.push(c);
                }
                Chunk::Parsed(expr) => {
                    self.output.write_all(buffer.as_ref()).unwrap();
                    self.output.write_all(&Into::<Vec<u8>>::into(expr)).unwrap();
                    buffer = Vec::new();
                }
            }
        }
        self.output.write_all(&buffer).unwrap();
    }
}

macro_rules! tokens {
    ($h:expr, $($t:expr),*) => {
        $h.into_iter()$(.chain(Into::<Vec<u8>>::into($t)))*.collect()
    }
}

impl<Item: Into<Vec<u8>>> From<Fancy<Item>> for Vec<u8> {
    fn from(Fancy { code, item }: Fancy<Item>) -> Self {
        code.into_iter().chain(item.into()).collect()
    }
}

impl From<ValueBit> for usize {
    fn from(bit: ValueBit) -> Self {
        match bit {
            ValueBit::B0 => 0,
            ValueBit::B1 => 1,
        }
    }
}

impl From<ValueUnsigned> for usize {
    fn from(unsigned: ValueUnsigned) -> Self {
        match unsigned {
            ValueUnsigned::UInt { msb, lsb } => {
                2 * Into::<Self>::into(*msb) + Into::<Self>::into(lsb)
            }
            ValueUnsigned::UTerm => 0,
        }
    }
}

impl From<ValueUnsigned> for Vec<u8> {
    fn from(unsigned: ValueUnsigned) -> Self {
        let val: usize = unsigned.into();
        val.to_string().into()
    }
}

impl From<ValueBit> for bool {
    fn from(bit: ValueBit) -> Self {
        match bit {
            ValueBit::B0 => false,
            ValueBit::B1 => true,
        }
    }
}

impl From<ValueBit> for Vec<u8> {
    fn from(bit: ValueBit) -> Self {
        let val: bool = bit.into();
        val.to_string().into()
    }
}

impl From<GenericArg> for Vec<u8> {
    fn from(arg: GenericArg) -> Self {
        match arg {
            GenericArg::Expr(expr) => expr.into(),
            GenericArg::AssociatedType { name, ty } => {
                let mut v: Vec<_> = name.into();
                v.extend(b" = ");
                v.extend(Into::<Vec<u8>>::into(ty));
                v
            }
        }
    }
}

impl From<PathComponent> for Vec<u8> {
    fn from(component: PathComponent) -> Self {
        let mut comma = vec![];
        match component {
            PathComponent::Args(args) => tokens![
                "<".bytes(),
                args.into_iter()
                    .map(Into::<Vec<u8>>::into)
                    .map(|arg| {
                        let delimitted = comma.clone().into_iter().chain(arg.into_iter());
                        comma = ", ".into();
                        delimitted
                    })
                    .flatten()
                    .collect::<Vec<_>>(),
                ">"
            ],
            PathComponent::Ident(item) => item.into(),
            PathComponent::Colon => "::".into(),
        }
    }
}

impl From<ValuePath> for Vec<u8> {
    fn from(ValuePath(value): ValuePath) -> Self {
        value
            .into_iter()
            .map(Into::<Vec<u8>>::into)
            .flatten()
            .collect()
    }
}

impl From<ExprValue> for Vec<u8> {
    fn from(expr: ExprValue) -> Self {
        match expr {
            ExprValue::Bit(expr) => expr.into(),
            ExprValue::Unsigned(expr) => expr.into(),
            ExprValue::Path(expr) => expr.into(),
        }
    }
}

impl From<ExprApplication> for Vec<u8> {
    fn from(ExprApplication { lhs, application }: ExprApplication) -> Self {
        match *application {
            Expr::Binary(Fancy {
                code,
                item:
                    ExprBinary {
                        op,
                        expr,
                        result: Some(result),
                    },
            }) => tokens![code, "{ ", *lhs, " ", op, " ", *expr, " == ", *result, " }"],
            Expr::Binary(Fancy {
                code,
                item:
                    ExprBinary {
                        op,
                        expr,
                        result: None,
                    },
            }) => tokens![code, "{ ", *lhs, " ", op, " ", *expr, " }"],
            Expr::Unary(Fancy {
                code,
                item: ExprNot { result: None },
            }) => tokens![code, "!", *lhs],
            Expr::Unary(Fancy {
                code,
                item: ExprNot {
                    result: Some(result),
                },
            }) => tokens![code, "{ ", "!", *lhs, " == ", *result, " }"],
            Expr::Value(expr) => expr.into(),
            Expr::Application(expr) => expr.into(),
        }
    }
}

macro_rules! fancy_expr {
    ($ty:ident { $code:ident, $item:ident }) => {
        Fancy {
            $code,
            $item: ExprApplication {
                lhs: Box::new(Expr::Value(Fancy {
                    $code: "".into(),
                    $item: ExprValue::Path(ValuePath(vec![PathComponent::Ident(Fancy {
                        $code: "".into(),
                        $item: "_".into(),
                    })])),
                })),
                application: Box::new(Expr::$ty(Fancy {
                    $item,
                    $code: "".into(),
                })),
            },
        }
    };
}
impl From<Expr> for Vec<u8> {
    fn from(expr: Expr) -> Self {
        match expr {
            Expr::Application(expr) => expr.into(),
            Expr::Unary(Fancy { code, item }) => fancy_expr! { Unary { code, item } }.into(),
            Expr::Binary(Fancy { code, item }) => fancy_expr! { Binary { code, item } }.into(),
            Expr::Value(Fancy { code, item }) => fancy_expr! { Value { code, item } }.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use textwrap::dedent;

    macro_rules! translate {
        (parse: $input:literal, expect: $expected:literal $(,)?) => {
            let mut output = Vec::new();
            let translator = Translator::new(&mut output);

            let input = dedent($input);
            translator.translate(input.as_bytes());

            let expected = dedent($expected);
            let output = std::str::from_utf8(&output).unwrap();
            assert_eq!(
                output, expected,
                "\nactual: {}\nexpected: {}",
                output, expected
            );
        };
    }

    #[test]
    fn translator_replaces_unsigned() {
        translate! {
            parse: "\
                xxx typenum::uint::UTerm
                xxx typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>, typenum::bit::B1>
                xxx",
            expect: "\
                xxx 0
                xxx 3
                xxx",
        }
    }

    #[test]
    fn unsigned_to_bytes_yields_decimal_encoding() {
        translate! {
            parse: "\
                typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>, typenum::bit::B0>
            ",
            expect: "\
                2
            ",
        }
    }

    #[test]
    fn bit_to_bytes_yields_bool_encoding() {
        translate! {
            parse: "typenum::bit::B0",
            expect: "false",
        }
        translate! {
            parse: "typenum::bit::B1",
            expect: "true",
        }
    }

    #[test]
    fn unary_expr_to_bytes_yields_operator_and_translated_expr() {
        translate! {
            parse: "<typenum::bit::B0 as std::ops::Not>::Output",
            expect: "!false",
        }
    }

    #[test]
    fn unary_expr_with_result_to_bytes_yields_operator_and_translated_expr_with_result() {
        translate! {
            parse: "<typenum::bit::B0 as std::ops::Not<Output = typenum::bit::B1>>",
            expect: "{ !false == true }",
        }
    }

    #[test]
    fn binary_expr_to_bytes_yields_operator_and_translated_expr() {
        translate! {
            parse: "std::ops::Add<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>>",
            expect: "{ _ + 1 }",
        }
    }

    #[test]
    fn binary_expr_with_result_to_bytes_yields_operator_and_translated_expr_with_result() {
        translate! {
            parse: "std::ops::Add<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>, Output = typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>>",
            expect: "{ _ + 1 == 1 }",
        }
    }

    #[test]
    fn expressions_in_path_types_are_translated() {
        translate! {
            parse: "std::ops::Add<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>, Output = mod::Type<typenum::uint::UTerm, AssociatedType = typenum::bit::B1>>",
            expect: "{ _ + 1 == mod::Type<0, AssociatedType = true> }",
        }
    }

    #[test]
    fn app() {
        translate! {
            parse: "<Self as std::ops::Add<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>>>::Output",
            expect: "{ Self + 1 }",
        }
    }

    #[test]
    fn translator_replaces_missing_implementation() {
        translate! {
        parse: "\
                error[E0277]: cannot subtract `typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>` from `Size`
                  --> verified/src/vec.rs:40:14
                   |
                40 |         self.into()
                   |              ^^^^ no implementation for `Size - typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>`
                   |
                   = help: the trait `std::ops::Sub<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>>` is not implemented for `Size`
                help: consider further restricting this bound with `+ std::ops::Sub<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>>`
                  --> verified/src/vec.rs:28:12
                   |
                28 | impl<Size: Unsigned, Element> Vec<Size, Element> {
                   |            ^^^^^^^^
                   = note: required because of the requirements on the impl of `std::convert::From<vec::Vec<Size, Element>>` for `(vec::Vec<<Size as std::ops::Sub<typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>, typenum::bit::B0>>>::Output, Element>, Element)`
                   = note: required because of the requirements on the impl of `std::convert::Into<(vec::Vec<<Size as std::ops::Sub<typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>, typenum::bit::B0>>>::Output, Element>, Element)>` for `vec::Vec<Size, Element>`",

        expect: "\
                error[E0277]: cannot subtract `1` from `Size`
                  --> verified/src/vec.rs:40:14
                   |
                40 |         self.into()
                   |              ^^^^ no implementation for `Size - 1`
                   |
                   = help: the trait `{ _ - 1 }` is not implemented for `Size`
                help: consider further restricting this bound with `+ { _ - 1 }`
                  --> verified/src/vec.rs:28:12
                   |
                28 | impl<Size: Unsigned, Element> Vec<Size, Element> {
                   |            ^^^^^^^^
                   = note: required because of the requirements on the impl of `std::convert::From<vec::Vec<Size, Element>>` for `(vec::Vec<{ Size - 2 }, Element>, Element)`
                   = note: required because of the requirements on the impl of `std::convert::Into<(vec::Vec<{ Size - 2 }, Element>, Element)>` for `vec::Vec<Size, Element>`",
        }
    }

    #[test]
    fn translator_replaces_unsigned_type_expectation() {
        translate! {
            parse: "\
                error[E0308]: mismatched types
                  --> verified/src/vec.rs:59:18
                   |
                59 |         (Vec(s - U2::new(), v), e)
                   |                  ^^^^^^^^^ expected struct `typenum::uint::UTerm`, found struct `typenum::uint::UInt`
                   |
                   = note: expected struct `typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>`
                              found struct `typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>, typenum::bit::B0>`",


            expect: "\
                error[E0308]: mismatched types
                  --> verified/src/vec.rs:59:18
                   |
                59 |         (Vec(s - U2::new(), v), e)
                   |                  ^^^^^^^^^ expected struct `0`, found struct `typenum::uint::UInt`
                   |
                   = note: expected struct `1`
                              found struct `2`",
        }
    }

    #[test]
    fn translator_parses_and_retains_control_chars() {
        translate! {
            parse: "\
                \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[32m   Compiling\u{1b}[0m verified v0.2.3 (/home/bob/verified/verified)
                \u{1b}[0m\u{1b}[1m\u{1b}[38;5;9merror[E0277]\u{1b}[0m\u{1b}[0m\u{1b}[1m: cannot subtract `typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>` from `Size`\u{1b}[0m
                \u{1b}[0m  \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m--> \u{1b}[0m\u{1b}[0mverified/src/vec.rs:40:14\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m|\u{1b}[0m
                \u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m40\u{1b}[0m\u{1b}[0m \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m| \u{1b}[0m\u{1b}[0m        self.into()\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m| \u{1b}[0m\u{1b}[0m             \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;9m^^^^\u{1b}[0m\u{1b}[0m \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;9mno implementation for `Size - typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>`\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m|\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m= \u{1b}[0m\u{1b}[0m\u{1b}[1mhelp\u{1b}[0m\u{1b}[0m: the trait `std::ops::Sub<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>>` is not implemented for `Size`\u{1b}[0m
                \u{1b}[0m\u{1b}[1m\u{1b}[38;5;14mhelp\u{1b}[0m\u{1b}[0m: consider further restricting this bound with `+ std::ops::Sub<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>>`\u{1b}[0m
                \u{1b}[0m  \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m--> \u{1b}[0m\u{1b}[0mverified/src/vec.rs:28:12\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m|\u{1b}[0m
                \u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m28\u{1b}[0m\u{1b}[0m \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m| \u{1b}[0m\u{1b}[0mimpl<Size: Unsigned, Element> Vec<Size, Element> {\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m| \u{1b}[0m\u{1b}[0m           \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;14m^^^^^^^^\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m= \u{1b}[0m\u{1b}[0m\u{1b}[1mnote\u{1b}[0m\u{1b}[0m: required because of the requirements on the impl of `std::convert::From<vec::Vec<Size, Element>>` for `(vec::Vec<<Size as std::ops::Sub<typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>, typenum::bit::B0>>>::Output, Element>, Element)`\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m= \u{1b}[0m\u{1b}[0m\u{1b}[1mnote\u{1b}[0m\u{1b}[0m: required because of the requirements on the impl of `std::convert::Into<(vec::Vec<<Size as std::ops::Sub<typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>, typenum::bit::B0>>>::Output, Element>, Element)>` for `vec::Vec<Size, Element>`",
            expect: "\
                \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[32m   Compiling\u{1b}[0m verified v0.2.3 (/home/bob/verified/verified)
                \u{1b}[0m\u{1b}[1m\u{1b}[38;5;9merror[E0277]\u{1b}[0m\u{1b}[0m\u{1b}[1m: cannot subtract `1` from `Size`\u{1b}[0m
                \u{1b}[0m  \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m--> \u{1b}[0m\u{1b}[0mverified/src/vec.rs:40:14\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m|\u{1b}[0m
                \u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m40\u{1b}[0m\u{1b}[0m \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m| \u{1b}[0m\u{1b}[0m        self.into()\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m| \u{1b}[0m\u{1b}[0m             \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;9m^^^^\u{1b}[0m\u{1b}[0m \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;9mno implementation for `Size - 1`\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m|\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m= \u{1b}[0m\u{1b}[0m\u{1b}[1mhelp\u{1b}[0m\u{1b}[0m: the trait `{ _ - 1 }` is not implemented for `Size`\u{1b}[0m
                \u{1b}[0m\u{1b}[1m\u{1b}[38;5;14mhelp\u{1b}[0m\u{1b}[0m: consider further restricting this bound with `+ { _ - 1 }`\u{1b}[0m
                \u{1b}[0m  \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m--> \u{1b}[0m\u{1b}[0mverified/src/vec.rs:28:12\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m|\u{1b}[0m
                \u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m28\u{1b}[0m\u{1b}[0m \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m| \u{1b}[0m\u{1b}[0mimpl<Size: Unsigned, Element> Vec<Size, Element> {\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m| \u{1b}[0m\u{1b}[0m           \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;14m^^^^^^^^\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m= \u{1b}[0m\u{1b}[0m\u{1b}[1mnote\u{1b}[0m\u{1b}[0m: required because of the requirements on the impl of `std::convert::From<vec::Vec<Size, Element>>` for `(vec::Vec<{ Size - 2 }, Element>, Element)`\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m= \u{1b}[0m\u{1b}[0m\u{1b}[1mnote\u{1b}[0m\u{1b}[0m: required because of the requirements on the impl of `std::convert::Into<(vec::Vec<{ Size - 2 }, Element>, Element)>` for `vec::Vec<Size, Element>`",
        }
        translate! {
            parse: "\
                \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[32m   Compiling\u{1b}[0m verified v0.2.3 (/home/bob/verified/verified)
                \u{1b}[0m\u{1b}[1m\u{1b}[38;5;9merror[E0308]\u{1b}[0m\u{1b}[0m\u{1b}[1m: mismatched types\u{1b}[0m
                \u{1b}[0m  \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m--> \u{1b}[0m\u{1b}[0mverified/src/vec.rs:59:18\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m|\u{1b}[0m
                \u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m59\u{1b}[0m\u{1b}[0m \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m| \u{1b}[0m\u{1b}[0m        (Vec(s - U2::new(), v), e)\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m| \u{1b}[0m\u{1b}[0m                 \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;9m^^^^^^^^^\u{1b}[0m\u{1b}[0m \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;9mexpected struct `typenum::uint::UTerm`, found struct `typenum::uint::UInt`\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m|\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m= \u{1b}[0m\u{1b}[0m\u{1b}[1mnote\u{1b}[0m\u{1b}[0m: expected struct `typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::\u{1b}[0m\u{1b}[0m\u{1b}[1mB1\u{1b}[0m\u{1b}[0m>`\u{1b}[0m
                \u{1b}[0m              found struct `typenum::uint::UInt<\u{1b}[0m\u{1b}[0m\u{1b}[1mtypenum::uint::UInt<\u{1b}[0m\u{1b}[0mtypenum::uint::UTerm, \u{1b}[0m\u{1b}[0m\u{1b}[1mtypenum::bit::B1>\u{1b}[0m\u{1b}[0m, typenum::bit::\u{1b}[0m\u{1b}[0m\u{1b}[1mB0\u{1b}[0m\u{1b}[0m>`",
            expect: "\
                \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[32m   Compiling\u{1b}[0m verified v0.2.3 (/home/bob/verified/verified)
                \u{1b}[0m\u{1b}[1m\u{1b}[38;5;9merror[E0308]\u{1b}[0m\u{1b}[0m\u{1b}[1m: mismatched types\u{1b}[0m
                \u{1b}[0m  \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m--> \u{1b}[0m\u{1b}[0mverified/src/vec.rs:59:18\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m|\u{1b}[0m
                \u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m59\u{1b}[0m\u{1b}[0m \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m| \u{1b}[0m\u{1b}[0m        (Vec(s - U2::new(), v), e)\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m| \u{1b}[0m\u{1b}[0m                 \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;9m^^^^^^^^^\u{1b}[0m\u{1b}[0m \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;9mexpected struct `0`, found struct `typenum::uint::UInt`\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m|\u{1b}[0m
                \u{1b}[0m   \u{1b}[0m\u{1b}[0m\u{1b}[1m\u{1b}[38;5;12m= \u{1b}[0m\u{1b}[0m\u{1b}[1mnote\u{1b}[0m\u{1b}[0m: expected struct `1`\u{1b}[0m
                \u{1b}[0m              found struct `2`",
        }
    }
}
