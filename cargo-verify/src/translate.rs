use crate::parse::{
    Chunk, ChunkIter, Chunks, Expr, ExprApplication, ExprBinary, ExprUnary, ExprValue, GenericArg,
    PathComponent, ValueBit, ValuePath, ValueUnsigned,
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
            GenericArg::AssociatedType(name, expr) => {
                let mut v: Vec<_> = name.into();
                v.extend(b" = ");
                v.extend(Into::<Vec<u8>>::into(expr));
                v
            }
        }
    }
}

impl From<PathComponent> for Vec<u8> {
    fn from(component: PathComponent) -> Self {
        let mut comma = vec![];
        match component {
            PathComponent::Args(args) => vec!['<' as u8]
                .into_iter()
                .chain(
                    args.into_iter()
                        .map(Into::<Vec<u8>>::into)
                        .map(|arg| {
                            let delimitted = comma.clone().into_iter().chain(arg.into_iter());
                            comma = ", ".into();
                            delimitted
                        })
                        .flatten(),
                )
                .chain(vec!['>' as u8])
                .collect::<Vec<_>>(),
            PathComponent::Ident(ident) => ident,
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

impl From<ExprApplication> for Vec<u8> {
    fn from(ExprApplication { lhs, application }: ExprApplication) -> Self {
        match *application {
            Expr::Binary(ExprBinary {
                op,
                expr,
                result: Some(result),
            }) => format!(
                "{{ {} {} {} == {} }}",
                std::str::from_utf8(&Into::<Self>::into(*lhs)).unwrap(),
                std::str::from_utf8(&op).unwrap(),
                std::str::from_utf8(&Into::<Self>::into(*expr)).unwrap(),
                std::str::from_utf8(&Into::<Self>::into(*result)).unwrap()
            )
            .into(),
            Expr::Binary(ExprBinary {
                op,
                expr,
                result: None,
            }) => format!(
                "{{ {} {} {} }}",
                std::str::from_utf8(&Into::<Self>::into(*lhs)).unwrap(),
                std::str::from_utf8(&op).unwrap(),
                std::str::from_utf8(&Into::<Self>::into(*expr)).unwrap()
            )
            .into(),
            Expr::Unary(ExprUnary::Not { result: None }) => format!(
                "!{}",
                std::str::from_utf8(&Into::<Self>::into(*lhs)).unwrap()
            )
            .into(),
            Expr::Unary(ExprUnary::Not {
                result: Some(result),
            }) => format!(
                "{{ !{} == {} }}",
                std::str::from_utf8(&Into::<Self>::into(*lhs)).unwrap(),
                std::str::from_utf8(&Into::<Self>::into(*result)).unwrap(),
            )
            .into(),
            Expr::Value(ExprValue::Bit(expr)) => expr.into(),
            Expr::Value(ExprValue::Unsigned(expr)) => expr.into(),
            Expr::Value(ExprValue::Path(expr)) => expr.into(),
            Expr::Application(expr) => expr.into(),
        }
    }
}

impl From<Expr> for Vec<u8> {
    fn from(expr: Expr) -> Self {
        match expr {
            Expr::Application(expr) => expr.into(),
            expr => ExprApplication {
                lhs: Box::new(Expr::Value(ExprValue::Path(ValuePath(vec![
                    PathComponent::Ident("_".into()),
                ])))),
                application: Box::new(expr),
            }
            .into(),
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
            assert_eq!(std::str::from_utf8(&output).unwrap(), expected);
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
}
