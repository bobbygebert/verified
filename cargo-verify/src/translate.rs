use crate::parse::{Chunk, ChunkIter, Chunks};

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
                Chunk::Char(c) => {
                    buffer.push(c);
                }
                Chunk::Unsigned(u) => {
                    self.output.write_all(buffer.as_ref()).unwrap();
                    self.output.write_all(&Into::<Vec<u8>>::into(u)).unwrap();
                    buffer = Vec::new();
                }
            }
        }
        self.output.write_all(&buffer).unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn translator_replaces_unsigned() {
        let mut output = Vec::new();
        let translator = Translator::new(&mut output);

        let input = "
            xxx typenum::uint::UTerm
            xxx typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UTerm, typenum::bit::B1>, typenum::bit::B1>
            xxx
        ";
        translator.translate(input.as_bytes());

        let expected = "
            xxx U0
            xxx U3
            xxx
        ";

        assert_eq!(std::str::from_utf8(&output).unwrap(), expected);
    }
}
