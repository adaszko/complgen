#![feature(test)]

extern crate test;

#[cfg(test)]
mod tests {
    #[bench]
    fn bench_cargo_grammar(b: &mut test::Bencher) {
        let cargo_grammar = include_str!("../cargo.usage");

        b.iter(|| {
        });
    }
}
