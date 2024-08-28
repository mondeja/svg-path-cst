use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;
use svg_path_cst::{svg_path_cst, SVGPathCSTNode, SyntaxError};

fn parse(data: &[u8]) -> Result<Vec<SVGPathCSTNode>, SyntaxError> {
    svg_path_cst(data)
}

fn performance(c: &mut Criterion) {
    let data = b"M 10 10 L 20 20";
    c.bench_function("basic", |b| b.iter(|| parse(black_box(data))));

    let simpleicons_data = include_bytes!("../fuzz/corpus/simpleicons.txt");
    c.bench_function("simpleicons", |b| {
        b.iter(|| parse(black_box(simpleicons_data)))
    });

    let elsevier_data = include_bytes!("../fuzz/corpus/elsevier.txt");
    c.bench_function("elsevier", |b| b.iter(|| parse(black_box(elsevier_data))));
}

criterion_group!(benches, performance);
criterion_main!(benches);
