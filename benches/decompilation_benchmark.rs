use criterion::{black_box, criterion_group, criterion_main, Criterion};
use hermes_dec_rs::decompiler::Decompiler;

fn decompilation_benchmark(c: &mut Criterion) {
    c.bench_function("decompiler_creation", |b| {
        b.iter(|| {
            black_box(Decompiler::new().unwrap());
        });
    });
}

criterion_group!(benches, decompilation_benchmark);
criterion_main!(benches); 