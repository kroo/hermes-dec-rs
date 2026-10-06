use criterion::{black_box, criterion_group, criterion_main, Criterion};
use hermes_dec_rs::bundle::{export_bundle, BundleOptions};
use hermes_dec_rs::decompiler::Decompiler;
use hermes_dec_rs::HbcFile;
use std::path::Path;

fn decompilation_benchmark(c: &mut Criterion) {
    c.bench_function("decompiler_creation", |b| {
        b.iter(|| {
            black_box(Decompiler::new().unwrap());
        });
    });
    benchmark_bundle(c, Path::new("data/bundle_semantics.hbc"));
    // In-memory API measurements include parsing and validation, not file I/O.
    // The CLI script separately measures complete process/file wall time.
    if std::env::var_os("HERMES_BENCH_LARGE").is_some() {
        let mut inputs: Vec<_> = std::fs::read_dir("data/large_test_files")
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .filter(|path| path.extension().is_some_and(|extension| extension == "hbc"))
            .collect();
        inputs.sort();
        for path in inputs {
            benchmark_bundle(c, &path);
        }
    }
}

fn benchmark_bundle(c: &mut Criterion, input: &Path) {
    let bytes = std::fs::read(input).unwrap();
    let mut group = c.benchmark_group("full_bundle_parse_lower_validate");
    group.sample_size(10);
    group.bench_function(input.file_name().unwrap().to_string_lossy(), |b| {
        b.iter(|| {
            let hbc = HbcFile::parse(black_box(&bytes)).unwrap();
            black_box(export_bundle(&hbc, &BundleOptions::default()).unwrap());
        });
    });
    group.finish();
}

criterion_group!(benches, decompilation_benchmark);
criterion_main!(benches);
