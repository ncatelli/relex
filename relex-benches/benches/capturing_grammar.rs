use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use relex_derive::Relex;

fn float_validator(lexed_input: &str) -> Option<f32> {
    lexed_input.parse::<f32>().ok()
}

#[derive(Relex, Debug, PartialEq)]
pub enum Token {
    #[matches(r"[0-9]+[.][0-9]+", float_validator)]
    FloatLit(f32),
    #[matches(r"[0-9]+", |lex: &str| { lex.parse::<i32>().ok() })]
    IntLit(i32),
    #[matches("\"[a-zA-Z]+\"", |lex: &str| { Some(lex.trim_matches('\"').to_string()) })]
    StringLit(String),
    #[matches(r"+")]
    Plus,
    #[matches(r"-")]
    Minus,
    #[matches("\n")]
    Newline,
    #[skip(" |\t")]
    WhiteSpace,
    #[matches("[.]")]
    Dot,
    #[eoi]
    Eoi,
}

fn short_input_stream(c: &mut Criterion) {
    let mut group = c.benchmark_group("benchmarking stream instantiation.");

    let input = "0 4 0 4 1 - 8 5 8 9 7 - 7 4 1 2 7 + 6 7 6 8 6 4 - 6 4 8";

    [1, 2, 4, 16].into_iter().for_each(|sample_size| {
        group.throughput(Throughput::Elements(sample_size as u64));
        group.bench_with_input(
            BenchmarkId::new("token count of", sample_size),
            &(sample_size),
            |b, &input_size| {
                b.iter(|| {
                    let mut cnt = 0;
                    let stream = token_stream_from_input(black_box(input))
                        .unwrap()
                        .take(input_size);

                    for _tok in stream {
                        cnt += 1;
                    }

                    assert_eq!(cnt, sample_size)
                })
            },
        );
    })
}

criterion_group!(benches, short_input_stream);
criterion_main!(benches);
