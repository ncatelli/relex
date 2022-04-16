use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use relex_runtime::*;

fn pad_input_to_length_with(suffix: &str, pad_str: &str, len: usize) -> String {
    let suffix_len = suffix.chars().count();
    let req_padding = len - suffix_len;

    if suffix_len > len {
        "".to_string()
    } else {
        pad_str
            .chars()
            .cycle()
            .take(req_padding)
            .chain(suffix.chars())
            .collect()
    }
}

pub fn linear_input_size_comparison(c: &mut Criterion) {
    let mut group = c.benchmark_group("exponential input length comparison");
    let input = "ab";
    let pad = "xy";
    let prog = Instructions::new(vec![
        Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(1))),
        Opcode::Any,
        Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
        Opcode::StartSave(InstStartSave::new(0)),
        Opcode::Consume(InstConsume::new('a')),
        Opcode::Consume(InstConsume::new('b')),
        Opcode::EndSave(InstEndSave::new(0)),
        Opcode::Match,
    ]);

    (1..10)
        .map(|exponent| 2usize.pow(exponent))
        .map(|input_len| (pad_input_to_length_with(input, pad, input_len), input_len))
        .for_each(|(input, sample_size)| {
            group.throughput(Throughput::Elements(sample_size as u64));
            group.bench_with_input(
                BenchmarkId::new("find match with sample size of defined length", sample_size),
                &(input, sample_size),
                |b, (input, input_size)| {
                    let expected_res = SaveGroupSlot::complete(0, *input_size - 2, *input_size);

                    b.iter(|| {
                        let res = run::<1>(prog.as_ref(), input);
                        assert_eq!(
                            Some(Some(&expected_res)),
                            res.as_ref().map(|slots| slots.get(0))
                        )
                    })
                },
            );
        })
}

pub fn linear_input_size_comparison_against_set_match(c: &mut Criterion) {
    let mut group = c.benchmark_group("exponential input length comparison for set matching");
    let input = "ab";
    let pad = "xy";

    let match_set = ['a'..='z', 'A'..='Z', '0'..='9', '_'..='_'];
    let prog = Instructions::new(vec![
        Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(1))),
        Opcode::Any,
        Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
        Opcode::StartSave(InstStartSave::new(0)),
        Opcode::ConsumeSet(InstConsumeSet::inclusive(CharacterSet::Ranges(
            match_set.to_vec(),
        ))),
        Opcode::ConsumeSet(InstConsumeSet::inclusive(CharacterSet::Ranges(
            match_set.to_vec(),
        ))),
        Opcode::EndSave(InstEndSave::new(0)),
        Opcode::Match,
    ]);

    (1..10)
        .map(|exponent| 2usize.pow(exponent))
        .map(|input_len| (pad_input_to_length_with(input, pad, input_len), input_len))
        .for_each(|(input, sample_size)| {
            group.throughput(Throughput::Elements(sample_size as u64));
            group.bench_with_input(
                BenchmarkId::new(
                    "find match in set with sample size of defined length",
                    sample_size,
                ),
                &(input, sample_size),
                |b, (input, input_size)| {
                    let expected_res = SaveGroupSlot::complete(0, *input_size - 2, *input_size);

                    b.iter(|| {
                        let res = run::<1>(prog.as_ref(), input);
                        assert_eq!(
                            Some(Some(&expected_res)),
                            res.as_ref().map(|slots| slots.get(0))
                        )
                    })
                },
            );
        })
}

criterion_group!(
    benches,
    linear_input_size_comparison,
    linear_input_size_comparison_against_set_match
);
criterion_main!(benches);