use relex_derive::Relex;

#[derive(Relex, Debug, PartialEq, Eq)]
pub enum Token {
    #[regex(r"[0-9]+", |lex: &str| { lex.parse::<i32>().ok() })]
    Int(i32),
    #[regex(r"n")]
    N,
}

fn main() -> Result<(), String> {
    let mut stream = token_stream_from_input("12345n4n")?;

    println!("{:?}", stream.next());
    println!("{:?}", stream.next());
    println!("{:?}", stream.next());
    println!("{:?}", stream.next());
    Ok(())
}
