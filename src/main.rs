use color_eyre::Result;
use dac::{parser, Expr};
use im_rc::HashMap;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::char,
    combinator::{eof, map, value},
    error::Error,
    sequence::{preceded, terminated},
    Finish, IResult,
};
use std::{
    io::{self, BufRead, Write},
    str::FromStr,
};

#[derive(Clone)]
enum Command {
    Quit,
    Normalize(Expr),
}

impl FromStr for Command {
    type Err = Error<String>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match terminated(parse_command, eof)(s.trim()).finish() {
            Ok((_remaining, command)) => Ok(command),
            Err(Error { input, code }) => Err(Error {
                input: input.to_string(),
                code,
            }),
        }
    }
}

fn parse_command(input: &str) -> IResult<&str, Command> {
    alt((
        value(
            Command::Quit,
            preceded(char(':'), alt((tag("q"), tag("quit")))),
        ),
        map(parser::expr, Command::Normalize),
    ))(input)
}

fn main() {
    loop {
        match handle(&mut io::stdin().lock(), &mut io::stdout()) {
            Ok(true) => continue,
            Ok(false) => break,
            Err(e) => eprintln!("{e}"),
        }
    }
}

fn handle(input: &mut impl BufRead, output: &mut impl Write) -> Result<bool> {
    let mut line = String::new();
    write!(output, ">> ")?;
    output.flush()?;
    input.read_line(&mut line)?;
    let command = line.parse()?;

    match command {
        Command::Quit => Ok(false),
        Command::Normalize(expr) => {
            let ty = expr.synth(HashMap::new())?;
            let normalized = expr.normalize()?;
            writeln!(output, "{normalized} : {ty}")?;
            Ok(true)
        }
    }
}
