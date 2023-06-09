use color_eyre::{eyre::eyre, Result};
use dac::{parser, Ctx, Expr, Ty};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::char,
    combinator::{eof, map, value},
    error::Error,
    sequence::{preceded, separated_pair, terminated},
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
    Assume(String, Expr),
}

impl FromStr for Command {
    type Err = Error<String>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match terminated(parse_command, eof)(s.trim_end()).finish() {
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
        map(
            preceded(
                tag(":assume"),
                separated_pair(
                    parser::ws(parser::identifier),
                    char(':'),
                    parser::ws(parser::expr),
                ),
            ),
            |(s, e)| Command::Assume(s.to_owned(), e),
        ),
        map(parser::ws(parser::expr), Command::Normalize),
    ))(input)
}

fn main() {
    let mut ctx = Ctx::new();

    loop {
        match handle(&mut io::stdin().lock(), &mut io::stdout(), &mut ctx) {
            Ok(true) => continue,
            Ok(false) => break,
            Err(e) => eprintln!("{e}"),
        }
    }
}

fn handle(input: &mut impl BufRead, output: &mut impl Write, ctx: &mut Ctx) -> Result<bool> {
    let mut line = String::new();
    write!(output, ">> ")?;
    output.flush()?;
    input.read_line(&mut line)?;
    let command = line.parse()?;

    match command {
        Command::Quit => Ok(false),
        Command::Normalize(expr) => {
            let normal = expr.normalize(ctx.to_owned())?;
            let ty = normal.ty.read_back_typed(&Ty::U(u8::MAX), ctx.to_owned());
            let val = normal.read_back(ctx.to_owned());
            writeln!(output, "{val} : {ty}")?;
            Ok(true)
        }
        Command::Assume(name, expr) => {
            if ctx.contains_key(&name) {
                return Err(eyre!("{name} already exists"));
            }

            let v = expr.eval(ctx.clone().into())?;
            let ty: Ty = v.try_into()?;

            ctx.insert(name, (ty, None));

            Ok(true)
        }
    }
}
