#![feature(
    box_syntax,
    box_patterns,
    iter_intersperse,
    generators,
    iter_from_generator,
    generator_trait
)]

use color_eyre::{eyre::ContextCompat, Report, Result};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, i32, multispace0, one_of},
    combinator::{eof, map, value},
    error::{Error, ParseError},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, preceded, terminated},
    Finish, IResult,
};
use std::{
    fmt, io,
    ops::{Deref, DerefMut},
    str::FromStr,
};

enum Token {
    Lit(i32),
    Times,
    Plus,
    Minus,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lit(v) => v.fmt(f),
            Self::Times => '*'.fmt(f),
            Self::Plus => '+'.fmt(f),
            Self::Minus => '-'.fmt(f),
        }
    }
}

#[derive(Clone, Debug)]
enum Ast {
    Lit(i32),
    Times(Box<Ast>, Box<Ast>),
    Plus(Box<Ast>, Box<Ast>),
    Minus(Box<Ast>, Box<Ast>),
}

impl IntoIterator for Ast {
    type Item = Token;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        let mut tokens = vec![];
        self.tokenize(&mut tokens);
        tokens.into_iter()
    }
}

impl FromStr for Ast {
    type Err = Error<String>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match terminated(ast, eof)(s.trim()).finish() {
            Ok((_remaining, ast)) => Ok(ast),
            Err(Error { input, code }) => Err(Error {
                input: input.to_string(),
                code,
            }),
        }
    }
}

impl Ast {
    fn tokenize(self, mut tokens: &mut Vec<Token>) {
        match self {
            Ast::Lit(v) => tokens.push(Token::Lit(v)),
            Ast::Times(a, b) => {
                a.tokenize(&mut tokens);
                b.tokenize(&mut tokens);
                tokens.push(Token::Times);
            }
            Ast::Plus(a, b) => {
                a.tokenize(&mut tokens);
                b.tokenize(&mut tokens);
                tokens.push(Token::Plus);
            }
            Ast::Minus(a, b) => {
                a.tokenize(&mut tokens);
                b.tokenize(&mut tokens);
                tokens.push(Token::Minus);
            }
        }
    }
}

struct Expr(Vec<Token>);

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut it = self.iter().peekable();

        while let Some(token) = it.next() {
            token.fmt(f)?;

            if it.peek().is_some() {
                ' '.fmt(f)?;
            }
        }

        Ok(())
    }
}

impl From<Ast> for Expr {
    fn from(value: Ast) -> Self {
        Expr(value.into_iter().collect())
    }
}

impl TryFrom<Expr> for Ast {
    type Error = Report;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        let mut it = value.iter();

        while let Some(token) = it.next() {
            match token {
                Token::Lit(_v) => {
                    todo!();
                }
                Token::Times => {
                    todo!();
                }
                Token::Plus => {
                    todo!();
                }
                Token::Minus => {
                    todo!();
                }
            }
        }

        todo!()
    }
}

impl Expr {
    fn eval(self) -> Result<i32> {
        let mut stack: Vec<i32> = vec![];

        for token in self.0 {
            match token {
                Token::Lit(v) => stack.push(v),
                Token::Times => {
                    let b = stack.pop().wrap_err("TODO")?;
                    let a = stack.pop().wrap_err("TODO")?;
                    stack.push(a * b);
                }
                Token::Plus => {
                    let b = stack.pop().wrap_err("TODO")?;
                    let a = stack.pop().wrap_err("TODO")?;
                    stack.push(a + b);
                }
                Token::Minus => {
                    let b = stack.pop().wrap_err("TODO")?;
                    let a = stack.pop().wrap_err("TODO")?;
                    stack.push(a - b);
                }
            }
        }

        stack.get(0).copied().wrap_err("TODO")
    }
}

impl Deref for Expr {
    type Target = Vec<Token>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Expr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
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

fn handle(input: &mut impl io::BufRead, output: &mut impl io::Write) -> Result<bool> {
    let mut line = String::new();
    write!(output, ">> ")?;
    output.flush()?;
    input.read_line(&mut line)?;
    let command = line.parse()?;

    match command {
        Command::Quit => Ok(false),
        Command::Eval(ast) => {
            println!("{:?}", ast);
            Ok(true)
        }
    }
}

#[derive(Clone)]
enum Command {
    Quit,
    Eval(Ast),
}

impl FromStr for Command {
    type Err = Error<String>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match terminated(command, eof)(s.trim()).finish() {
            Ok((_remaining, command)) => Ok(command),
            Err(Error { input, code }) => Err(Error {
                input: input.to_string(),
                code,
            }),
        }
    }
}

fn command(input: &str) -> IResult<&str, Command> {
    alt((
        value(
            Command::Quit,
            preceded(char(':'), alt((tag("q"), tag("quit")))),
        ),
        map(ast, Command::Eval),
    ))(input)
}

fn factor(input: &str) -> IResult<&str, Ast> {
    alt((parens(ws(ast)), map(i32, Ast::Lit)))(input)
}

fn term(input: &str) -> IResult<&str, Ast> {
    map(separated_list1(ws(char('*')), factor), |list| {
        list.into_iter()
            .reduce(|a, b| Ast::Times(box a, box b))
            .unwrap()
    })(input)
}

fn ast(input: &str) -> IResult<&str, Ast> {
    map(
        pair(term, many0(pair(ws(one_of("+-")), term))),
        |(a, list)| {
            list.into_iter().fold(a, |a, (op, b)| match op {
                '+' => Ast::Plus(box a, box b),
                '-' => Ast::Minus(box a, box b),
                _ => unreachable!(),
            })
        },
    )(input)
}

fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

fn parens<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(char('('), inner, char(')'))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let ast: Ast = "10 - (4 + 3) * 2".parse().unwrap();
        let expr: Expr = ast.into();

        assert_eq!(expr.to_string(), "10 4 3 + 2 * -");

        let result = expr.eval().unwrap();

        assert_eq!(result, -4);
    }
}
