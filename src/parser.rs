use crate::Expr;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0, u64, u8},
    combinator::{map, opt, recognize, value},
    error::ParseError,
    multi::{many0_count, separated_list1},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn primitive(input: &str) -> IResult<&str, Expr> {
    alt((
        map(
            preceded(char('u'), opt(preceded(multispace0, parens(ws(u8))))),
            Expr::U,
        ),
        value(Expr::Nat, tag("nat")),
        value(Expr::Zero, tag("zero")),
        map(
            preceded(pair(tag("succ"), multispace0), parens(ws(expr))),
            |e| Expr::Succ(box e),
        ),
        map(
            preceded(
                pair(tag("the"), multispace0),
                parens(pair(ws(expr), preceded(char(','), ws(expr)))),
            ),
            |(e_1, e_2)| Expr::The(box e_1, box e_2),
        ),
        map(
            preceded(
                pair(tag("lam"), multispace0),
                parens(pair(ws(identifier), preceded(char(','), ws(expr)))),
            ),
            |(x, e)| Expr::Lam(x.to_owned(), box e),
        ),
        map(u64, Expr::NatLit),
        map(
            preceded(
                pair(tag("pi"), multispace0),
                parens(tuple((
                    ws(identifier),
                    preceded(char(','), ws(expr)),
                    preceded(char(','), ws(expr)),
                ))),
            ),
            |(x, e_1, e_2)| Expr::Pi(x.to_owned(), box e_1, box e_2),
        ),
        map(identifier, |i| Expr::Var(i.to_owned())),
    ))(input)
}

fn factor(input: &str) -> IResult<&str, Expr> {
    alt((parens(ws(expr)), primitive))(input)
}

fn term(input: &str) -> IResult<&str, Expr> {
    map(
        pair(factor, opt(parens(separated_list1(char(','), ws(expr))))),
        |(e, o)| match o {
            None => e,
            Some(args) => args.into_iter().fold(e, |a, b| Expr::App(box a, box b)),
        },
    )(input)

    //     alt((
    //         parens(ws(expr)),
    //         map(
    //             pair(factor, opt(parens(separated_list1(char(','), ws(expr))))),
    //             |(e, o)| match o {
    //                 None => e,
    //                 Some(args) => args.into_iter().fold(e, |a, b| Expr::App(box a, box b)),
    //             },
    //         ),
    //     ))(input)
}

pub fn expr(input: &str) -> IResult<&str, Expr> {
    map(separated_list1(ws(char('+')), term), |list| {
        list.into_iter()
            .reduce(|a, b| Expr::Plus(box a, box b))
            .unwrap()
    })(input)
}

// pub fn expr(input: &str) -> IResult<&str, Expr> {
//     alt((
//         parens(ws(expr)),
//         map(
//             pair(factor, opt(parens(separated_list1(char(','), ws(expr))))),
//             |(e, o)| match o {
//                 None => e,
//                 Some(args) => args.into_iter().fold(e, |a, b| Expr::App(box a, box b)),
//             },
//         ),
//     ))(input)
// }

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
