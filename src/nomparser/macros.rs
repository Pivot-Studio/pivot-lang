#[macro_export]
macro_rules! del_newline_or_space {
    ($e:expr) => {
        nom::sequence::delimited(
            nom::multi::many0(nom::branch::alt((
                nom::bytes::complete::tag("\n"),
                nom::bytes::complete::tag("\r\n"),
                nom::bytes::complete::tag(" "),
                nom::bytes::complete::tag("\t"),
            ))),
            $e,
            nom::multi::many0(nom::branch::alt((
                nom::bytes::complete::tag("\n"),
                nom::bytes::complete::tag("\r\n"),
                nom::bytes::complete::tag(" "),
                nom::bytes::complete::tag("\t"),
            ))),
        )
    };
}

/// parse_bin_ops matches an expression, which is consisted by
/// the exp type separated by one of ops.
macro_rules! parse_bin_ops {
    ($exp:ident, $($op:ident),*) => {
        delspace(map_res(
            tuple((
                $exp,
                many0(tuple((
                    alt((
                        $(
                            tag_token_symbol(TokenType::$op),
                        )*
                    )),
                    $exp,
                ))),
            )),
            create_bin,
        ))
    };
}
