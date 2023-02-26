macro_rules! del_newline_or_space {
    ($e:expr) => {
        delimited(
            many0(alt((tag("\n"), tag("\r\n"), tag(" "), tag("\t")))),
            $e,
            many0(alt((tag("\n"), tag("\r\n"), tag(" "), tag("\t")))),
        )
    };
}

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

/// # semi_statement
/// 将原始parser接上一个分号
///
/// 对缺少分号的情况能自动进行错误容忍
///
/// 所有分号结尾的statement都应该使用这个宏来处理分号
macro_rules! semi_statement {
    ($e:expr) => {
        alt((terminated($e, tag_token_symbol(TokenType::SEMI)), map_res(tuple(($e,recognize(many0(alt((tag("\n"), tag("\r\n"), preceded(many0(tag(" ")),tag("\n")), tag("\t"))))))), |(node,e)|{
            let range = node.range();
            let r = Range::new(e,e);
            res_enum(StErrorNode{
                range,
                st: node,
                err:ErrorNode{
                    range: r,//Range{start: range.end, end: range.end},
                    msg: "missing semi".to_string(),
                    code: ErrorCode::MISSING_SEMI,
                    src:"".to_string(),
                },
            }.into())
        })))
    };
}
