struct header<'a> {
    size: u64,
    next: &'a header<'a>,
}



