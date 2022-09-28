use internal_macro::is_runtime;
mod gc;

#[is_runtime] // jit注册
fn addtest(left: usize, right: usize) -> usize {
    let a = vec!["a"];
    for b in a {
        println!("{}", b);
    }
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        let result = addtest(2, 2);
        assert_eq!(result, 4);
    }
}
