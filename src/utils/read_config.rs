use std::path::PathBuf;
#[warn(unused_must_use)]
pub fn get_config_path(current: &str) -> String {
    let mut cur_path = PathBuf::from(current);
    if cur_path.is_file() {
        if cur_path.pop() == false {
            println!("找不到配置文件～");
            return "".to_string();
        }
    }
    let dir = cur_path.read_dir().unwrap();
    for x in dir {
        if let Ok(path) = x {
            if path.file_name().eq("Kagari.toml") {
                if let Some(p) = cur_path.to_str() {
                    return p.to_string();
                } else {
                    println!("找不到配置文件～")
                }
            }
        }
    }
    if cur_path.pop() == false {
        println!("找不到配置文件～");
        return "".to_string();
    }
    let mut next_path = String::new();
    if let Some(p) = cur_path.to_str() {
        next_path.push_str(&p.to_string());
    }
    return get_config_path(&next_path);
}
