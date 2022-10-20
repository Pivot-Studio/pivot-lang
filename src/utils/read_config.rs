use std::path::PathBuf;

pub fn get_config_path(current: String) -> Result<String, &'static str> {
    let mut cur_path = PathBuf::from(current);
    if cur_path.is_file() {
        if cur_path.pop() == false {
            return Err("找不到配置文件～");
        }
    }
    let dir = cur_path.read_dir().unwrap();
    for x in dir {
        if let Ok(path) = x {
            if path.file_name().eq("Kagari.toml") {
                if let Some(p) = cur_path.to_str() {
                    let res = String::from(p.to_string());
                    return Ok(res);
                } else {
                    return Err("找不到配置文件～");
                }
            }
        }
    }
    if cur_path.pop() == false {
        return Err("找不到配置文件～");
    }
    let mut next_path = String::new();

    if let Some(p) = &cur_path.to_str() {
        next_path.push_str(&p);
    } else {
        return Err("找不到配置文件～");
    }
    return get_config_path(next_path);
}
