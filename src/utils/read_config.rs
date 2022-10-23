use std::path::PathBuf;

use rustc_hash::FxHashMap;
use serde::Deserialize;

use crate::{nomparser::SourceProgram, Db};

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
                    return Ok(res + "/Kagari.toml");
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

#[derive(Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Config {
    pub entry: String,
    pub deps: Option<FxHashMap<String, Dependency>>,
}

#[derive(Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Dependency {
    pub version: Option<String>,
    pub path: String,
}

pub fn get_config(db: &dyn Db, entry: SourceProgram) -> Result<Config, String> {
    let config = entry.text(db);
    let re = toml::from_str(&config);
    if let Err(re) = re {
        return Err(format!("配置文件解析错误:{:?}", re));
    }
    let config: Config = re.unwrap();
    Ok(config)
}
