use std::{env, path::PathBuf};

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
    let dir = cur_path.read_dir();
    if dir.is_err() {
        return Err("找不到配置文件～");
    }
    let dir = dir.unwrap();
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

#[derive(Deserialize, Clone, Debug, PartialEq, Eq, Default)]
pub struct Config {
    pub project: String,
    pub entry: String,
    pub deps: Option<FxHashMap<String, Dependency>>,
    #[serde(skip)]
    pub root: String,
}

#[derive(Deserialize, Clone, Debug, PartialEq, Eq, Default)]
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
    let mut config: Config = re.unwrap();
    let libroot = env::var("KAGARI_LIB_ROOT");
    if libroot.is_err() {
        return Err("未设置环境变量KAGARI_LIB_ROOT，无法找到系统库".to_string());
    }
    let mut deps = FxHashMap::<String, Dependency>::default();
    let libroot = PathBuf::from(libroot.unwrap());
    let libroot = libroot.read_dir();
    if libroot.is_err() {
        return Err("KAGARI_LIB_ROOT没有指向合法的目录，无法找到系统库".to_string());
    }
    let libroot = libroot.unwrap();
    for x in libroot {
        if let Ok(path) = x {
            if path.path().is_dir() {
                let mut dep = Dependency::default();
                dep.path = path.path().to_str().unwrap().to_string();
                deps.insert(path.file_name().to_str().unwrap().to_string(), dep);
            }
        }
    }
    if config.deps.is_none() {
        config.deps = Some(deps);
    } else {
        let mut rawdeps = config.deps.unwrap();
        for (k, v) in rawdeps.iter_mut() {
            deps.insert(k.clone(), v.clone());
        }
        config.deps = Some(deps);
    }
    Ok(config)
}
