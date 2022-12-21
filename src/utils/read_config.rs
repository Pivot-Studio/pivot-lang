use std::{env, path::PathBuf};

use serde::Deserialize;
use std::collections::BTreeMap;

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

#[derive(Deserialize, Clone, Debug, PartialEq, Eq, Default, Hash)]
pub struct Config {
    pub project: String,
    pub entry: String,
    pub deps: Option<BTreeMap<String, Dependency>>,
    #[serde(skip)]
    pub root: String,
}

#[derive(Deserialize, Clone, Debug, PartialEq, Eq, Default, Hash)]
pub struct Dependency {
    pub version: Option<String>,
    pub path: String,
}

pub fn get_config(db: &dyn Db, entry: SourceProgram) -> Result<Config, String> {
    let config = entry.text(db);
    let mut config_root = PathBuf::from(entry.path(db)); // xxx/Kagari.toml
    config_root.pop();
    let re = toml::from_str(&config);
    if let Err(re) = re {
        return Err(format!("配置文件解析错误:{:?}", re));
    }
    let mut config: Config = re.unwrap();
    let libroot = env::var("KAGARI_LIB_ROOT");
    if libroot.is_err() {
        return Err("未设置环境变量KAGARI_LIB_ROOT，无法找到系统库".to_string());
    }
    let mut deps = BTreeMap::<String, Dependency>::default();
    let libroot = dunce::canonicalize(PathBuf::from(libroot.unwrap())).unwrap();
    let libroot = libroot.read_dir();
    if libroot.is_err() {
        return Err("KAGARI_LIB_ROOT没有指向合法的目录，无法找到系统库".to_string());
    }
    let libroot = libroot.unwrap();
    for x in libroot {
        if let Ok(path) = x {
            if path.path().is_dir() {
                let mut dep = Dependency::default();
                dep.path = dunce::canonicalize(path.path())
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string();
                deps.insert(path.file_name().to_str().unwrap().to_string(), dep);
            }
        }
    }
    if config.deps.is_none() {
        config.deps = Some(deps);
    } else {
        let mut rawdeps = config.deps.unwrap();
        for (k, v) in rawdeps.iter_mut() {
            if PathBuf::from(&v.path).is_absolute() {
                v.path = dunce::canonicalize(&v.path)
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string();
            } else {
                v.path = dunce::canonicalize(config_root.join(&v.path))
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string();
            }
            deps.insert(k.clone(), v.clone());
        }
        config.deps = Some(deps);
    }
    config.root = dunce::canonicalize(config_root.clone())
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();
    config.entry = dunce::canonicalize(config_root.join(&config.entry))
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();
    Ok(config)
}
#[cfg(target_os = "linux")]
pub fn enter() -> &'static str {
    "\n"
}
#[cfg(target_os = "macos")]
pub fn enter() -> &'static str {
    "\n"
}
#[cfg(target_os = "windows")]
pub fn enter() -> &'static str {
    "\r\n"
}
