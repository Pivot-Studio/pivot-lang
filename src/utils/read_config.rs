use std::{env, fs::read_to_string, path::PathBuf};

use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

use crate::{ast::compiler::COMPILE_PROGRESS, nomparser::SourceProgram, Db};

pub fn get_config_path(current: String) -> Result<String, &'static str> {
    let mut cur_path = PathBuf::from(current);
    if cur_path.is_file() && !cur_path.pop() {
        return Err("找不到配置文件～");
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
                    let res = p.to_string();
                    return Ok(res + "/Kagari.toml");
                } else {
                    return Err("找不到配置文件～");
                }
            }
        }
    }
    if !cur_path.pop() {
        return Err("找不到配置文件～");
    }
    let mut next_path = String::new();

    if let Some(p) = &cur_path.to_str() {
        next_path.push_str(p);
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
    #[serde(default)]
    pub path: String,
    pub git: Option<String>,
    pub head: Option<String>,
}

#[derive(Deserialize, Clone, Debug, PartialEq, Eq, Default, Hash, Serialize)]
pub struct ModSum {
    pub name: String,
    pub git: Option<GitInfo>,
}

#[derive(Deserialize, Clone, Debug, PartialEq, Eq, Default, Hash, Serialize)]
pub struct GitInfo {
    pub url: String,
    pub commit: String,
}

#[salsa::tracked]
pub fn get_config(db: &dyn Db, entry: SourceProgram) -> Result<Config, String> {
    let config = entry.text(db);
    let mut config_root = PathBuf::from(entry.path(db)); // xxx/Kagari.toml
    config_root.pop();
    let re = toml::from_str(config);
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
    let lib_path = libroot.clone();
    let libroot = libroot.read_dir();
    if libroot.is_err() {
        return Err("KAGARI_LIB_ROOT没有指向合法的目录，无法找到系统库".to_string());
    }
    let libroot = libroot.unwrap();
    for x in libroot {
        if let Ok(path) = x {
            if path.path().is_dir() && !path.file_name().eq("thirdparty") {
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
    let binding = lib_path.join("thirdparty");
    let third_party = binding.to_str().unwrap();
    let mut i = 1;
    let mut err = None;
    let lockfile = config_root.join("Kagari.lock");
    let mut sums = toml::from_str::<FxHashMap<String, ModSum>>(
        &read_to_string(lockfile.clone()).unwrap_or_default(),
    )
    .unwrap_or_default();
    let mut sum_changed = false;
    if config.deps.is_none() {
        config.deps = Some(deps);
    } else {
        let mut rawdeps = config.deps.clone().unwrap();
        let pb = &COMPILE_PROGRESS;
        if pb.length().is_none() {
            pb.set_length(rawdeps.len() as u64);
        } else {
            pb.inc_length(rawdeps.len() as u64);
        }
        // pb.set_prefix(format!("[{:3}/{:3}]", pb.position(), pb.length().unwrap()));
        pb.set_message("正在分析依赖");
        rawdeps.iter_mut().for_each(|(k, v)| {
            v.git
                .clone()
                .and_then(|git| {
                    pb.set_message("正在下载依赖");
                    // pb.set_prefix(format!("[{:3}/{:3}]", pb.position(), pb.length().unwrap()));
                    i += 1;
                    v.head
                        .clone()
                        .or_else(|| {
                            pb.abandon_with_message(format!(
                                "类型为git的依赖项{}未指定分支，无法下载依赖",
                                k
                            ));
                            err = Some("类型为git的依赖项未指定分支，无法下载依赖".to_string());
                            None
                        })
                        .map(|mut b| {
                            let (child, target) = kagari::download_repo(&git, third_party);
                            pb.set_message(format!("正在下载依赖{}", k));
                            if child.is_some() {
                                child.unwrap().unwrap();
                            }
                            if let Some(sum) = sums.get(k) {
                                b = sum.git.clone().unwrap().commit;
                            } else {
                                sum_changed = true;
                            }
                            let target = kagari::cp_to_hash_dir(target.to_str().unwrap(), &b);
                            sums.insert(
                                k.clone(),
                                ModSum {
                                    name: k.clone(),
                                    git: Some(GitInfo {
                                        url: git,
                                        commit: target
                                            .file_name()
                                            .unwrap()
                                            .to_str()
                                            .unwrap()
                                            .to_string(),
                                    }),
                                },
                            );
                            let mut dep = Dependency::default();
                            dep.path = target.to_string_lossy().to_string();
                            deps.insert(k.clone(), dep);
                        })
                })
                .or_else(|| {
                    // pb.set_prefix(format!("[{:3}/{:3}]", pb.position(), pb.length().unwrap()));
                    i += 1;
                    pb.set_message(format!("正在分析依赖{}", k));
                    if PathBuf::from(&v.path).is_absolute() {
                        _ = dunce::canonicalize(&v.path)
                            .map(|p| {
                                v.path = p.to_str().unwrap().to_string();
                                p
                            })
                            .map_err(|e| {
                                pb.abandon_with_message(format!("error: {:?}", e));
                                err = Some(format!("error: {:?}", e));
                                format!("error: {:?}", e)
                            });
                    } else {
                        _ = dunce::canonicalize(config_root.join(&v.path))
                            .map(|p| {
                                v.path = p.to_str().unwrap().to_string();
                                p
                            })
                            .map_err(|e| {
                                pb.abandon_with_message(format!("error: {:?}", e));
                                err = Some(format!("error: {:?}", e));
                                format!("error: {:?}", e)
                            });
                    }
                    deps.insert(k.clone(), v.clone());
                    None
                });
            pb.inc(1);
        });
        if err.is_some() {
            return Err(err.unwrap());
        }
        config.deps = Some(deps);
    }
    if sum_changed {
        toml::to_string_pretty(&sums)
            .map_err(|e| format!("error: {:?}", e))
            .and_then(|s| std::fs::write(lockfile, s).map_err(|e| format!("error: {:?}", e)))?;
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
