use std::{env, fs::read_to_string, path::PathBuf};

use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use ustr::Ustr;

use crate::{
    ast::{compiler::COMPILE_PROGRESS, node::pkg::UseNode},
    nomparser::SourceProgram,
    Db,
};

#[cfg(feature = "repl")]
use crate::repl::{REPL_VIRTUAL_CONF, REPL_VIRTUAL_ENTRY};

static KAGARI_CONFIG_FILE: &str = "Kagari.toml";

/// search_config_file search configuration file KAGARI_CONFIG_FILE recursively from current path and return the file path,
/// if it isn't found in current path, search_config_file will search at its parent folder util there is no more parent.
pub fn search_config_file(current: String) -> Result<String, &'static str> {
    log::trace!("get_config_path: {:?}", current);
    #[cfg(target_arch = "wasm32")] // TODO support std on wasm
    {
        if current.starts_with("http") {
            return Ok("http://www.test.com/Kagari.toml".to_string());
        }
        if current.starts_with("core") {
            return Ok("core/Kagari.toml".to_string());
        } else if current.starts_with("std") {
            return Ok("std/Kagari.toml".to_string());
        }
    }
    #[cfg(feature = "repl")]
    if current == REPL_VIRTUAL_ENTRY {
        return Ok(REPL_VIRTUAL_CONF.to_string());
    }

    let mut cur_path = PathBuf::from(current);
    if cur_path.is_file() && !cur_path.pop() {
        return Err("找不到配置文件～");
    }
    let re = cur_path.read_dir();

    if re.is_err() {
        return Err("找不到配置文件～");
    }
    let iter = re.unwrap();
    for f in iter.flatten() {
        if f.file_name().eq(KAGARI_CONFIG_FILE) {
            let p = f.path();
            let kagari_file_path = p.to_str().unwrap();
            return Ok(kagari_file_path.to_string());
        }
    }

    if !cur_path.pop() {
        return Err("找不到配置文件～");
    }

    if let Some(parent) = &cur_path.to_str() {
        search_config_file(parent.to_string())
    } else {
        Err("找不到配置文件～")
    }
}

/// Config is the code representation of the configuration with toml format in a kagari.toml file.
/// Each config stands for a pivot-lang project.
#[derive(Deserialize, Serialize, Clone, Debug, PartialEq, Eq, Default, Hash)]
pub struct Config {
    /// project is the name of a pivot-lang project
    pub project: String,
    /// entry is the entry file a pivot-lang project,
    /// usually it's used as a entry point to parse the whole project
    pub entry: String,

    /// deps stands for the dependencies used in the project
    /// the key is the name of a library
    /// the dependency is the concrete information to access the library
    pub deps: Option<BTreeMap<Ustr, Dependency>>,

    /// root represents the root path of a project,
    /// and it's decided by the position of kagari.toml file
    #[serde(skip)]
    pub root: String,
}

/// ConfigWrapper wraps a config, which represents all configuration of an entry node of a program.
/// it's typically used to resolve path for the dependency used in use statement to the real path the library is downloaded
#[salsa::tracked]
pub struct ConfigWrapper {
    #[return_ref]
    config: Config,
    #[return_ref]
    pub use_node: UseNode,
}

#[salsa::tracked]
impl ConfigWrapper {
    /// # resolve_dep_path
    ///
    /// resolve_dep_path resolves the path of a used library according to the name.
    /// for example, in a pivot-lang file:
    /// ```rust
    /// use project1::test::tuple;
    /// ```
    /// we will use `project1` to resolve the path of the project
    #[salsa::tracked]
    pub(crate) fn resolve_dep_path(self, db: &dyn Db) -> PathBuf {
        let u = self.use_node(db);
        let mut path = PathBuf::from(self.config(db).root.clone());
        if let Some(cm) = &self.config(db).deps {
            if let Some(dep) = cm.get(&u.namespace[0].name) {
                path = path.join(&dep.path);
            }
        }

        // skip the length validation as the valid namespaces always contains more than one element
        for p in u.namespace[1..].iter() {
            path = path.join(p.name.as_str());
        }

        // because the namespace is classified based on each file, hence we need to append the pi file extension
        path = path.with_extension("pi");
        path
    }
}

#[derive(Deserialize, Serialize, Clone, Debug, PartialEq, Eq, Default, Hash)]
pub struct Dependency {
    pub version: Option<String>,
    pub git: Option<String>,
    pub head: Option<String>,

    /// path is the place where the dependency code could be access locally,
    /// it's used to load the code during parsing use statements
    #[serde(default)]
    pub path: String,
}

/// ModSum represents the revision of a module imported by a pivot-lang project as a dependency.
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
#[cfg(target_arch = "wasm32")]
pub fn prepare_build_envs(db: &dyn Db, entry: SourceProgram) -> Result<Config, String> {
    let config = entry.text(db);
    let mut config_root = PathBuf::from(entry.path(db)); // xxx/Kagari.toml
    config_root.pop();
    let re = toml::from_str(config);
    if let Err(re) = re {
        return Err(format!("配置文件解析错误:{:?}", re));
    }
    let mut config: Config = re.unwrap();
    // let d = crate::lsp::wasm::PLLIB_DIR;
    let mut deps = BTreeMap::<String, Dependency>::default();
    for path in crate::lsp::wasm::PLLIB_DIR.dirs() {
        if !path.contains("thirdparty") {
            let dep = Dependency {
                path: crate::utils::canonicalize(path.path())
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string(),
                ..Default::default()
            };
            deps.insert(
                path.path().file_name().unwrap().to_str().unwrap().into(),
                dep,
            );
        }
    }
    config.deps = Some(deps);
    return Ok(config);
}

/// prepare_build_envs parses and validates the kagari.toml file,
/// loads the system library according to the path inside environ,
/// and loads all outside depdencies by git with sumcheck validation into the project configuration.
#[salsa::tracked]
#[cfg(not(target_arch = "wasm32"))]
pub fn prepare_build_envs(db: &dyn Db, kagari_source: SourceProgram) -> Result<Config, String> {
    let mut config_root = PathBuf::from(kagari_source.path(db));
    config_root.pop();

    // process the kagari file
    let re_kagari_config = toml::from_str(kagari_source.text(db));
    if let Err(re) = re_kagari_config {
        return Err(format!("配置文件解析错误:{:?}", re));
    }
    let mut config: Config = re_kagari_config.unwrap();

    let re_libroot = env::var("KAGARI_LIB_ROOT");
    if re_libroot.is_err() {
        return Err("未设置环境变量KAGARI_LIB_ROOT, 无法找到系统库".to_string());
    }

    let libroot = crate::utils::canonicalize(PathBuf::from(re_libroot.unwrap())).unwrap();
    let lib_path = libroot.clone();
    let libroot = libroot.read_dir();
    if libroot.is_err() {
        return Err("KAGARI_LIB_ROOT没有指向合法的目录, 无法找到系统库".to_string());
    }

    // load all built-in libraries as project depdencies by default
    let mut deps = BTreeMap::<Ustr, Dependency>::default();
    for path in libroot.unwrap().flatten() {
        if path.path().is_dir() && !path.file_name().eq("thirdparty") {
            let dep = Dependency {
                path: crate::utils::canonicalize(path.path())
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string(),
                ..Default::default()
            };
            deps.insert(path.file_name().to_str().unwrap().to_string().into(), dep);
        }
    }

    let lockfile = config_root.join("Kagari.lock");
    // load checksums for all dependencies from kagari.lock file to validate the code downloaded by git
    let mut sums = toml::from_str::<FxHashMap<String, ModSum>>(
        &read_to_string(lockfile.clone()).unwrap_or_default(),
    )
    .unwrap_or_default();

    let mut sum_changed = false;
    let binding = lib_path.join("thirdparty");
    let third_party = binding.to_str().unwrap();
    let mut i = 1;
    let mut err = None;
    if config.deps.is_none() {
        // add built-in libraries as the dependencies of a pivot-lang project
        config.deps = Some(deps);
    } else {
        // rawdeps are the dependencies specified in the kagari.toml file which excludes the built-in dependencies
        let mut rawdeps = config.deps.clone().unwrap();

        let pb = &COMPILE_PROGRESS;
        match pb.length() {
            None => pb.set_length(rawdeps.len() as u64),
            _ => pb.inc_length(rawdeps.len() as u64),
        }
        pb.set_message("正在分析依赖");

        // load each dependecy through git clone, validate its checksum and load it into the deps
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
                            if let Some(child) = child {
                                child.unwrap();
                            }
                            if let Some(sum) = sums.get(k.as_str()) {
                                b = sum.git.clone().unwrap().commit;
                            } else {
                                sum_changed = true;
                            }
                            let target = kagari::cp_to_hash_dir(target.to_str().unwrap(), &b);
                            sums.insert(
                                k.to_string(),
                                ModSum {
                                    name: k.to_string(),
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
                            let dep = Dependency {
                                path: target.to_string_lossy().to_string(),
                                ..Default::default()
                            };
                            deps.insert(*k, dep);
                        })
                })
                .or_else(|| {
                    // pb.set_prefix(format!("[{:3}/{:3}]", pb.position(), pb.length().unwrap()));
                    i += 1;
                    pb.set_message(format!("正在分析依赖{}", k));
                    if PathBuf::from(&v.path).is_absolute() {
                        _ = crate::utils::canonicalize(&v.path)
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
                        _ = crate::utils::canonicalize(config_root.join(&v.path))
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
                    deps.insert(*k, v.clone());
                    None
                });
            pb.inc(1);
        });

        if let Some(err) = err {
            return Err(err);
        }
        config.deps = Some(deps);
    }

    if sum_changed {
        toml::to_string_pretty(&sums)
            .map_err(|e| format!("error: {:?}", e))
            .and_then(|s| std::fs::write(lockfile, s).map_err(|e| format!("error: {:?}", e)))?;
    }

    config.root = crate::utils::canonicalize(config_root.clone())
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();
    config.entry = crate::utils::canonicalize(config_root.join(&config.entry))
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();
    Ok(config)
}

pub fn enter() -> &'static str {
    "\n"
}
