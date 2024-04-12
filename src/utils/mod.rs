pub mod plc_new;
pub mod read_config;
pub mod test_symbol;

use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    io,
    path::{Path, PathBuf},
};

use lsp_types::Url;

pub fn get_hash_code<T>(obj: T) -> u64
where
    T: Hash,
{
    let mut hasher = DefaultHasher::new();
    obj.hash(&mut hasher);
    hasher.finish()
}

pub fn url_from_path(file: &str) -> Url {
    {
        if file.starts_with("@__repl") {
            return Url::parse("httss://example.com").unwrap();
        }
        #[cfg(any(unix, windows, target_os = "redox", target_os = "wasi"))]
        return Url::from_file_path(file).unwrap();
        #[cfg(not(any(unix, windows, target_os = "redox", target_os = "wasi")))]
        {
            if file.starts_with("http") {
                return Url::parse(file).unwrap();
            }
            return Url::parse("httss://example.com").unwrap();
        }
    }
}

pub fn canonicalize<P: AsRef<Path>>(path: P) -> io::Result<PathBuf> {
    #[cfg(target_arch = "wasm32")]
    return Ok(path.as_ref().to_path_buf());
    #[cfg(not(target_arch = "wasm32"))]
    {
        if path.as_ref().starts_with("@__repl__") {
            return Ok(path.as_ref().to_path_buf());
        }
        dunce::canonicalize(path)
    }
}
