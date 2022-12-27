pub mod read_config;
pub mod test_symbol;

use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

pub fn get_hash_code<T>(obj: T) -> u64
where
    T: Hash,
{
    let mut hasher = DefaultHasher::new();
    obj.hash(&mut hasher);
    hasher.finish()
}
