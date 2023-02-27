#[macro_export]
macro_rules! skip_if_not_modified_by {
    ($entity:expr,$modifier:expr) => {
        if let Some((t, _)) = $entity {
            if t != $modifier {
                continue;
            }
        } else {
            continue;
        }
    };
}
