#[macro_export]
macro_rules! if_not_modified_by {
    ($entity:expr,$modifier:expr, $logic:stmt) => {
        if let Some((t, _)) = $entity {
            if t != $modifier {
                $logic
            }
        } else {
            $logic
        }
    };
}

#[macro_export]
macro_rules! skip_if_not_modified_by {
    ($entity:expr,$modifier:expr) => {
        use crate::if_not_modified_by;
        if_not_modified_by!($entity, $modifier, continue);
    };
}
