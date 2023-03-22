#[macro_export]
macro_rules! if_not_modified_by {
    ($entity:expr, $modifier:expr, $logic:stmt) => {
        if let Some((tt, _)) = $entity {
            if tt != $modifier {
                $logic
            }
        } else {
            $logic
        }
    };
}

#[macro_export]
macro_rules! skip_if_not_modified_by {
    ($entity:expr, $modifier:expr) => {
        use $crate::if_not_modified_by;
        if_not_modified_by!($entity, $modifier, continue);
    };
}

#[macro_export]
macro_rules! add_basic_types {
    ($map:expr, $(
        $ident:ident
    ),+) => {
        $(
            paste::paste! {
                let [<pltype_$ident>] = PLType::PRIMITIVE(PriType::[<$ident:upper>]);
                $map
                .insert(stringify!($ident).to_string(), Arc::new(RefCell::new( [<pltype_$ident>])));
            }
        )*
    };
}

#[macro_export]
macro_rules! generic_impl {
    ($($args:ident),*) => (
        $(
            impl $args {
                pub fn need_gen_code(&self) -> bool {
                    if self.generic_map.is_empty() {
                        return false;
                    }
                    for (_, v) in self.generic_map.iter() {
                        match &*v.clone().borrow() {
                            PLType::GENERIC(g) => {
                                if g.curpltype.is_none() {
                                    return false;
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                    true
                }
                pub fn new_pltype(&self) -> $args {
                    let mut res = self.clone();
                    res.generic_map = res
                        .generic_map
                        .iter()
                        .map(|(k, pltype)| {
                            if let PLType::GENERIC(g) = &*pltype.borrow() {
                                return (k.clone(), Arc::new(RefCell::new(PLType::GENERIC(g.clone()))));
                            }
                            unreachable!()
                        })
                        .collect::<IndexMap<String, Arc<RefCell<PLType>>>>();
                    res.generic_map
                        .iter()
                        .for_each(|(_, v)| match &mut *v.clone().borrow_mut() {
                            PLType::GENERIC(g) => {
                                g.clear_type();
                            }
                            _ => unreachable!(),
                        });
                    res
                }
            }
        )*
    );
    ($($args:ident),*,) => (
        $crate::generic_impl!($($args),*)
    );
}

#[macro_export]
macro_rules! format_label {
    ($fmt:expr $(,$args:expr)*) => {
        Some(($fmt.into(), vec![
            $($args.into(),)*
        ]))
    };
    ($fmt:expr$(,$args:expr)*,) => {
        $crate::format_label!($fmt $(,$args)*)
    };
}
