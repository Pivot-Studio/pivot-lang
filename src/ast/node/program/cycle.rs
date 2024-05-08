use crate::ast::accumulators::Diagnostics;
use crate::ast::diag::ErrorCode;
use crate::ast::node::NodeEnum;
use crate::ast::range::Range;
use crate::utils::read_config::ConfigWrapper;

use super::{ModWrapper, Program, ProgramNode};

use crate::ast::plmod::Mod;

use crate::format_label;

use rustc_hash::FxHashMap;
use salsa::AsId;
use ustr::ustr;

use crate::lsp::mem_docs::FileCompileInput;

use crate::Db;

pub fn cycle_deps_recover(
    db: &dyn Db,
    cycle: &salsa::Cycle,
    _: FileCompileInput,
) -> Option<ModWrapper> {
    let key = cycle.all_participants(db);
    let mut files = FxHashMap::default();
    let mut prev_use_map = FxHashMap::default();
    let params = cycle
        .participant_keys()
        .enumerate()
        .filter(|(i, _)| key[*i].starts_with("Program_emit"))
        .map(|(_, k)| Program::from_id(k.key_index()))
        .last()
        .unwrap();
    let src_file_path = params.params(db).file(db);
    let mut prev_file = src_file_path;
    build_init_params(params, db, &mut prev_use_map);
    let filtered = cycle
        .participant_keys()
        .enumerate()
        .filter(|(i, _)| key[*i].starts_with("Program_emit"));
    let len = filtered.count();
    while files.len() < len {
        for (_, p) in cycle
            .participant_keys()
            .enumerate()
            .filter(|(i, _)| key[*i].starts_with("Program_emit"))
            .map(|(u, k)| (u, Program::from_id(k.key_index())))
        {
            let prog = match_node(p, db);
            if let Some(r) = prev_use_map.get(p.params(db).file(db)) {
                files.insert(prev_file, *r);
                prev_file = p.params(db).file(db);
                prev_use_map.clear();
                build_use_map(prog, db, p, &mut prev_use_map);
            }
        }
    }

    let first_range = *files.get(&src_file_path).unwrap();
    let mut diag = first_range.new_err(ErrorCode::CYCLE_DEPENDENCY);
    diag.set_source(src_file_path);
    for (f, r) in files.iter() {
        let msg = "import in cycle here";
        diag.add_label(*r, ustr(f), format_label!(msg));
    }
    Diagnostics::push(db, (src_file_path.to_string(), vec![diag]));
    db.report_untracked_read();
    Some(ModWrapper::new(
        db,
        Mod::new(ustr(src_file_path), Default::default()),
    ))
}

pub(crate) fn build_init_params(
    params: Program,
    db: &dyn Db,
    prev_use_map: &mut FxHashMap<String, Range>,
) {
    let prog = match_node(params, db);
    build_use_map(prog, db, params, prev_use_map);
}

pub(crate) fn match_node(params: Program, db: &dyn Db) -> ProgramNode {
    match *params.entry_node(db).node(db) {
        NodeEnum::Program(p) => p,
        _ => panic!("not a program"),
    }
}

pub(crate) fn build_use_map(
    prog: ProgramNode,
    db: &dyn Db,
    params: Program,
    prev_use_map: &mut FxHashMap<String, Range>,
) {
    for u in prog.uses.iter() {
        let u = if let NodeEnum::UseNode(p) = *u.clone() {
            p
        } else {
            continue;
        };
        let range = u.range;
        let wrapper = ConfigWrapper::new(db, params.config(db), u);
        let path = wrapper.resolve_dep_path(db);
        prev_use_map.insert(path.to_str().unwrap().to_string(), range);
    }
}
