#[cfg(feature = "llvm")]
use crate::compiler::Options;
use console::Emoji;
use std::time::Duration;

use indicatif::{ProgressBar, ProgressDrawTarget, ProgressStyle};
use lazy_static::lazy_static;

lazy_static! {
    pub static ref COMPILE_PROGRESS: ProgressBar = ProgressBar::hidden();
    pub static ref CHECK_PROGRESS: ProgressBar = ProgressBar::hidden();
}

pub(crate) static CHECK: Emoji<'_, '_> = Emoji("🧐  ", ":-)");

pub(crate) static LOOKING_GLASS: Emoji<'_, '_> = Emoji("🔍  ", ":-)");

pub(crate) static TRUCK: Emoji<'_, '_> = Emoji("🚚  ", ":-)");

pub(crate) static CLIP: Emoji<'_, '_> = Emoji("🔗  ", ":-)");

pub(crate) static SPARKLE: Emoji<'_, '_> = Emoji("✨ ", ":-)");

lazy_static! {
    pub(crate) static ref PROGRESS_STYLE: ProgressStyle = ProgressStyle::with_template(
        "{prefix:.bold} {spinner} [{bar:40.cyan/blue}] {wide_msg:.green} ({elapsed})",
    )
    .unwrap();
    pub(crate) static ref MSG_PROGRESS_STYLE: ProgressStyle =
        ProgressStyle::with_template("{prefix:.bold} {spinner} {wide_msg:.green} ({elapsed})",)
            .unwrap();
}

#[cfg(feature = "llvm")]
pub fn prepare_progressbar(pb: &indicatif::ProgressBar, op: Options, prefix: String) {
    pb.enable_steady_tick(Duration::from_millis(50));
    pb.set_style(PROGRESS_STYLE.clone());

    let is_present_only = op.printast || op.flow;
    if is_present_only {
        pb.set_draw_target(ProgressDrawTarget::hidden());
    } else {
        pb.set_draw_target(ProgressDrawTarget::stderr());
    }
    pb.set_prefix(prefix);
}
