use console::Emoji;

use indicatif::{ProgressBar, ProgressStyle};
use lazy_static::lazy_static;

lazy_static! {
    pub static ref COMPILE_PROGRESS: ProgressBar = { ProgressBar::hidden() };
}

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
