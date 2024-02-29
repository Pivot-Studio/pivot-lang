use serde::Serialize;
use std::fmt;

/// Infomations gathered by the default setting of
/// [vergen::Config](https://docs.rs/vergen/latest/vergen/struct.Config.html)
#[derive(Debug, Clone, Serialize)]
pub struct VergenInfo {
    /// `VERGEN_BUILD_TIMESTAMP` e.g. `2021-02-25T23:28:39.493201+00:00`
    pub build_timestamp: String,
    /// `CARGO_PKG_VERSION` e.g. `5.0.0`
    pub build_semver: String,
    /// `VERGEN_RUSTC_CHANNEL` e.g. `nightly`
    pub rustc_channel: String,
    /// `VERGEN_RUSTC_COMMIT_DATE` e.g. `2021-02-24`
    pub rustc_commit_date: String,
    /// `VERGEN_RUSTC_COMMIT_HASH` e.g. `a8486b64b0c87dabd045453b6c81500015d122d6`
    pub rustc_commit_hash: String,
    /// `VERGEN_RUSTC_HOST_TRIPLE` e.g. `x86_64-apple-darwin`
    pub rustc_host_triple: String,
    /// `VERGEN_RUSTC_LLVM_VERSION` e.g. `11.0`
    pub rustc_llvm_version: String,
    /// `VERGEN_RUSTC_SEMVER` e.g. `1.52.0-nightly`
    pub rustc_semver: String,
    /// `VERGEN_CARGO_FEATURES` e.g. `git,build`
    pub cargo_features: String,
    /// `VERGEN_CARGO_OPT_LEVEL` e.g. `debug`
    pub cargo_profile: String,
    /// `VERGEN_CARGO_TARGET_TRIPLE` e.g. `x86_64-unknown-linux-gnu`
    pub cargo_target_triple: String,
    /// `VERGEN_GIT_BRANCH` e.g. `feature/fun`
    pub git_branch: String,
    /// `VERGEN_GIT_COMMIT_TIMESTAMP` e.g. `2021-02-24T20:55:21+00:00`
    pub git_commit_timestamp: String,
    /// `VERGEN_GIT_DESCRIBE` e.g. `5.0.0-2-gf49246c`
    pub git_semver: String,
    /// `VERGEN_GIT_SHA` e.g. `f49246ce334567bff9f950bfd0f3078184a2738a`
    pub git_sha: String,
}

impl fmt::Display for VergenInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Build Timestamp:     2021-02-23T20:14:46.558472672+00:00
        writeln!(f, "{:<20} {}", "Build Timestamp:", self.build_timestamp)?;
        // Build Version:       0.1.0-9-g46f83e1
        writeln!(f, "{:<20} {}", "Build Version:", self.build_semver)?;
        // Commit SHA:          46f83e112520533338245862d366f6a02cef07d4
        writeln!(f, "{:<20} {}", "Commit SHA:", self.git_sha)?;
        // Commit Date:         2021-02-23T08:08:02-05:00
        writeln!(f, "{:<20} {}", "Commit Date:", self.git_commit_timestamp)?;
        // Commit Branch:       master
        writeln!(f, "{:<20} {}", "Commit Branch:", self.git_branch)?;
        // rustc Version:       1.52.0-nightly
        writeln!(f, "{:<20} {}", "rustc Version:", self.rustc_semver)?;
        // rustc Channel:       nightly
        writeln!(f, "{:<20} {}", "rustc Channel:", self.rustc_channel)?;
        // rustc Host Triple:   x86_64-unknown-linux-gnu
        writeln!(f, "{:<20} {}", "rustc Host Triple:", self.rustc_host_triple)?;
        // rustc Commit SHA:    3f5aee2d5241139d808f4fdece0026603489afd1
        writeln!(f, "{:<20} {}", "rustc Commit SHA:", self.rustc_commit_hash)?;
        // cargo Target Triple: x86_64-unknown-linux-musl
        writeln!(
            f,
            "{:<20} {}",
            "cargo Target Triple:", self.cargo_target_triple
        )?;
        // cargo Profile:       release
        writeln!(f, "{:<20} {}", "cargo Profile:", self.cargo_profile)?;
        Ok(())
    }
}

impl VergenInfo {
    /// ### new
    ///
    /// it reads and stores the building information rendered by vergen through environment variables
    pub fn new() -> Self {
        Self {
            build_semver: env!("CARGO_PKG_VERSION").to_string(),
            build_timestamp: env!("VERGEN_BUILD_TIMESTAMP").to_string(),
            rustc_channel: env!("VERGEN_RUSTC_CHANNEL").to_string(),
            rustc_commit_date: env!("VERGEN_RUSTC_COMMIT_DATE").to_string(),
            rustc_commit_hash: env!("VERGEN_RUSTC_COMMIT_HASH").to_string(),
            rustc_host_triple: env!("VERGEN_RUSTC_HOST_TRIPLE").to_string(),
            rustc_llvm_version: env!("VERGEN_RUSTC_LLVM_VERSION").to_string(),
            rustc_semver: env!("VERGEN_RUSTC_SEMVER").to_string(),
            cargo_features: env!("VERGEN_CARGO_FEATURES").to_string(),
            cargo_profile: env!("VERGEN_CARGO_OPT_LEVEL").to_string(),
            cargo_target_triple: env!("VERGEN_CARGO_TARGET_TRIPLE").to_string(),
            git_branch: env!("VERGEN_GIT_BRANCH").to_string(),
            git_commit_timestamp: env!("VERGEN_GIT_COMMIT_TIMESTAMP").to_string(),
            git_semver: env!("VERGEN_GIT_DESCRIBE").to_string(),
            git_sha: env!("VERGEN_GIT_SHA").to_string(),
        }
    }
}
