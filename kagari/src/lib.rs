use std::{
    path::{Path, PathBuf},
    process::{Command, Output},
};

pub fn download_repo(
    repo_url: &str,
    target_dir: &str,
) -> (Option<Result<Output, std::io::Error>>, PathBuf) {
    let branch = "default";
    let sub_dir = repo_url
        .strip_prefix("http://")
        .or_else(|| repo_url.strip_prefix("https://"))
        .expect("Invalid repo url")
        .trim_end_matches(".git");
    let target_dir = Path::new(target_dir).join(sub_dir).join(branch);
    if !target_dir.exists() {
        std::fs::create_dir_all(&target_dir).expect("Failed to create target directory");
    } else {
        return (None, target_dir);
    }
    (
        Some(
            Command::new("git")
                .arg("clone")
                // .arg("--depth")
                // .arg("1")
                .arg(repo_url)
                .arg(".")
                // .arg("-b")
                // .arg(branch)
                // .arg("--single-branch")
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::null())
                .current_dir(target_dir.clone())
                .spawn()
                .expect("git clone failed")
                .wait_with_output(),
        ),
        target_dir,
    )
}

pub fn cp_to_hash_dir(repo_dir: &str, head: &str) -> PathBuf {
    // checkout to head
    _ = Command::new("git")
        .current_dir(repo_dir.clone())
        .arg("checkout")
        .arg(head)
        .output()
        .unwrap();
    let re = Command::new("git")
        .current_dir(repo_dir.clone())
        .arg("rev-parse")
        .arg(head)
        .output()
        .unwrap();
    let hash = std::str::from_utf8(&re.stdout);
    let path = Path::new(repo_dir)
        .parent()
        .unwrap()
        .join(hash.unwrap().trim());
    if !path.exists() {
        std::fs::create_dir_all(&path).expect("Failed to create target directory");
        // copy repo_dir to hash_dir using std lib
        fs_extra::dir::copy(
            repo_dir,
            path.clone(),
            &fs_extra::dir::CopyOptions::new().content_only(true),
        )
        .unwrap();
    }
    path
}
