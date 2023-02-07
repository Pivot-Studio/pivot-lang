use std::{
    path::{Path, PathBuf},
    process::Command,
};

pub fn download_repo(
    repo_url: &str,
    target_dir: &str,
    branch: &str,
) -> (Option<std::process::Child>, PathBuf) {
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
                .arg("--depth")
                .arg("1")
                .arg(repo_url)
                .arg(".")
                .arg("-b")
                .arg(branch)
                .arg("--single-branch")
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::null())
                .current_dir(target_dir.clone())
                .spawn()
                .expect("git clone failed"),
        ),
        target_dir,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_download_repo() {
        let repo_url = "https://github.com/voml/voml.git";
        let target_dir = "test_dir";
        let (child, ret_target_dir) = download_repo(repo_url, target_dir, "master");
        let status = child.unwrap().wait().unwrap();

        assert!(status.success());
        assert!(ret_target_dir.exists());
        let (child, ret_target_dir) = download_repo(repo_url, target_dir, "master");
        assert!(child.is_none());
        assert!(ret_target_dir.exists());
        fs::remove_dir_all(target_dir).unwrap();
    }
}
