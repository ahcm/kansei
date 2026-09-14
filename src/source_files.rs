use std::fs;
use std::path::{Path, PathBuf};

/// Do not hide unreadable paths or follow directory symlinks into cycles.
pub fn collect(path: &Path, out: &mut Vec<PathBuf>) -> Result<(), String>
{
    let metadata = fs::symlink_metadata(path).map_err(|e| format!("{}: {e}", path.display()))?;
    if metadata.file_type().is_symlink() {
        return Err(format!("symbolic link is not supported: {}", path.display()));
    }
    if metadata.is_dir() {
        for entry in fs::read_dir(path).map_err(|e| format!("{}: {e}", path.display()))? {
            collect(&entry.map_err(|e| e.to_string())?.path(), out)?;
        }
    }
    else if metadata.is_file() && path.extension().and_then(|s| s.to_str()) == Some("ks") {
        out.push(path.to_path_buf());
    }
    Ok(())
}
