use std::fs;
use std::path::{Path, PathBuf};

fn default_modules_dir() -> Option<PathBuf>
{
    directories::ProjectDirs::from("com", "ahcm", "kansei").map(|proj_dirs| {
        let data_dir = proj_dirs.data_dir();
        data_dir.join("modules")
    })
}

fn collect_ks_files(path: &Path, out: &mut Vec<PathBuf>) -> Result<(), String>
{
    let metadata = fs::symlink_metadata(path).map_err(|e| format!("{}: {e}", path.display()))?;
    if metadata.file_type().is_symlink()
    {
        return Err(format!("install: symbolic links are not supported: {}", path.display()));
    }
    if metadata.is_dir()
    {
        for entry in fs::read_dir(path).map_err(|e| e.to_string())?
        {
            collect_ks_files(&entry.map_err(|e| e.to_string())?.path(), out)?;
        }
    }
    else if metadata.is_file() && path.extension().and_then(|s| s.to_str()) == Some("ks")
    {
        out.push(path.to_path_buf());
    }
    Ok(())
}

fn validate_module_name(name: &str) -> Result<(), String>
{
    if name.is_empty()
        || !name
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || b == b'_' || b == b'-')
    {
        return Err(format!(
            "install: invalid module name '{name}' (use letters, digits, '_' or '-')"
        ));
    }
    Ok(())
}

// Reserve a unique sibling directory atomically, including across processes.
fn staging_dir(root: &Path) -> Result<PathBuf, String>
{
    static NEXT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    loop
    {
        let id = NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let path = root.join(format!(".kansei-install-{}-{id}", std::process::id()));
        match fs::create_dir(&path)
        {
            Ok(()) => return Ok(path),
            Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(e) => return Err(e.to_string()),
        }
    }
}

fn install_from_path(dep_name: &str, dep_path: &Path, modules_dir: &Path) -> Result<(), String>
{
    validate_module_name(dep_name)?;
    let root = fs::canonicalize(modules_dir).map_err(|e| e.to_string())?;
    let candidate = dep_path.join("modules");
    let source = if candidate.is_dir()
    {
        candidate
    }
    else
    {
        dep_path.to_path_buf()
    };
    if !source.is_dir()
    {
        return Err(format!("install: source must be a directory: {}", source.display()));
    }
    let source = fs::canonicalize(source).map_err(|e| e.to_string())?;
    let dest = root.join(dep_name);
    if source.starts_with(&dest) || dest.starts_with(&source)
    {
        return Err("install: source and destination must not overlap".to_string());
    }
    if let Ok(metadata) = fs::symlink_metadata(&dest)
    {
        if metadata.file_type().is_symlink() || !metadata.is_dir()
        {
            return Err("install: destination must be a directory, not a symbolic link".to_string());
        }
    }
    let mut files = Vec::new();
    collect_ks_files(&source, &mut files)?;
    if files.is_empty()
    {
        return Err("install: source contains no .ks files".to_string());
    }
    let stage = staging_dir(&root)?;
    let replacement = stage.join("new");
    let backup = stage.join("old");
    let result = (|| {
        fs::create_dir(&replacement).map_err(|e| e.to_string())?;
        for file in files
        {
            let target = replacement.join(file.strip_prefix(&source).map_err(|e| e.to_string())?);
            fs::create_dir_all(target.parent().ok_or("install: missing parent")?)
                .map_err(|e| e.to_string())?;
            fs::copy(&file, &target).map_err(|e| e.to_string())?;
        }
        let had_previous = dest.exists();
        if had_previous
        {
            fs::rename(&dest, &backup).map_err(|e| e.to_string())?;
        }
        if let Err(error) = fs::rename(&replacement, &dest)
        {
            if had_previous
            {
                fs::rename(&backup, &dest).map_err(|rollback| format!(
                    "install: {error}; rollback failed: {rollback}; previous installation retained at {}", backup.display()))?;
            }
            return Err(error.to_string());
        }
        Ok(())
    })();
    // Never delete the backup if rollback failed.
    if result.is_ok() || !backup.exists()
    {
        fs::remove_dir_all(&stage).map_err(|e| format!("install: staging cleanup failed: {e}"))?;
    }
    result
}

fn install_from_manifest(manifest_path: &Path, modules_dir: &Path) -> Result<(), String>
{
    let manifest = fs::read_to_string(manifest_path).map_err(|e| e.to_string())?;
    let value: toml::Value = toml::from_str(&manifest).map_err(|e| e.to_string())?;
    let deps = value
        .get("dependencies")
        .and_then(|v| v.as_table())
        .ok_or_else(|| "kansei.toml missing [dependencies]".to_string())?;

    for name in deps.keys()
    {
        validate_module_name(name)?;
    }
    for (name, dep) in deps
    {
        let dep_path = if let Some(path) = dep.as_str()
        {
            PathBuf::from(path)
        }
        else if let Some(table) = dep.as_table()
        {
            let path = table
                .get("path")
                .and_then(|v| v.as_str())
                .ok_or_else(|| format!("dependency '{name}' missing path"))?;
            PathBuf::from(path)
        }
        else
        {
            return Err(format!("dependency '{name}' must be a path or table"));
        };
        let dep_path = manifest_path
            .parent()
            .unwrap_or(Path::new("."))
            .join(dep_path);
        install_from_path(name, &dep_path, modules_dir)?;
    }
    Ok(())
}

pub fn run_install(args: &[String]) -> i32
{
    let modules_dir = match default_modules_dir()
    {
        Some(dir) => dir,
        None =>
        {
            eprintln!("install: could not resolve user data directory");
            return 1;
        }
    };
    if let Err(e) = fs::create_dir_all(&modules_dir)
    {
        eprintln!("install: failed to create {}: {}", modules_dir.display(), e);
        return 1;
    }

    if args.is_empty()
    {
        let manifest_path = PathBuf::from("kansei.toml");
        if !manifest_path.exists()
        {
            eprintln!("install: kansei.toml not found");
            return 1;
        }
        if let Err(e) = install_from_manifest(&manifest_path, &modules_dir)
        {
            eprintln!("{e}");
            return 1;
        }
        return 0;
    }

    for dep in args
    {
        let path = PathBuf::from(dep);
        let name = path
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("module");
        if let Err(e) = install_from_path(name, &path, &modules_dir)
        {
            eprintln!("{e}");
            return 1;
        }
    }
    0
}

#[cfg(test)]
mod tests
{
    use super::*;

    struct Fixture(PathBuf);
    impl Fixture
    {
        fn new() -> Self
        {
            Self(staging_dir(&std::env::temp_dir()).unwrap())
        }
    }
    impl Drop for Fixture
    {
        fn drop(&mut self)
        {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn rejects_escaping_names_without_touching_files()
    {
        let fixture = Fixture::new();
        let root = fixture.0.join("modules");
        fs::create_dir(&root).unwrap();
        let source = fixture.0.join("source");
        fs::create_dir(&source).unwrap();
        fs::write(source.join("main.ks"), "puts 1").unwrap();
        for name in ["", ".", "..", "../source", "/tmp", "a/b", "a\\b"]
        {
            assert!(install_from_path(name, &source, &root).is_err());
        }
        assert_eq!(fs::read_to_string(source.join("main.ks")).unwrap(), "puts 1");
    }

    #[test]
    fn replaces_complete_tree_and_keeps_old_install_on_invalid_source()
    {
        let fixture = Fixture::new();
        let root = fixture.0.join("modules");
        fs::create_dir(&root).unwrap();
        let source = fixture.0.join("source");
        fs::create_dir(&source).unwrap();
        fs::write(source.join("old.ks"), "puts 1").unwrap();
        install_from_path("demo", &source, &root).unwrap();
        fs::remove_file(source.join("old.ks")).unwrap();
        assert!(install_from_path("demo", &source, &root).is_err());
        assert!(root.join("demo/old.ks").exists());
        fs::write(source.join("new.ks"), "puts 2").unwrap();
        install_from_path("demo", &source, &root).unwrap();
        assert!(!root.join("demo/old.ks").exists());
        assert!(root.join("demo/new.ks").exists());
        assert_eq!(fs::read_dir(&root).unwrap().count(), 1);
    }

    #[test]
    fn manifest_paths_are_relative_to_manifest()
    {
        let fixture = Fixture::new();
        let root = fixture.0.join("modules");
        fs::create_dir(&root).unwrap();
        fs::create_dir(fixture.0.join("source")).unwrap();
        fs::write(fixture.0.join("source/main.ks"), "puts 1").unwrap();
        let manifest = fixture.0.join("kansei.toml");
        fs::write(&manifest, "[dependencies]\ndemo = \"source\"\n").unwrap();
        install_from_manifest(&manifest, &root).unwrap();
        assert!(root.join("demo/main.ks").exists());
    }

    #[cfg(unix)]
    #[test]
    fn rejects_destination_symlinks()
    {
        let fixture = Fixture::new();
        let root = fixture.0.join("modules");
        fs::create_dir(&root).unwrap();
        let source = fixture.0.join("source");
        fs::create_dir(&source).unwrap();
        fs::write(source.join("main.ks"), "puts 1").unwrap();
        std::os::unix::fs::symlink(&source, root.join("demo")).unwrap();
        assert!(install_from_path("demo", &source, &root).is_err());
        assert!(source.join("main.ks").exists());
    }
}
