use std::fs;
use std::path::{Path, PathBuf};

fn default_modules_dir() -> Option<PathBuf>
{
    directories::ProjectDirs::from("com", "ahcm", "kansei").map(|proj_dirs| {
        let data_dir = proj_dirs.data_dir();
        data_dir.join("modules")
    })
}

fn collect_ks_files(path: &Path, out: &mut Vec<PathBuf>)
{
    if path.is_dir()
    {
        if let Ok(entries) = fs::read_dir(path)
        {
            for entry in entries.flatten()
            {
                collect_ks_files(&entry.path(), out);
            }
        }
    }
    else if path.extension().and_then(|s| s.to_str()) == Some("ks")
    {
        out.push(path.to_path_buf());
    }
}

fn copy_ks_tree(src: &Path, dst: &Path) -> Result<(), String>
{
    let mut files = Vec::new();
    collect_ks_files(src, &mut files);
    for file in files
    {
        let rel = file.strip_prefix(src).map_err(|e| e.to_string())?;
        let target = dst.join(rel);
        if let Some(parent) = target.parent()
        {
            fs::create_dir_all(parent).map_err(|e| e.to_string())?;
        }
        fs::copy(&file, &target).map_err(|e| e.to_string())?;
    }
    Ok(())
}

fn install_from_path(dep_name: &str, dep_path: &Path, modules_dir: &Path) -> Result<(), String>
{
    let candidate = dep_path.join("modules");
    let source_root = if candidate.is_dir() { candidate } else { dep_path.to_path_buf() };
    if !source_root.exists()
    {
        return Err(format!(
            "install: source path '{}' not found",
            source_root.display()
        ));
    }
    let dest = modules_dir.join(dep_name);
    if dest.exists()
    {
        fs::remove_dir_all(&dest).map_err(|e| e.to_string())?;
    }
    fs::create_dir_all(&dest).map_err(|e| e.to_string())?;
    copy_ks_tree(&source_root, &dest)?;
    Ok(())
}

fn install_from_manifest(manifest_path: &Path, modules_dir: &Path) -> Result<(), String>
{
    let manifest = fs::read_to_string(manifest_path).map_err(|e| e.to_string())?;
    let value: toml::Value = toml::from_str(&manifest).map_err(|e| e.to_string())?;
    let deps = value
        .get("dependencies")
        .and_then(|v| v.as_table())
        .ok_or_else(|| "kansei.toml missing [dependencies]".to_string())?;

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
