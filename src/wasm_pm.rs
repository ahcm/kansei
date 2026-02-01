use directories::ProjectDirs;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const DEFAULT_WASM_MODULES_REPO: &str = "https://github.com/ahcm/kansei-wasm-modules";
const DEFAULT_WASM_MODULES_LOCAL: &str = "../kansei-wasm-modules";
const DEFAULT_WASM_TARGET: &str = "wasm32-wasip1";

pub fn run_wasm_command(args: &[String]) -> i32
{
    if args.is_empty()
    {
        eprintln!("wasm expects a subcommand (install).");
        return 2;
    }
    match args[0].as_str()
    {
        "install" => run_wasm_install(&args[1..]),
        _ =>
        {
            eprintln!("Unknown wasm subcommand: {}", args[0]);
            2
        }
    }
}

fn run_wasm_install(args: &[String]) -> i32
{
    let mut name: Option<String> = None;
    let mut repo_override: Option<String> = None;
    let mut target = DEFAULT_WASM_TARGET.to_string();
    let mut idx = 0;
    while idx < args.len()
    {
        match args[idx].as_str()
        {
            "--wasm-target" =>
            {
                if idx + 1 >= args.len()
                {
                    eprintln!("wasm install: --wasm-target expects a value");
                    return 2;
                }
                target = args[idx + 1].clone();
                idx += 2;
            }
            "--wasm-modules-repo" =>
            {
                if idx + 1 >= args.len()
                {
                    eprintln!("wasm install: --wasm-modules-repo expects a value");
                    return 2;
                }
                repo_override = Some(args[idx + 1].clone());
                idx += 2;
            }
            arg =>
            {
                if name.is_none() && !arg.starts_with('-')
                {
                    name = Some(arg.to_string());
                    idx += 1;
                }
                else
                {
                    eprintln!("wasm install: unexpected argument '{arg}'");
                    return 2;
                }
            }
        }
    }

    let name = match name
    {
        Some(val) => val,
        None =>
        {
            eprintln!("wasm install: missing module name");
            return 2;
        }
    };

    let repo = match resolve_repo(repo_override.as_deref())
    {
        Ok(path) => path,
        Err(err) =>
        {
            eprintln!("wasm install: {err}");
            return 1;
        }
    };

    if let Err(err) = build_module(&repo, &name, &target)
    {
        eprintln!("wasm install: {err}");
        return 1;
    }

    if let Err(err) = install_wasm(&repo, &name, &target)
    {
        eprintln!("wasm install: {err}");
        return 1;
    }

    println!("Installed wasm module '{name}'.");
    0
}

fn resolve_repo(override_repo: Option<&str>) -> Result<PathBuf, String>
{
    let spec = match override_repo
    {
        Some(val) => val.to_string(),
        None =>
        {
            let local = PathBuf::from(DEFAULT_WASM_MODULES_LOCAL);
            if local.exists()
            {
                return Ok(local);
            }
            DEFAULT_WASM_MODULES_REPO.to_string()
        }
    };

    let candidate = PathBuf::from(&spec);
    if candidate.exists()
    {
        return Ok(candidate);
    }

    let cache_root = cache_root_dir()?;
    fs::create_dir_all(&cache_root).map_err(|e| e.to_string())?;
    let repo_name = repo_name_from_spec(&spec);
    let repo_dir = cache_root.join(repo_name);
    if repo_dir.exists()
    {
        return Ok(repo_dir);
    }

    let status = Command::new("git")
        .arg("clone")
        .arg(&spec)
        .arg(&repo_dir)
        .status()
        .map_err(|e| format!("failed to run git: {e}"))?;
    if !status.success()
    {
        return Err(format!("git clone failed for '{spec}'"));
    }
    Ok(repo_dir)
}

fn repo_name_from_spec(spec: &str) -> String
{
    let trimmed = spec.trim_end_matches('/');
    let mut name = trimmed;
    if let Some(pos) = trimmed.rfind('/')
    {
        name = &trimmed[pos + 1..];
    }
    else if let Some(pos) = trimmed.rfind(':')
    {
        name = &trimmed[pos + 1..];
    }
    name.trim_end_matches(".git").to_string()
}

fn cache_root_dir() -> Result<PathBuf, String>
{
    let proj_dirs = ProjectDirs::from("com", "ahcm", "kansei")
        .ok_or_else(|| "could not resolve user data directory".to_string())?;
    Ok(proj_dirs.data_dir().join("wasm-modules"))
}

fn build_module(repo_dir: &Path, name: &str, target: &str) -> Result<(), String>
{
    let status = Command::new("cargo")
        .arg("build")
        .arg("--target")
        .arg(target)
        .arg("--release")
        .arg("-p")
        .arg(name)
        .current_dir(repo_dir)
        .status()
        .map_err(|e| format!("failed to run cargo: {e}"))?;
    if !status.success()
    {
        return Err("cargo build failed".to_string());
    }
    Ok(())
}

fn install_wasm(repo_dir: &Path, name: &str, target: &str) -> Result<(), String>
{
    let artifact = repo_dir
        .join("target")
        .join(target)
        .join("release")
        .join(format!("{name}.wasm"));
    if !artifact.exists()
    {
        return Err(format!(
            "expected wasm artifact not found: {}",
            artifact.display()
        ));
    }

    let proj_dirs = ProjectDirs::from("com", "ahcm", "kansei")
        .ok_or_else(|| "could not resolve user data directory".to_string())?;
    let wasm_dir = proj_dirs.data_dir().join("wasm");
    fs::create_dir_all(&wasm_dir).map_err(|e| e.to_string())?;
    let dest = wasm_dir.join(format!("{name}.wasm"));
    fs::copy(&artifact, &dest).map_err(|e| e.to_string())?;
    Ok(())
}
