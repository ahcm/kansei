use std::fs;
use std::io::{Read, Seek, SeekFrom};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

const OUTPUT_LIMIT: u64 = 1024 * 1024;

fn captured(file: &mut fs::File) -> Result<String, String>
{
    if file.metadata().map_err(|e| e.to_string())?.len() > OUTPUT_LIMIT {
        return Err("output exceeded 1 MiB".to_string());
    }
    file.seek(SeekFrom::Start(0)).map_err(|e| e.to_string())?;
    let mut text = String::new();
    file.take(OUTPUT_LIMIT + 1).read_to_string(&mut text).map_err(|e| e.to_string())?;
    Ok(text)
}

fn run_one(exe: &Path, path: &Path, mode: &str, timeout: Duration) -> Result<(), String>
{
    let expected_status = match fs::read_to_string(path.with_extension("status")) {
        Ok(value) => value.trim().parse::<i32>().map_err(|_| "invalid .status file")?,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => 0,
        Err(e) => return Err(e.to_string()),
    };
    let mut stdout = tempfile::tempfile().map_err(|e| e.to_string())?;
    let mut stderr = tempfile::tempfile().map_err(|e| e.to_string())?;
    let mut command = Command::new(exe);
    command.args(["--bytecode", mode]).arg(path)
        .stdin(Stdio::null())
        .stdout(stdout.try_clone().map_err(|e| e.to_string())?)
        .stderr(stderr.try_clone().map_err(|e| e.to_string())?);
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        command.process_group(0);
    }
    let mut child = command.spawn().map_err(|e| e.to_string())?;
    let start = Instant::now();
    let status = loop {
        if let Some(status) = child.try_wait().map_err(|e| e.to_string())? { break status; }
        let too_large = stdout.metadata().map_err(|e| e.to_string())?.len() > OUTPUT_LIMIT
            || stderr.metadata().map_err(|e| e.to_string())?.len() > OUTPUT_LIMIT;
        if start.elapsed() >= timeout || too_large {
            #[cfg(unix)]
            // The child owns a new process group, so terminate its descendants too.
            unsafe { libc::kill(-(child.id() as i32), libc::SIGKILL); }
            let _ = child.kill();
            let _ = child.wait();
            return Err(if too_large { "output exceeded 1 MiB" } else { "timed out" }.to_string());
        }
        std::thread::sleep(Duration::from_millis(10));
    };
    let stdout = captured(&mut stdout)?;
    let stderr = captured(&mut stderr)?;
    if status.code() != Some(expected_status) {
        return Err(format!("expected exit {expected_status}, got {status}\n{stderr}"));
    }
    for (extension, actual) in [("out", &stdout), ("err", &stderr)] {
        match fs::read_to_string(path.with_extension(extension)) {
            Ok(expected) if &expected != actual => return Err(format!(
                "{extension} mismatch\nexpected: {expected:?}\nactual:   {actual:?}")),
            Ok(_) => {},
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {},
            Err(e) => return Err(e.to_string()),
        }
    }
    Ok(())
}

pub fn run_tests(args: &[String]) -> i32
{
    let mut mode = "all";
    let mut timeout = Duration::from_secs(30);
    let mut paths = Vec::new();
    let mut args = args.iter();
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--bytecode" => match args.next().map(String::as_str) {
                Some(value @ ("off" | "simple" | "advanced" | "all")) => mode = value,
                _ => { eprintln!("test: --bytecode expects off, simple, advanced, or all"); return 2; }
            },
            "--timeout" => match args.next().and_then(|v| v.parse::<u64>().ok()) {
                Some(seconds) if seconds > 0 => timeout = Duration::from_secs(seconds),
                _ => { eprintln!("test: --timeout expects positive seconds"); return 2; }
            },
            "--" => { paths.extend(args.map(PathBuf::from)); break; },
            value if value.starts_with('-') => { eprintln!("test: unknown option {value}"); return 2; },
            value => paths.push(PathBuf::from(value)),
        }
    }
    let mut files = Vec::new();
    for path in paths {
        if let Err(error) = crate::source_files::collect(&path, &mut files) {
            eprintln!("test: {error}"); return 1;
        }
    }
    files.sort();
    files.dedup();
    if files.is_empty() { eprintln!("test found no .ks files"); return 1; }
    let exe = match std::env::current_exe() {
        Ok(exe) => exe,
        Err(e) => { eprintln!("test: {e}"); return 1; }
    };
    let modes = if mode == "all" { vec!["off", "simple", "advanced"] } else { vec![mode] };
    let mut failures = 0;
    let mut count = 0;
    for file in files {
        for mode in &modes {
            count += 1;
            match run_one(&exe, &file, mode, timeout) {
                Ok(()) => println!("PASS [{}] {}", mode, file.display()),
                Err(error) => { failures += 1; eprintln!("FAIL [{}] {}: {error}", mode, file.display()); }
            }
        }
    }
    println!("{} passed; {} failed", count - failures, failures);
    i32::from(failures > 0)
}
