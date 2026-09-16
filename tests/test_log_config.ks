use std::log
use std::IO
use std::lib::Regex

logger = std::log
IO = std::IO
Regex = std::lib::Regex

path = "tests/tmp_log.txt"
rotated_path = "tests/tmp_log.txt.1"

logger.set(path, "truncate")
logger.format("{message}")
logger.flush(true)
log "one"

logger.format("ts:{timestamp} msg:{message}")
log "two"

content = IO.read(path)
pattern = "^one\\nts:[0-9]+\\.[0-9]{3} msg:two\\n$"
if not Regex.is_match(pattern, content)
  error "log format mismatch: " + content
end

IO.write(path, "old")
logger.set(path, "rotate", 1)
logger.format("{message}")
log "new"

if not IO.exists(rotated_path)
  error "log rotation failed: missing rotated file"
end

rotated = IO.read(rotated_path)
if rotated != "old"
  error "log rotation content mismatch"
end

logger.set(path, "truncate")
logger.format("{level}:{message}")
logger.level("warn")
logger.info("skip")
logger.warn("ok")

content2 = IO.read(path)
if content2 != "warn:ok\n"
  error "log level filter mismatch: " + content2
end

cfg = logger.get()
if cfg.level != "warn"
  error "logger.get level mismatch"
end

IO.remove(path)
IO.remove(rotated_path)
