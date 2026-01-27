use std::log
use std::IO
use std::lib::Regex

log = std::log
IO = std::IO
Regex = std::lib::Regex

path = "tests/tmp_log.txt"
rotated_path = "tests/tmp_log.txt.1"

log.set(path, "truncate")
log.format("{message}")
log.flush(true)
log "one"

log.format("ts:{timestamp} msg:{message}")
log "two"

content = IO.read(path)
pattern = "^one\\nts:[0-9]+\\.[0-9]{3} msg:two\\n$"
if !Regex.is_match(pattern, content)
  error "log format mismatch: " + content
end

IO.write(path, "old")
log.set(path, "rotate", 1)
log.format("{message}")
log "new"

if !IO.exists(rotated_path)
  error "log rotation failed: missing rotated file"
end

rotated = IO.read(rotated_path)
if rotated != "old"
  error "log rotation content mismatch"
end

IO.remove(path)
IO.remove(rotated_path)
