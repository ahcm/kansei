#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp_dir="$(mktemp -d)"
log_file="${tmp_dir}/kansei.log"
stdout_file="${tmp_dir}/stdout.txt"
stderr_file="${tmp_dir}/stderr.txt"

trap 'rm -rf "${tmp_dir}"' EXIT

cd "${root_dir}"

cargo run -- -l "${log_file}" tests/test_logging.ks >"${stdout_file}" 2>"${stderr_file}"

expected_stdout="stdout:1"
expected_stderr="stderr:2"

if [[ "$(cat "${stdout_file}")" != "${expected_stdout}" ]]; then
  echo "stdout mismatch: $(cat "${stdout_file}")" >&2
  exit 1
fi

if [[ "$(cat "${stderr_file}")" != "${expected_stderr}" ]]; then
  echo "stderr mismatch: $(cat "${stderr_file}")" >&2
  exit 1
fi

expected_log=$'log line 1\nlog line 2'
if [[ "$(cat "${log_file}")" != "${expected_log}" ]]; then
  echo "log file mismatch: $(cat "${log_file}")" >&2
  exit 1
fi

cargo run -- tests/test_log_config.ks >/dev/null 2>&1

echo "logging tests ok"
