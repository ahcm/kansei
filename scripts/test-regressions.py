#!/usr/bin/env python3
"""Exercise the CLI, formatter, LSP, and test runner with a built Kansei binary."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

BINARY = str(Path(sys.argv.pop(1)).resolve())


def run(*args, source=None, timeout=15):
    return subprocess.run([BINARY, *map(str, args)], input=source, text=True,
                          capture_output=True, timeout=timeout)


def frame(message):
    body = json.dumps(message).encode()
    return b"Content-Length: " + str(len(body)).encode() + b"\r\n\r\n" + body


def messages(data):
    result = []
    while data:
        header, data = data.split(b"\r\n\r\n", 1)
        length = int(header.split(b":", 1)[1])
        result.append(json.loads(data[:length]))
        data = data[length:]
    return result


class Regressions(unittest.TestCase):
    def test_execution_modes(self):
        result = run("test", "--bytecode", "all", "tests/regression", "tests/test_currying.ks", "tests/test_logic.ks", "tests/test_casts.ks", "tests/test_wasm_spec.ks")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("0 failed", result.stdout)

    def test_github_wat_command_is_preserved(self):
        for target in ("wasip1", "wasip2"):
            result = run("--dump-wat", "--wasi", target, "-e", "puts 1 + 2")
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertTrue(result.stdout.startswith("(module"))
            self.assertIn("wasi=" + target, result.stdout)

    def test_syntax_errors_do_not_abort(self):
        for source in ['fn broken(', '$', 'end puts "ignored"', '"unterminated']:
            result = run("-e", source)
            self.assertEqual(result.returncode, 1, result.stderr)
            self.assertIn("Syntax Error", result.stderr)
            self.assertNotIn("panicked", result.stderr)

    def test_lsp_recovers_after_invalid_document(self):
        uri = "file:///regression.ks"
        requests = [
            {"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {}},
            {"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {
                "textDocument": {"uri": uri, "text": "fn broken(", "version": 1}}},
            {"jsonrpc": "2.0", "method": "textDocument/didChange", "params": {
                "textDocument": {"uri": uri, "version": 2},
                "contentChanges": [{"text": "puts 1"}]}},
            {"jsonrpc": "2.0", "id": 2, "method": "shutdown"},
            {"jsonrpc": "2.0", "method": "exit"},
        ]
        result = subprocess.run([BINARY, "lsp"], input=b"".join(map(frame, requests)),
                                capture_output=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stderr)
        responses = messages(result.stdout)
        diagnostics = [m["params"]["diagnostics"] for m in responses
                       if m.get("method") == "textDocument/publishDiagnostics"]
        self.assertEqual(len(diagnostics), 2)
        self.assertTrue(diagnostics[0])
        self.assertEqual(diagnostics[1], [])
        self.assertTrue(any(m.get("id") == 2 for m in responses))

    def test_formatter_preserves_behavior_and_comments(self):
        source = '# heading\nfn demo()\nputs "first\n  second" # keep me\nend\ndemo()\n'
        formatted = run("fmt", "--stdin", source=source)
        self.assertEqual(formatted.returncode, 0, formatted.stderr)
        self.assertIn("# keep me", formatted.stdout)
        self.assertEqual(run("fmt", "--stdin", source=formatted.stdout).stdout, formatted.stdout)
        for mode in ("off", "simple", "advanced"):
            before = run("--bytecode", mode, "-e", source)
            after = run("--bytecode", mode, "-e", formatted.stdout)
            self.assertEqual(before.returncode, 0, before.stderr)
            self.assertEqual((before.returncode, before.stdout, before.stderr),
                             (after.returncode, after.stdout, after.stderr))

    def test_formatter_leaves_invalid_file_untouched(self):
        with tempfile.TemporaryDirectory() as temp:
            path = Path(temp) / "invalid.ks"
            path.write_text("end important content")
            self.assertEqual(run("fmt", path).returncode, 1)
            self.assertEqual(path.read_text(), "end important content")

    def test_runner_rejects_mismatch_missing_paths_and_timeout(self):
        with tempfile.TemporaryDirectory() as temp:
            path = Path(temp) / "case.ks"
            path.write_text('puts "actual"')
            path.with_suffix(".out").write_text("wrong\n")
            result = run("test", "--bytecode", "off", path)
            self.assertEqual(result.returncode, 1)
            self.assertIn("mismatch", result.stderr)
            path.with_suffix(".out").unlink()
            path.write_text("while true\n1\nend")
            result = run("test", "--bytecode", "off", "--timeout", "1", path, timeout=5)
            self.assertEqual(result.returncode, 1)
            self.assertIn("timed out", result.stderr)
            result = run("test", Path(temp) / "missing")
            self.assertEqual(result.returncode, 1)


if __name__ == "__main__":
    unittest.main()
