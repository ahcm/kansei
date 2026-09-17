#!/usr/bin/env python3
"""Run release baselines in-process and optionally compare with a saved run."""
import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import platform
import statistics
import subprocess
import sys

ROOT = Path(__file__).resolve().parent.parent


def command(*args):
    return subprocess.check_output(args, cwd=ROOT, text=True).strip()


def metadata():
    cpu = platform.processor()
    cpuinfo = Path('/proc/cpuinfo')
    if cpuinfo.exists():
        cpu = next((line.split(':', 1)[1].strip() for line in cpuinfo.read_text().splitlines()
                    if line.startswith('model name')), cpu)
    return {
        'revision': command('git', 'rev-parse', 'HEAD'),
        'dirty': bool(command('git', 'status', '--porcelain')),
        'recorded_at': datetime.now(timezone.utc).isoformat(),
        'cpu': cpu, 'architecture': platform.machine(), 'platform': platform.platform(),
        'rustc': command('rustc', '--version'), 'features': 'no-default-features',
        'rustflags': os.environ.get('RUSTFLAGS', ''),
        'encoded_rustflags': os.environ.get('CARGO_ENCODED_RUSTFLAGS', ''),
        'harness_sha256': hashlib.sha256((ROOT / 'src/eval/benchmarks.rs').read_bytes()).hexdigest(),
        'lockfile_sha256': hashlib.sha256((ROOT / 'Cargo.lock').read_bytes()).hexdigest(),
    }


def compare(current, baseline, limit):
    if baseline.get('schema') != current['schema']:
        raise ValueError('Baseline schema differs; record a new baseline.')
    for key in ('cpu', 'architecture', 'rustc', 'features', 'rustflags',
                'encoded_rustflags', 'harness_sha256'):
        if baseline['metadata'].get(key) != current['metadata'][key]:
            raise ValueError(f'Baseline {key} differs; compare runs on the same machine/toolchain/harness.')
    before = {item['name']: item for item in baseline['benchmarks']}
    after = {item['name']: item for item in current['benchmarks']}
    if before.keys() != after.keys():
        raise ValueError('Benchmark cases differ; record a new baseline.')
    failed = False
    for name, result in after.items():
        change = (result['median_ns'] / before[name]['median_ns'] - 1) * 100
        print(f'{name:38s} {change:+7.1f}%', file=sys.stderr)
        failed |= limit is not None and change > limit
    return failed


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, help='save JSON results (default: stdout)')
    parser.add_argument('--baseline', type=Path, help='compare against saved JSON on the same machine')
    parser.add_argument('--max-regression', type=float, help='fail if any median slows by more than this percent')
    parser.add_argument('--jobs', type=int, default=2, help='Cargo build parallelism (default: 2)')
    args = parser.parse_args()
    if args.max_regression is not None and (args.baseline is None or args.max_regression < 0):
        parser.error('--max-regression requires --baseline and a nonnegative percentage')
    if args.jobs < 1:
        parser.error('--jobs must be positive')
    # Read first: --output may intentionally replace the baseline after comparison.
    baseline = json.loads(args.baseline.read_text()) if args.baseline else None
    info = metadata()
    result = subprocess.run([
        'cargo', 'test', '--release', '--locked', '--no-default-features', '--lib',
        '-j', str(args.jobs), 'performance_baselines', '--', '--ignored', '--nocapture', '--test-threads=1',
    ], cwd=ROOT, stdout=subprocess.PIPE, text=True)
    if result.returncode:
        print(result.stdout, file=sys.stderr)
        return result.returncode
    cases = [json.loads(line.split('KANSEI_BENCH ', 1)[1]) for line in result.stdout.splitlines()
             if 'KANSEI_BENCH ' in line]
    if not cases:
        raise ValueError('No benchmark results were emitted.')
    for case in cases:
        # Verify the output before saving a baseline that future runs will trust.
        if case['median_ns'] <= 0 or statistics.median(case['samples_ns']) != case['median_ns']:
            raise ValueError(f'Invalid samples for {case["name"]}')
        print(f'{case["name"]:38s} {case["median_ns"]:12.1f} ns/op', file=sys.stderr)
    report = {'schema': 1, 'metadata': info, 'benchmarks': cases}
    failed = compare(report, baseline, args.max_regression) if baseline else False
    encoded = json.dumps(report, indent=2) + '\n'
    if args.output:
        args.output.write_text(encoded)
    else:
        print(encoded, end='')
    return int(failed)


if __name__ == '__main__':
    try:
        sys.exit(main())
    except (OSError, ValueError, KeyError, subprocess.CalledProcessError) as error:
        print(f'benchmark: {error}', file=sys.stderr)
        sys.exit(1)
