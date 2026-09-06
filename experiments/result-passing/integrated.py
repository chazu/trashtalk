#!/usr/bin/env python3
"""Measure actual compiler/runtime opt-in against default behavior in disposable repos."""
import argparse
import csv
import hashlib
import json
import os
from pathlib import Path
import random
import shutil
import statistics
import subprocess
import tempfile

from run import ROOT, copy_repo, environment, command

WORKLOADS = {'constant': 24, 'nested': 12, 'numeric': 24, 'getter': 8,
             'class_map': 3, 'block_map': 3, 'browser': 1}
FIXTURE = '''
  classMethod: assignedConstant [
    | result |
    result := @ ResultProbe constant.
    ^ result
  ]
  classMethod: assignedNested [
    | result |
    result := @ ResultProbe nested.
    ^ result
  ]
  classMethod: assignedGetter: receiver [
    | result |
    result := @ receiver getNumber.
    ^ result
  ]
  classMethod: assignedMap: receiver callback: callback [
    | result |
    result := @ receiver collect: callback.
    ^ result
  ]
  classMethod: assignedBrowser [
    | result |
    result := @ Trash instanceRecordsFor: Counter.
    ^ result
  ]
'''
HARNESS = r'''
set -uo pipefail
source "$TRASHTALK_DIR/lib/trash.bash" || exit 90
trap '_env_cleanup_on_exit || true' EXIT
for name in ResultProbe Object Counter Array Block Trash String Runtime; do
  _ensure_class_sourced "$name" || exit 91
done
(_create_instance ResultProbe resultprobe_fixture) || exit 92
(_create_instance Array array_fixture) || exit 93
@ array_fixture setItems: '["0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"]' || exit 94
for ((i=0;i<10;i++)); do (_create_instance Counter "counter_fixture$i") || exit 95; done
block=$(@ Block params: '["x"]' code: 'printf "%s\n" "$x"' captured: '{}') || exit 96
printf 'READY\n'
while read -r name loops round; do
  [[ $name != stop ]] || break
  start=${EPOCHREALTIME/./}
  status=0
  for ((i=0;i<loops;i++)); do
    case $name in
      constant) __ResultProbe__class__assignedConstant ;;
      nested) __ResultProbe__class__assignedNested ;;
      numeric) __ResultProbe__class__assignedAdd ;;
      getter) __ResultProbe__class__assignedGetter_ resultprobe_fixture ;;
      class_map) __ResultProbe__class__assignedMap_callback_ array_fixture ResultProbe ;;
      block_map) __ResultProbe__class__assignedMap_callback_ array_fixture "$block" ;;
      browser) __ResultProbe__class__assignedBrowser ;;
    esac || { status=$?; break; }
  done > "$TMPDIR/output" 2> "$TMPDIR/error"
  elapsed=$((${EPOCHREALTIME/./}-start))
  [[ $status == 0 && ! -s "$TMPDIR/error" ]] || { cat "$TMPDIR/error" >&2; exit 97; }
  result=$(tail -1 "$TMPDIR/output")
  case $name in
    constant|nested) [[ $result == 'plain result' ]] || exit 98 ;;
    numeric) [[ $result == 11 ]] || exit 98 ;;
    getter) [[ $result == 7 ]] || exit 98 ;;
    class_map|block_map) data=$(_env_get "$result"); jq -e '.items==[range(0;25)|tostring]' <<< "$data" >/dev/null || exit 98 ;;
    browser) jq -se 'length==10 and all(.[]; .class_name=="Counter" and .data.value==0)' "$TMPDIR/output" >/dev/null || exit 98 ;;
  esac
  printf '%s\n' "$elapsed"
done
'''


def summarize(samples):
    output = {}
    rng = random.Random(20260906)
    for name in WORKLOADS:
        groups = {mode: [s['microseconds']/s['loops'] for s in samples
                         if s['case'] == name and s['mode'] == mode] for mode in ('off', 'on')}
        improvements = [100*(off-on)/off for off, on in zip(groups['off'], groups['on'])]
        bootstrap = sorted(statistics.median(rng.choices(improvements, k=len(improvements))) for _ in range(5000))
        output[name] = {'median_us': {mode: statistics.median(rows) for mode, rows in groups.items()},
                        'p95_batch_mean_us': {mode: sorted(rows)[int((len(rows)-1)*.95)] for mode, rows in groups.items()},
                        'paired_median_improvement_pct': statistics.median(improvements),
                        'paired_bootstrap_95_pct': [bootstrap[125], bootstrap[4874]]}
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rounds', type=int, default=24)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    if args.rounds < 2:
        parser.error('--rounds must be at least 2')
    samples, workers = [], {}
    report = {'kind': 'integrated guarded A', 'rounds': args.rounds,
              'base_commit': command(['git', 'rev-parse', 'HEAD'], cwd=ROOT).stdout.strip(),
              'bash': command(['bash', '--version']).stdout.splitlines()[0],
              'host': os.uname()._asdict() if hasattr(os.uname(), '_asdict') else list(os.uname()),
              'load_start': os.getloadavg(),
              'source_sha256': {p: hashlib.sha256((ROOT/p).read_bytes()).hexdigest() for p in
                  ['lib/trash.bash', 'lib/trash-json.bash', 'lib/jq-compiler/codegen.jq',
                   'lib/jq-compiler/build-cache.bash', 'lib/jq-compiler/build-plan.jq',
                   'tests/fixtures/ResultProbe.trash', 'experiments/result-passing/integrated.py']}}
    with tempfile.TemporaryDirectory(prefix='trash-integrated-') as temporary:
        work = Path(temporary)
        try:
            for mode in ('off', 'on'):
                repo = work/mode
                copy_repo(ROOT, repo)
                # Reuse content-addressed parse inputs, never writable shared
                # state. The compiler still validates source/fingerprint keys.
                cache = ROOT/'trash/.compiled/.astcache'
                if cache.is_dir():
                    shutil.copytree(cache, repo/'trash/.compiled/.astcache')
                (repo/'trash/ResultProbe.trash').write_text((ROOT/'tests/fixtures/ResultProbe.trash').read_text()+FIXTURE)
                env = dict(environment(repo, work, work/(mode+'-state')), TRASHTALK_VALUE_SEND=str(int(mode == 'on')))
                print(f'Building {mode}', flush=True)
                command(['make', 'bash'], cwd=repo, env=env)
                errors = (work/(mode+'.stderr')).open('w+')
                process = subprocess.Popen(['bash', '-c', HARNESS], cwd=repo, env=env,
                                           stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=errors, text=True)
                workers[mode] = (process, errors)
                ready = process.stdout.readline().strip()
                if ready != 'READY':
                    errors.seek(0)
                    raise RuntimeError(f'{mode} failed to start: {ready}\n{errors.read()}')
            def measure(mode, name, loops, round_number):
                process, errors = workers[mode]
                process.stdin.write(f'{name} {loops} {round_number}\n')
                process.stdin.flush()
                line = process.stdout.readline().strip()
                if not line.isdigit():
                    errors.seek(0)
                    raise RuntimeError(f'{mode}/{name} invalid: {line}\n{errors.read()}')
                return {'mode': mode, 'case': name, 'round': round_number, 'loops': loops, 'microseconds': int(line)}
            for name, loops in WORKLOADS.items():
                for mode in workers:
                    measure(mode, name, loops, -1)
            for round_number in range(args.rounds):
                for name, loops in WORKLOADS.items():
                    for mode in (('off', 'on') if round_number % 2 == 0 else ('on', 'off')):
                        samples.append(measure(mode, name, loops, round_number))
                print(f'Round {round_number+1}/{args.rounds}', flush=True)
            for process, errors in workers.values():
                process.stdin.write('stop 0 0\n')
                process.stdin.flush()
                if process.wait(timeout=10) != 0:
                    raise RuntimeError('benchmark worker failed at shutdown')
        finally:
            for process, errors in workers.values():
                if process.poll() is None:
                    process.terminate()
                    process.wait(timeout=10)
                errors.close()
    report.update(load_end=os.getloadavg(), results=summarize(samples))
    args.output.parent.mkdir(parents=True, exist_ok=True)
    csv_path = args.output.with_suffix('.samples.csv')
    with csv_path.open('w') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(samples[0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(samples)
    report['samples_file'] = csv_path.name
    report['samples_sha256'] = hashlib.sha256(csv_path.read_bytes()).hexdigest()
    args.output.write_text(json.dumps(report, indent=2)+'\n')
    print(json.dumps(report['results'], indent=2))


if __name__ == '__main__':
    main()
