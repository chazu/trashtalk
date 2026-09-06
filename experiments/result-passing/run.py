#!/usr/bin/env python3
"""Disposable result-transport experiments; never installs a runtime variant."""
import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import random
import re
import shutil
import statistics
import subprocess
import tempfile
import time

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
MODES = ["C", "C_native", "A_all", "A_guard", "B_naive", "B_guard"]
CASES = {"constant": 40, "getter": 8, "arithmetic": 8, "nested": 15,
         "class_map": 1, "block_map": 1, "browser": 1}
ENV = dict(os.environ, LC_ALL="C", TRASHTALK_SKIP_USER_CONFIG="1",
           TRASHTALK_PROGRESS="0", TRASHTALK_LOG_LEVEL="error")
for key in ("TRASH_SESSION_ID", "TRASH_PROFILE", "TRASH_PROFILE_FILE", "_COMPILER_VERSION"):
    ENV.pop(key, None)


def command(args, **kwargs):
    return subprocess.run(args, check=True, text=True, capture_output=True,
                          env=kwargs.pop("env", ENV), **kwargs)


def copy_repo(source, target):
    target.mkdir()
    for name in ("lib", "trash", "bin", "axe"):
        shutil.copytree(source / name, target / name,
                        ignore=shutil.ignore_patterns("*.db*", ".astcache", ".symbolcache", ".buildcache", "user"))
    shutil.copy2(source / "Makefile", target / "Makefile")


def environment(repo, support, result):
    tmp = result / "tmp"
    tmp.mkdir(parents=True)
    return dict(ENV, TRASHTALK_DIR=str(repo), TRASHDIR=str(repo / "trash"),
                SQLITE_JSON_DB=str(result / "instances.db"), TMPDIR=str(tmp),
                EXP_SUPPORT=str(support), EXP_RESULTS=str(result))


def compile_fixture(repo, source, output):
    command(["bash", str(repo / "lib/jq-compiler/driver.bash"), "compile", str(source),
             "-o", str(output), "--check"], env=dict(ENV, TRASHTALK_DIR=str(repo)))


def function(source, name):
    start = source.index(f"function {name} {{")
    return source[start:source.index("\n}\n", start) + 2]


def generated_runtime(repo):
    runtime = (repo / "lib/trash.bash").read_text()
    entry = function(runtime, "@")
    split = entry.index("  # Capture output and store in $__")
    prefix, capture = entry[entry.index("\n") + 1:split], entry[split:-2]
    a = "_exp_value_send() {\n" + prefix + '''
  if _exp_eligible "$@"; then
    send "$@"
    return $?
  fi
''' + capture + "\n}\n"
    bprefix = prefix.replace('send "$@"', '_exp_capture_into "$__exp_destination" "$@"')
    bprefix = bprefix.replace('  _ensure_class_sourced "$___class"', '''
  if [[ -z "${_SOURCED_COMPILED_CLASSES[$___class]:-}" ]]; then
    _exp_capture_into "$__exp_destination" "$@"; return
  fi
  _ensure_class_sourced "$___class"''')
    b = '_exp_into_naive() {\n  local __exp_destination="$1"; shift\n' + bprefix + '''
  if _exp_eligible "$@"; then
    local __exp_value="" __exp_status
    local __="${__:-}"
    local -a _CALL_STACK=("${_CALL_STACK[@]}")
    _exp_send "$@"
    __exp_status=$?
    printf -v "$__exp_destination" '%s' "$__exp_value"
    return "$__exp_status"
  fi
  _exp_capture_into "$__exp_destination" "$@"
}
'''
    # Reuse receiver preparation in fallbacks while retaining BOTH original
    # captures (or the original single outer capture for direct methods).
    prepared = b.replace('_exp_into_naive()', '_exp_into()', 1)
    prepared = prepared.replace('_exp_capture_into "$__exp_destination" "$@"\n      return',
                                '_exp_capture_prepared_direct "$__exp_destination" "$@"\n      return')
    prepared = prepared.replace('_exp_capture_into "$__exp_destination" "$@"\n        return',
                                '_exp_capture_prepared_direct "$__exp_destination" "$@"\n        return')
    prepared = prepared.replace('  _exp_capture_into "$__exp_destination" "$@"\n}',
                                '  _exp_capture_prepared "$__exp_destination" "$@"\n}')
    tail = '_exp_public_tail() {\n' + capture + '\n}\n'
    dispatch = function(runtime, "send")
    trace = '\n  [[ -z "${EXP_TRACE:-}" ]] || printf "%s %s %s %s\\n" "$BASHPID" "$BASH_SUBSHELL" "$1" "$2" >> "$EXP_RESULTS/depth"'
    instrumented = dispatch.replace("function send {", "function send {" + trace, 1)
    bsend = instrumented.replace("function send {", "function _exp_send {", 1)
    for variable in ("class_method_func", "namespaced_func"):
        bsend = bsend.replace(f'"${variable}" "$@"', f'_exp_invoke "${variable}" "$@"')
    return instrumented + "\n" + a + b + prepared + tail + bsend + "\n"


def capabilities(repo, unchecked=False):
    # A bounded emitted subset, not a claim to have implemented effect analysis.
    selected = {
        "ResultProbe": ["class__constant", "class__valueWith_", "getNumber", "arithmetic"],
        "Counter": ["class__description", "getValue", "getStep"],
    }
    lines = []
    for owner, selectors in selected.items():
        source = (repo / "trash/.compiled" / owner).read_text()
        digest = re.search(rf'__{owner}__sourceHash="([^"]+)"', source)[1]
        for selector in selectors:
            name = f"__{owner}__{selector}"
            bodies = re.findall(rf'^{re.escape(name)}\(\) {{\n(.*?)\n}}', source, re.M | re.S)
            body = bodies[-1]
            assert body.count("echo ") == 1 and "@ " not in body, (name, body)
            assert all(line.strip().startswith(("local ", "echo ")) for line in body.splitlines()), (name, body)
            bbody = body.replace("echo ", "_exp_return ", 1)
            fields = list(dict.fromkeys(re.findall(r'\$\(_ivar ([a-zA-Z_][a-zA-Z_0-9]*)\)', body)))
            prelude = []
            if not unchecked:
                for index, field in enumerate(fields):
                    variable = f"__exp_number_{index}"
                    prelude += [f'  local {variable}', f'  {variable}=$(_ivar {field})',
                                f'  if ! _exp_is_integer "${variable}"; then _exp_scalar_fallback {name} "$@"; return $?; fi']
                    bbody = bbody.replace(f'$(_ivar {field})', f'${variable}')
            bbody = "\n".join(prelude + [bbody])
            lines += [f'_EXP_HASH[{name}]={digest}', f'_EXP_OWNER[{name}]=__{owner}',
                      f'_EXP_ARITY[{name}]={1 if selector == "class__valueWith_" else 0}',
                      f'_EXP_BFUNC[{name}]={name}__experiment', f'{name}__experiment() {{\n{bbody}\n}}']
    return "\n".join(lines) + "\n"


def lower_captures(repo, mode):
    # Experimental lowering of simple captured sends in emitted Bash and the
    # shared collection primitive. No public/bare @ call is rewritten.
    assignment = re.compile(r'([a-zA-Z_][a-zA-Z_0-9]*)="\$\(@ ([^()\n]*)\)"')
    unquoted_assignment = re.compile(r'([a-zA-Z_][a-zA-Z_0-9]*)=\$\(@ ([^()\n]*)\)')
    def replacement(match, quoted):
        name, args = match.groups()
        if mode.startswith("A_"):
            expression = f'$(_exp_value_send {args})'
            return f'{name}="{expression}"' if quoted else f'{name}={expression}'
        entry = '_exp_into_naive' if mode == 'B_naive' else '_exp_into' if mode == 'B_guard' else '_exp_assign'
        return f'{entry} {name} {args}'
    counts = {}
    for path in list((repo / "trash/.compiled").glob("*")) + [repo / "lib/trash-json.bash"]:
        if not path.is_file():
            continue
        text = path.read_text()
        lines, count = [], 0
        for line in text.splitlines(keepends=True):
            # Preserve declaration/assignment status policy in raw methods.
            if line.lstrip().startswith('local '):
                lines.append(line)
                continue
            changed, quoted = assignment.subn(lambda match: replacement(match, True), line)
            changed, unquoted = unquoted_assignment.subn(lambda match: replacement(match, False), changed)
            lines.append(changed)
            count += quoted + unquoted
        changed = ''.join(lines)
        if count:
            path.write_text(changed)
            command(["bash", "-n", str(path)])
            counts[str(path.relative_to(repo))] = count
    return counts


def normalize(data, work, artifact):
    text = data.decode("utf-8", errors="backslashreplace")
    # Error diagnostics may contain generated shell line numbers or temp paths.
    text = text.replace(str(work), "<experiment>")
    for mode in MODES:
        text = text.replace(f"/{mode}/", "/<mode>/")
    if artifact.endswith(".stderr"):
        text = re.sub(r"line [0-9]+", "line <n>", text)
        if artifact == "profiling.stderr":
            text = re.sub(r"[0-9]+ms", "<ms>", text)
            text = re.sub(r"\[[0-9]+\.[0-9]+\]", "[<time>]", text)
    return text


def compare_semantics(results, work):
    baseline = results["C_native"]
    differences = {}
    for mode, folder in results.items():
        different = []
        for path in sorted(baseline.glob("*")):
            if path.suffix not in (".stdout", ".stderr", ".state", ".session", ".store"):
                continue
            expected = normalize(path.read_bytes(), work, path.name)
            actual_path = folder / path.name
            actual = normalize(actual_path.read_bytes(), work, path.name) if actual_path.exists() else "<caller exited before recording state>"
            if actual != expected:
                different.append({"artifact": path.name, "baseline": expected, "actual": actual})
        differences[mode] = different
    return {"cases": len(list(baseline.glob("*.state"))), "differences": differences,
            "send_depths": {mode: [int(line.split()[1]) for line in (folder / "depth").read_text().splitlines()]
                            for mode, folder in results.items()},
            "class_map_callback_depths": {mode: [int(line.split()[1]) for line in (folder / "map.depth").read_text().splitlines()
                                                 if line.split()[2:] == ["ResultProbe", "valueWith:"]]
                                          for mode, folder in results.items()}}


def summarize(rows):
    summary = {}
    rng = random.Random(0)
    for case in CASES:
        by_mode = {mode: sorted([row for row in rows if row["case"] == case and row["mode"] == mode], key=lambda r: r["sample"])
                   for mode in MODES}
        values = {mode: [r["microseconds"] / r["loops"] / 1000 for r in group] for mode, group in by_mode.items()}
        summary[case] = {}
        for mode, times in values.items():
            paired = [(a-b)/a*100 for a, b in zip(values["C_native"], times)]
            boots = sorted(statistics.median(rng.choices(paired, k=len(paired))) for _ in range(2000))
            summary[case][mode] = {"median_ms": statistics.median(times),
                "p95_batch_mean_ms": sorted(times)[math.ceil(.95*len(times))-1],
                "median_paired_improvement_percent": statistics.median(paired),
                "paired_bootstrap_95_percent": [boots[49], boots[1949]]}
    return summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--samples", type=int, default=24)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--semantics-only", action="store_true")
    parser.add_argument("--unchecked-b", action="store_true", help="reproduce unsafe B failure cases; semantics only")
    args = parser.parse_args()
    if args.samples < 3:
        parser.error("use at least 3 samples")
    if args.unchecked_b:
        if not args.semantics_only:
            parser.error("--unchecked-b requires --semantics-only")
        ENV["EXP_UNCHECKED_B"] = "1"
    output = args.output.resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    metadata = {"date_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
                "commit": command(["git", "rev-parse", "HEAD"], cwd=ROOT).stdout.strip(),
                "bash": command(["bash", "--version"]).stdout.splitlines()[0],
                "jq": command(["jq", "--version"]).stdout.strip(),
                "platform": command(["uname", "-sm"]).stdout.strip(),
                "load_start": os.getloadavg(), "samples": args.samples, "comparison_baseline": "C_native",
                "experiment_hashes": {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
                                      for p in HERE.iterdir() if p.suffix in (".py", ".bash", ".trash")}}
    command(["make", "bash"], cwd=ROOT)
    with tempfile.TemporaryDirectory(prefix="trash-result-experiment-") as temporary:
        work = Path(temporary)
        template = work / "template"
        copy_repo(ROOT, template)
        source = template / "trash/ResultProbe.trash"
        source.write_text((HERE / "Probe.trash").read_text())
        compile_fixture(template, source, template / "trash/.compiled/ResultProbe")
        child = template / "trash/ResultChild.trash"
        child.write_text("ResultChild subclass: ResultProbe\n")
        compile_fixture(template, child, template / "trash/.compiled/ResultChild")
        for name, text in {
            "ResultOverride": "ResultOverride subclass: ResultProbe\n  classMethod: constant [ ^ 'override' ]\n",
            "ResultCold": "ResultCold subclass: Object\n  classMethod: constant [ ^ 'cold' ]\n",
            "ResultTrait": "ResultTrait trait\n  method: traitValue [ ^ 'trait' ]\n",
            "ResultTraitUser": "ResultTraitUser subclass: Object\n  include: ResultTrait\n",
        }.items():
            is_trait = name == "ResultTrait"
            source_path = template / "trash" / ("traits" if is_trait else "") / f"{name}.trash"
            output_path = template / "trash/.compiled" / ("traits" if is_trait else "") / name
            source_path.write_text(text)
            compile_fixture(template, source_path, output_path)
        reload_source = work / "reload.trash"
        reload_source.write_text(source.read_text().replace("plain result", "reloaded result"))
        compile_fixture(template, reload_source, work / "ReloadedProbe")
        generated = generated_runtime(template)
        caps = capabilities(template, args.unchecked_b)
        variants, semantic_dirs = {}, {}
        for mode in MODES:
            home = work / mode
            home.mkdir()
            repo = home / "repo"
            copy_repo(template, repo)
            support = home / "support"
            support.mkdir()
            for name in ("runtime.bash", "harness.bash"):
                shutil.copy2(HERE / name, support / name)
            (support / "generated.bash").write_text(generated)
            (support / "capabilities.bash").write_text(caps)
            shutil.copy2(work / "ReloadedProbe", support / "ReloadedProbe")
            # C quantifies a generic bridge's overhead. Candidate lowering uses
            # its natural calling convention, compared with unchanged C_native.
            metadata.setdefault("lowering", {})[mode] = lower_captures(repo, mode) if mode != "C_native" else {}
            result = home / "semantics"
            env = dict(environment(repo, support, result), EXP_MODE=mode)
            proc = command(["bash", str(support / "harness.bash"), "semantics"], env=env, cwd=repo)
            if proc.stdout or proc.stderr:
                raise RuntimeError(f"{mode} unexpected harness output: {proc.stdout} {proc.stderr}")
            semantic_dirs[mode] = result
            variants[mode] = (repo, support, home)
        semantics = compare_semantics(semantic_dirs, work)
        for mode in MODES:
            scalar_depth = 2 if mode in ("C", "C_native") else 1 if mode.startswith("A_") else 0
            map_depth = 5 if mode in ("C", "C_native") else 4 if mode == "A_guard" else 3
            assert semantics["send_depths"][mode] == [scalar_depth], (mode, semantics["send_depths"])
            assert semantics["class_map_callback_depths"][mode] == [map_depth] * 25, (mode, semantics["class_map_callback_depths"])
        report = {"metadata": metadata, "semantics": semantics, "samples": []}
        output.write_text(json.dumps(report, indent=2) + "\n")
        print(json.dumps({"semantics_cases": semantics["cases"], "differences": {m: len(v) for m, v in semantics["differences"].items()},
                          "send_depths": semantics["send_depths"]}), flush=True)
        if any(semantics["differences"][mode] for mode in ("C_native", "A_guard", "B_naive", "B_guard")):
            raise RuntimeError(f"guarded compatibility failed; details saved to {output}")
        if args.semantics_only:
            return
        workers = {}
        logs = []
        try:
            for mode, (repo, support, home) in variants.items():
                result = home / "benchmark"
                env = dict(environment(repo, support, result), EXP_MODE=mode)
                log = (home / "worker.err").open("w+")
                logs.append(log)
                proc = subprocess.Popen(["bash", str(support / "harness.bash"), "benchmark"], cwd=repo,
                                        env=env, text=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=log)
                workers[mode] = proc
                if proc.stdout.readline().strip() != "READY":
                    log.seek(0)
                    raise RuntimeError(f"{mode} failed to start: {log.read()}")
            for sample in range(-1, args.samples):
                order = MODES[sample % len(MODES):] + MODES[:sample % len(MODES)]
                for case, loops in CASES.items():
                    for mode in order:
                        proc = workers[mode]
                        proc.stdin.write(f"{case} {loops} {max(sample, 0)}\n")
                        proc.stdin.flush()
                        line = proc.stdout.readline()
                        try:
                            row = json.loads(line)
                        except ValueError as exc:
                            for log in logs:
                                log.flush(); log.seek(0)
                                print(log.read())
                            raise RuntimeError(f"{mode} invalid benchmark: {line}") from exc
                        if sample >= 0:
                            report["samples"].append(row)
                if sample >= 0 and (sample + 1) % 5 == 0:
                    print(f"Finished {sample+1}/{args.samples} paired rounds", flush=True)
            report["summary"] = summarize(report["samples"])
            report["metadata"]["load_end"] = os.getloadavg()
            output.write_text(json.dumps(report, indent=2) + "\n")
            print(json.dumps(report["summary"], indent=2), flush=True)
        finally:
            for proc in workers.values():
                if proc.poll() is None:
                    proc.stdin.write("stop 0 0\n"); proc.stdin.flush()
                    try:
                        proc.wait(timeout=10)
                    except subprocess.TimeoutExpired:
                        proc.kill(); proc.wait()
            for log in logs:
                log.close()


if __name__ == "__main__":
    main()
