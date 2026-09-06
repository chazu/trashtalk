#!/usr/bin/env python3
"""Check whether a successful constant send survives the caller's set -e."""
import argparse
import json
from pathlib import Path
import shutil
import subprocess
import tempfile

from run import ROOT, HERE, MODES, copy_repo, compile_fixture, environment, generated_runtime, capabilities


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    rows = []
    with tempfile.TemporaryDirectory(prefix="trash-result-strict-") as temporary:
        work = Path(temporary)
        repo = work / "repo"
        copy_repo(ROOT, repo)
        source = repo / "trash/ResultProbe.trash"
        source.write_text((HERE / "Probe.trash").read_text())
        compile_fixture(repo, source, repo / "trash/.compiled/ResultProbe")
        support = work / "support"
        support.mkdir()
        shutil.copy2(HERE / "runtime.bash", support / "runtime.bash")
        (support / "generated.bash").write_text(generated_runtime(repo))
        (support / "capabilities.bash").write_text(capabilities(repo))
        script = '''
source "$TRASHTALK_DIR/lib/trash.bash" || exit 90
source "$EXP_SUPPORT/runtime.bash"
source "$EXP_SUPPORT/generated.bash"
_ensure_class_sourced ResultProbe || exit 91
source "$EXP_SUPPORT/capabilities.bash"
# Keep the runtime's existing cleanup guards; do not let cleanup's status mask
# whether the assigned method itself reached the following statement.
trap '_env_cleanup_on_exit || true' EXIT
[[ "$EXP_STRICT" != 1 ]] || set -e
printf 'before\n'
_exp_assign observed ResultProbe constant
printf 'after:%s\n' "$observed"
'''
        for strict in (False, True):
            for mode in MODES:
                folder = work / f"{mode}-{strict}"
                env = dict(environment(repo, support, folder), EXP_MODE=mode, EXP_STRICT=str(int(strict)))
                result = subprocess.run(["bash", "-c", script], cwd=repo, env=env,
                                        text=True, capture_output=True)
                rows.append({"mode": mode, "errexit": strict, "status": result.returncode,
                             "stdout": result.stdout, "stderr": result.stderr})
        env = dict(environment(repo, support, work / "trace"), EXP_MODE="B_guard", EXP_STRICT="1")
        trace = subprocess.run(["bash", "-c", script.replace("|| set -e", "|| set -ex")],
                               cwd=repo, env=env, text=True, capture_output=True)
        assert trace.returncode == 1
        lines = trace.stderr.splitlines()
        position = next(i for i, line in enumerate(lines) if "_CALL_DEPTH++" in line)
        failure_trace = lines[position:position+3]
    for row in rows:
        expected_failure = row["errexit"] and row["mode"].startswith("B_")
        assert row["status"] == (1 if expected_failure else 0), row
        assert row["stdout"] == ("before\n" if expected_failure else "before\nafter:plain result\n"), row
        assert not row["stderr"], row
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps({"probe": "successful constant send with and without caller errexit",
                                      "cleanup_trap": "_env_cleanup_on_exit || true",
                                      "B_guard_failure_trace": failure_trace,
                                      "results": rows}, indent=2) + "\n")
    print(json.dumps(rows, indent=2))


if __name__ == "__main__":
    main()
