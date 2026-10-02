# Configuration Design

**Status:** Implemented 2026-09-30 (both slices). `trash/Config.trash`,
`lib/config.bash`, and `tests/test_config.bash`.
**Date:** 2026-09-30

## Problem

User-facing settings are scattered `@ Env get: 'X' default: 'Y'` calls in the
classes that use them. Finding which model Gusgus runs means reading three
driver files. There is no list of settings, no description of what each one
does, and no check for a misspelled name: `TRASHTALK_JCODE_MDOEL=...` is
silently ignored.

Current user-facing knobs:

| Env var | Default | Read by |
| --- | --- | --- |
| `TRASHTALK_GUSGUS_PROFILE` | `jcode` | `trash/Gusgus.trash:7` |
| `TRASHTALK_JCODE_MODEL` | `gpt-5.6-terra` | `trash/Agent/JcodeDriver.trash:34` |
| `TRASHTALK_CODEX_MODEL` | `gpt-5.6-terra` | `trash/Agent/CodexDriver.trash:23` |
| `TRASHTALK_MAKI_MODEL` | `openai/gpt-5.6-terra` | `trash/Agent/MakiDriver.trash:16` |
| `TRASHTALK_CONTROL_WAIT` | `30` | `trash/Agent/Worker.trash:153` |
| `TRASHTALK_DECISION_TARGET` | `jev` | `trash/Decision/Target.trash:7` |
| `CLM_BASE_URL` | `http://127.0.0.1:8700` | `trash/Decision/Target.trash:18` |
| `CLM_BC250_URL` | none | `trash/Decision/Target.trash:19` |
| `CLM_MODEL` | `clm-latest` | `trash/Decision/Target.trash:29` |

The Jev model is hardcoded as `typesafe/jev-1.13` in
`trash/OpenRouter/Jev.trash:5`, so it cannot be changed without editing code.

The only way to set these persistently is exporting them from `~/.trashrc`,
which `lib/trash.bash:105` sources on every runtime load.

## Goals

- One place that lists every setting, its default, and what it does.
- A file the user can keep in a dotfiles repository and diff.
- Setting a value from the REPL or Innards without opening an editor, with the
  change landing in that same file.
- Existing env var overrides keep working, including in tests.
- Reading a setting costs no subprocess beyond the send itself.

## Non-goals

- Live reload. Settings are read when a harness or session starts. A running
  process keeps the values it started with; restart the session to pick up a
  change.
- Secrets. API keys stay in the environment. Settings may name a credential
  variable, never hold its value.
- Runtime and compiler switches read directly by Bash (`TRASHTALK_VALUE_SEND`,
  `TRASHTALK_LOG_LEVEL`, `TRASHTALK_PROGRESS`, `TRASHTALK_NO_AUTOTICK`,
  `TRASH_DEBUG`), bootstrap paths (`TRASHTALK_DIR`, `SQLITE_JSON_DB`), and
  internal `_`-prefixed variables. These stay env-only.
- `TRASHTALK_USER`. It is baked into the worker service unit at install time;
  moving it is a separate decision.

## Design

### Where settings come from

`@ Config at: 'jcode.model'` resolves in this order and returns the first hit:

1. **Environment variable** named in the key's declaration
   (`TRASHTALK_JCODE_MODEL`). Existing exports, one-off overrides, and test
   setups keep working unchanged.
2. **User config file**, `${XDG_CONFIG_HOME:-$HOME/.config}/trashtalk/config`.
   `TRASHTALK_CONFIG` names a different file. `TRASHTALK_SKIP_USER_CONFIG=1`,
   which isolated tests already set, skips it.
3. **Declared default.**

An explicit `TRASHTALK_CONFIG` is read even when `TRASHTALK_SKIP_USER_CONFIG`
is set; the isolated test runner unsets it, so a developer's config cannot leak
into tests.

`at:` on an undeclared key fails with `ConfigurationError`, so a typo in code
fails at the call site instead of silently reading nothing. So do a syntax
error anywhere in the file and an invalid value for the requested key, from
the file or the environment. An invalid value for a different key does not
fail the read; `Trash doctor` reports it. An empty environment variable counts
as unset.

### The file

```toml
# Trashtalk configuration. `@ Config list` shows every key and its source.
gusgus.profile = "jcode"
jcode.model = "gpt-5.6-terra"
agent.controlWait = 30
```

The format is a flat subset of TOML: one `dotted.key = value` per line,
strings in double quotes with no escape sequences, bare integers and
`true`/`false`, and `#` comment lines. Any file this reader accepts is valid
TOML, so editors highlight it and a full TOML reader could replace ours later
without changing anyone's file. Table headers (`[jcode]`), inline tables,
arrays, and multi-line strings are rejected with a line number.

A reader this small runs on Bash builtins (`while read`, parameter expansion),
so `at:` forks nothing: no jq and no sqlite. The file is re-read on each
`at:`. It is small, and settings are read at start-up points, not in loops.

### Declaring keys

`lib/config.bash` holds one table of declarations, one
`_trash_config_declare key ENV_VAR type default description` call per key.
`trash/Config.trash` exposes the functions as class primitives.

| Key | Env override | Type | Default | Description |
| --- | --- | --- | --- | --- |
| `gusgus.profile` | `TRASHTALK_GUSGUS_PROFILE` | one of `jcode maki codex shell` | `jcode` | Harness for new Gusgus sessions |
| `jcode.model` | `TRASHTALK_JCODE_MODEL` | string | `gpt-5.6-terra` | Model for Jcode sessions |
| `codex.model` | `TRASHTALK_CODEX_MODEL` | string | `gpt-5.6-terra` | Model for Codex sessions |
| `maki.model` | `TRASHTALK_MAKI_MODEL` | string | `openai/gpt-5.6-terra` | Model for Maki sessions |
| `agent.controlWait` | `TRASHTALK_CONTROL_WAIT` | integer | `30` | Seconds to wait for a harness control reply |
| `assignment.attempts` | `TRASHTALK_ASSIGNMENT_ATTEMPTS` | integer | `4` | Turns an Assignment gets before it needs review |
| `decision.target` | `TRASHTALK_DECISION_TARGET` | one of `jev clm-local clm-bc250 clm-prefer-bc250` | `jev` | Where typed decisions run |
| `jev.model` | `TRASHTALK_JEV_MODEL` | string | `typesafe/jev-1.13` | OpenRouter model for Jev decisions (new) |
| `clm.baseUrl` | `CLM_BASE_URL` | string | `http://127.0.0.1:8700` | Local CLM endpoint |
| `clm.bc250Url` | `CLM_BC250_URL` | string | empty | BC-250 CLM endpoint |
| `clm.model` | `CLM_MODEL` | string | `clm-latest` | CLM model name |

The declarations are kept central rather than spread across owning classes, so
`list` and `doctor` work without loading every class. They are Bash
associative arrays, so `at:` looks up a key's env var and default without jq. Enum values mirror
the `caseOf:` branches in `Agent::Worker` and `Decision::Target`. The
declaration and its `caseOf:` must change together; a test can check that each
declared value has a branch.

Callers change from

```smalltalk
classMethod: model [ ^ @ Env get: 'TRASHTALK_JCODE_MODEL' default: 'gpt-5.6-terra' ]
```

to

```smalltalk
classMethod: model [ ^ @ Config at: 'jcode.model' ]
```

### Interface

| Send | Behavior |
| --- | --- |
| `@ Config at: key` | Effective value, by the order above |
| `@ Config list` | Every key with its value and source (`default`, `file`, or `env` and the variable) |
| `@ Config template` | A commented file with every key at its default, for starting a config |
| `@ Config path` | The user file's path, whether or not it exists |
| `@ Config at: key put: value` | Validates and writes the value into the user file |
| `@ Config reset: key` | Removes the key's lines from the user file |
| `@ Config check` | Doctor findings, one `ok`/`warn`/`bad` line each |

### Writing from the REPL

`at:put:` edits the file in place, so setting a value once from the REPL or
Innards and keeping it under version control are the same act: the change
shows up as a diff in the dotfiles repository.

- It replaces the key's first line, keeping a trailing comment, and drops any
  later duplicates; a new key is appended. Comments, blank lines, and order
  are preserved. It refuses to rewrite a file with a syntax error.
- It validates the key and value against the declaration first, and writes
  nothing on failure. String values cannot contain `"` or `\`.
- It refuses to write when `TRASHTALK_SKIP_USER_CONFIG` is set without an
  explicit `TRASHTALK_CONFIG`.
- It writes a temp file in the target's directory and renames it over the
  target. If the config path is a symlink (GNU Stow and similar tools), it
  writes to the symlink's target so the link survives.
- If the key's env var is set, the write still happens, but the send warns
  that the environment value shadows it. Otherwise an old `.trashrc` export
  would make `at:put:` look broken.

### Version control

Keep `~/.config/trashtalk/config` in a dotfiles repository as a symlink or a
managed copy. Start one with:

```bash
mkdir -p ~/.config/trashtalk
@ Config template > ~/.config/trashtalk/config
```

Because secrets never go in this file, it is safe to commit.

### `.trashrc`

`.trashrc` stays as it is, the place for shell setup: `PATH`, functions, and
any env overrides. Existing exports of the variables above keep working and
win over the file. `@ Config list` shows them with source `env`, which makes a
forgotten export easy to spot.

### Doctor

`Trash doctor` gains:

- file syntax errors with line numbers;
- unknown keys in the file, which are almost always typos;
- values that fail their declared type;
- duplicate keys (the last one wins, but it is reported);
- keys whose file value is shadowed by a set env var (a note, not an error).

### Tests

Isolated tests already set `TRASHTALK_SKIP_USER_CONFIG=1`, so a developer's
config cannot change test results. Config's own tests point
`TRASHTALK_CONFIG` at a temp file and cover: precedence, parse rejection, type
validation, `at:put:` preserving comments and order, writing through a symlink,
and `at:` on an undeclared key.

## Slices

Both slices shipped together.

1. **Read path.** `Config` with declarations, the file reader, `at:`, `list`,
   `template`, `path`, and doctor checks. Migrate the knobs above and replace
   the hardcoded Jev model. Users edit the file by hand. This gives the
   listing, typo checking, and a versionable file.
2. **Write path.** `at:put:` and `reset:` with atomic, symlink-aware writes
   and the shadowing warning.

Later, if needed:

- **Workspace layer:** a checked-in per-repository file between env and the
  user file, so a project can pin its Gusgus model. This needs a trust rule
  first: a cloned repository must not be able to redirect `clm.baseUrl` or
  pick a harness without the user's consent.
- **Table headers** (`[jcode]`) in the reader.

## Environment-only switches

These are read directly by Bash and are not `Config` keys.

| Variable | Effect |
| --- | --- |
| `TRASHTALK_DIR`, `TRASHDIR`, `SQLITE_JSON_DB` | Checkout, class directory, and object database. They default to `~/.trashtalk`, `~/.trashtalk/trash`, and `~/.trashtalk/instances.db` independently; `TRASHDIR` does not follow `TRASHTALK_DIR`, so set all three for another checkout. |
| `TRASH_SESSION_ID` | Shares the session object cache (`/tmp/trashtalk_<id>`) across shells. Defaults to the creating shell's PID. |
| `TRASH_KEEP_ENV=1` | Keeps that cache when the creating shell exits. |
| `TRASH_SKIP_DEPCHECK=1` | Skips the `jq`/`sqlite3`/`uuidgen`/`jo` check when the runtime is sourced. |
| `TRASHTALK_SKIP_USER_CONFIG=1` | Skips `~/.trashrc` when the runtime is sourced, and the user `Config` file (see [where settings come from](#where-settings-come-from)). |
| `HONKER_EXT` | Honker SQLite extension path without its `.dylib`/`.so` suffix, tried before the default locations. |
| `TRASHTALK_INTERACTIVE=1` | Lets `TRASHTALK_PROGRESS=auto` show progress outside an interactive shell; the REPL sets it when attached to a terminal. |
| `TRASHTALK_LOG_LEVEL`, `TRASHTALK_QUIET` | Diagnostic level; see [performance](performance.md#diagnostics-and-progress). `TRASHTALK_QUIET` means `error` when no level is set. |
| `TRASHTALK_STRICT=1` | Compiler: parse warnings, and missing or unparseable traits, fail the compile instead of emitting a partial class. |
| `TRASHTALK_LENIENT=1` | Compiler: ships output containing `# ERROR:` codegen markers with a warning instead of failing. |
| `TRASHTALK_VALUE_SEND=1` | Opt-in capture optimization; see [result passing](result-passing-design.md). |
| `TRASHTALK_HISTORY_FILE` | REPL history (default `~/.trash_history`). |
| `TRASHTALK_WORKER_LOG` | Worker stderr log (default `run/worker/stderr.log`). |
| `TRASHTALK_JCODE_COMPACT_TIMEOUT` | Seconds to wait for Jcode compaction (default 300). |
| `TRASHTALK_SHELL_DRIVER` | Script that `Agent::ShellDriver` runs with `bash -c` for the `shell` agent profile, a model-free harness for the delivery loop. |

Test-runner switches (`TRASH_TEST_JOBS`, `TRASH_TEST_TIMEOUT`, `TRASH_TEST_KEEP`,
`TRASH_TEST_TRACE`) are described in [performance](performance.md).

## Alternatives considered

- **Settings stored in SQLite, set with `at:put:`.** This fits the image idea
  (`instances.db` as the image) and needs no file format. It was rejected
  because the values cannot be diffed or committed, and it splits the source of
  truth between `.trashrc` and the database. Writing through to a file gives
  the same REPL convenience.
- **A `.trash` file of config code compiled at start-up.** This makes config
  Turing-complete: it can't be listed, validated, or safely rewritten by
  `at:put:`, and it needs compile caching. `.trashrc` already covers
  "config as code" for anyone who needs it.
- **Load everything into the environment at start-up.** This adds file or
  database work to every runtime load, including every detached harness and
  worker, for values most processes never read.
- **JSON or YAML.** JSON has no comments and is unpleasant to edit by hand.
  YAML needs a parser dependency. The TOML subset reads with Bash builtins.
