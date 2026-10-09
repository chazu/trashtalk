# Settings and preferences in Trashtalk

**Status:** Implemented 2026-10-07 (slices 1 to 3). Slice 4, removing the
legacy config file and the `trashtalk.config` machine part, waits until
existing files have been imported. `trash/Settings.trash`,
`trash/Preferences.trash`, the groups listed under [migration](#migration),
`lib/config.bash`, `tests/test_settings.bash`, and
`lib/jq-compiler/tests/test_settings_validation.bash`.
**Date:** 2026-10-07

## Decision

Move configuration out of the TOML-subset file and the Bash declaration table
and into two declarative class kinds:

- A **`Settings`** subclass is how a subsystem declares what can be configured.
  Each `setting:` line names one setting, its type, default, and description.
- A **`Preferences`** subclass holds a user's values, one literal send per line.
  Subclassing layers one set of preferences over another, so a machine can
  override a user's choices.

Both forms are data, not code. The build checks preferences against the
declarations, the same way it checks `implements:` against protocols. The
runtime still reads a setting with Bash builtins and no subprocess.

```smalltalk
package: Agent

JcodeSettings subclass: Settings
  prefix: jcode
  setting: model type: string default: 'gpt-5.6-terra'
    doc: 'Model for Jcode sessions'
  setting: provider type: string default: 'openai'
    doc: 'openai (subscription login) or the name of a local OpenAI-compatible endpoint'
  setting: contextWindow type: integer default: 65536
    doc: 'Context window in tokens for a local Jcode provider'
```

```smalltalk
Chaz subclass: Preferences
  Agent::JcodeSettings model: 'gpt-5.6-terra'
  GusgusSettings profile: 'pi'
```

## Problem

[Configuration](config-design.md) gave every setting a declaration, a default,
a description, and a single user file. Two parts of it do not fit how
Trashtalk code is organized:

- **Declarations live away from their owners.** All 22 keys are
  `_trash_config_declare` calls in `lib/config.bash`. A package that adds a
  setting edits a shared Bash file instead of declaring it next to its own
  classes, and an application written in Trashtalk cannot declare settings
  without editing the runtime.
- **User values are in a second language.** The file is TOML, checked only when
  a key is read or when `Trash doctor` runs. A misspelled key does not fail
  until doctor runs. The values don't appear in the class browser and aren't
  connected to the classes that use them.

The configuration design rejected "a `.trash` file of config code" because code
can run any logic, cannot be listed or validated, and cannot be safely rewritten
by `at:put:`. Restricting both class kinds to declarations and literal values
addresses each of those objections; see [build validation](#build-validation)
and [writing from the REPL](#writing-from-the-repl).

## Goals

- A package or application declares its settings in Trashtalk, next to the
  classes that read them.
- Every setting can be listed, with its type, default, description, current
  value, and source, without loading the classes that own it.
- A user sets values in Trashtalk, in a file they can diff and keep in a
  dotfiles repository.
- An unknown setting, unknown group, or wrong-typed value fails the build.
- Machine-specific values layer over a user's values without copying them.
- Setting a value from the REPL edits the user's source file.
- Existing `@ Config at: 'jcode.model'` calls and `TRASHTALK_*` environment
  overrides keep working.
- Reading a setting forks nothing.

## Non-goals

These carry over from the [configuration design](config-design.md#non-goals):

- **Live reload.** A running process keeps the values it started with.
- **Secrets.** A setting may name a credential variable, never hold its value.
- **Runtime and compiler switches.** Bootstrap paths and the switches listed
  under [environment-only switches](config-design.md#environment-only-switches)
  stay env-only.

New to this design:

- **Computed values.** Preferences hold literals only. Anyone who needs a value
  computed at start-up can still export it from `.trashrc`; the environment
  variable wins.
- **Per-object or per-workspace settings.** A settings group is class-side
  only. Scoped settings (`@ Agent::JcodeSettings for: workspace`) are a later
  extension; see [later work](#later-work).

## Design

### Settings groups

A settings group is a direct subclass of `Settings`. Like `Protocol`, only
direct subclasses may declare settings, and a group has no instances: the
class is the singleton.

```smalltalk
GusgusSettings subclass: Settings
  prefix: gusgus
  setting: profile type: #(jcode maki codex pi chad shell) default: 'jcode'
    doc: 'Harness for new Gusgus sessions'
```

| Line | Meaning |
| --- | --- |
| `prefix: name` | First segment of every key in the group. Required, unique across the build. |
| `setting: sel type: T default: V doc: 'text'` | One setting. The key is `prefix.sel`. The keywords come in this order and may span lines. |
| `env: NAME` | Optional, after `doc:`. Overrides the derived environment variable. |

Types are `string`, `integer` (non-negative), `boolean`, or a literal array of
allowed strings (an enum), such as `#(jev decider)`. The default must satisfy
the type; the build checks it. A description that contains an apostrophe uses
double quotes. A setting may not be named after a class-side selector every
group answers (`new`, `describe`, `groups`, `reset`, `class`, `id`, `inspect`,
`printString`).

The environment variable is derived as `TRASHTALK_<PREFIX>_<SELECTOR>` with the
selector in upper snake case: `jcode.contextWindow` becomes
`TRASHTALK_JCODE_CONTEXT_WINDOW`. Most current keys follow that rule. The ones
that do not, such as `agent.controlWait` and `TRASHTALK_CONTROL_WAIT`, declare
`env:`.

A group may hold class methods besides its settings, for example a derived
value or a validation that spans two settings. It may not declare instance
variables or `include:` traits.

### Reading a setting

The compiler generates one class-side getter per setting. Each is a primitive
over the existing builtin reader:

```smalltalk
model := @ Agent::JcodeSettings model.
```

compiles the getter to

```bash
__Agent__JcodeSettings__class__model() { trash_config_at 'jcode.model'; }
```

The setter `model:` compiles to `trash_settings_put 'jcode.model' "$1"`.
`@ Config at: 'jcode.model'` keeps working and returns the same value. New code
uses the getter, which the class browser can find; the callers listed under
[migration](#migration) use it. `Config at:` remains for callers that compute
the key.

### Manifest

The build writes the settings of every group into two generated files under
`trash/.compiled/`, next to `.protocol-manifest.json`:

- `.settings-manifest.json`: groups, keys, types, defaults, descriptions,
  environment variables, and owning classes, for `list`, `describe`, doctor,
  and Innards.
- `.settings.bash`: the same declarations, and every preferences class's
  values, as calls that fill Bash associative arrays. It replaces the
  hand-written `_trash_config_declare` table, so `trash_config_at` still looks
  up a key's type and default without jq.

Both are derived from `.protocol-manifest.json` after every build, so they
cover every class, not only the ones a build compiled. An entry the manifest no
longer describes is kept while its source file exists, so a manifest rebuilt
from a partial build loses nothing.

`lib/config.bash` sources `.settings.bash` on the first `at:` in a process. Its
first line carries a content hash; a later `at:` compares it with a builtin
read and sources the table again after a rebuild. A process that never reads a
setting pays nothing.

### Preferences

A preferences class is a subclass of `Preferences` or of another preferences
class. Its body is a list of sends, one per line:

```smalltalk
# My settings on every machine.
Chaz subclass: Preferences
  Agent::JcodeSettings model: 'gpt-5.6-terra'
  GusgusSettings profile: 'pi'   # local harness by default
  Agent::WorkerSettings controlWait: 45
```

Each line is a settings group, one setting selector with a colon, and a
literal: a quoted string (single quotes, or double quotes when the value holds
an apostrophe), an integer, `true`, or `false`. Comment lines and trailing
comments are allowed. Nothing else is: no methods, instance variables, traits,
expressions, or variable references. A group in a package is named with its
qualified name, as in `Agent::JcodeSettings`.

Every preferences class is private to its user, including the classes for
particular machines. They live only in `trash/user/`: the Makefile already
compiles that directory, `.gitignore` already excludes it, and isolated test
checkouts leave it out. This repository never contains a preferences class, so
a committed class can never depend on one. A preferences class takes no
`package:`; a class in `trash/user/` is named by its file. Keep the files in a
dotfiles repository and symlink each file in. Do not symlink the directory: the
build resolves directory links, and the class would then be found twice.

### Layering by subclassing

A preferences subclass overrides some of its superclass's values and inherits
the rest. A user keeps their shared values in one class and adds a subclass for
each machine, each in its own file:

```smalltalk
# trash/user/Chaz.trash: everywhere.
Chaz subclass: Preferences
  Agent::JcodeSettings model: 'gpt-5.6-terra'
  GusgusSettings profile: 'jcode'
```

```smalltalk
# trash/user/Sol.trash: the machine with the local model server.
Sol subclass: Chaz
  host: sol
  GusgusSettings profile: 'pi'
  Agent::JcodeSettings provider: 'local'
  Agent::JcodeSettings baseUrl: 'http://127.0.0.1:8000/v1'
```

The nearest class that sets a key wins.

### Creating a preferences class

```smalltalk
@ Trash newPreferencesClass: 'Chaz' subclassing: 'Preferences'
@ Trash newPreferencesClass: 'Sol' subclassing: 'Chaz'
```

This writes `trash/user/<Name>.trash`, compiles it, and opens it with
`@ Trash edit:`. `createPreferencesClass:subclassing:` does the same without
opening the editor. The body is a commented `# host: name` line, then every setting
from the manifest, commented out, with its description above it. Uncommenting a
line sets that value. The file compiles as written, so an untouched new class
changes nothing.

It fails, writing nothing, when the name is not a capitalized identifier, when
a class of that name already has a source or a compiled artifact, or when the
superclass is neither `Preferences` nor an existing preferences class.

`@ Trash newUserClass: 'Name'` is the general version: it writes a header-only
`Name subclass: Object` to `trash/user/`, compiles it, and opens it
(`createUserClass:` skips the editor). It replaces
`@ Trash new:`, whose skeleton adds an example method and test, never checks
package directories for an existing class, and cannot choose a superclass.
`@ Config template` is replaced by `newPreferencesClass:subclassing:`.

### The active preferences class

The same files are linked into `trash/user/` on every machine, so each machine
picks its class:

1. `TRASHTALK_PREFERENCES`, when set, names the class, such as `Sol`.
2. Otherwise, the class whose `host:` matches the short host name
   (`${HOSTNAME%%.*}`, a Bash variable, so no subprocess), ignoring case.
3. Otherwise, the only direct subclass of `Preferences`, such as `Chaz` on a
   machine with no class of its own.

When no rule picks a class, no preferences apply. When there are several root
classes and nothing else picks one, doctor reports the ambiguity. The build
rejects two classes with the same `host:`, and a `host:` line is the one line
besides settings that a preferences class may hold.

`TRASHTALK_SKIP_USER_CONFIG=1`, which isolated tests set, skips rules 2 and 3
as it skips the user file. A test that needs preferences names a fixture class
with `TRASHTALK_PREFERENCES`, which the test runner unsets. Test checkouts also
drop the copied preferences from `.settings.bash`, since `trash/user/` is not
copied.

### Machine setups

`machines/<name>/` keeps the settings of the tools around Trashtalk (omlx, pi,
Hindsight), which are not secret and are shared by design. Its
`trashtalk.config` part is being retired: a machine's Trashtalk values are that
user's choice and move into a `host:` class in their own preferences, with
`@ Config import:`. Until slice 4, `trash-machine apply` still links the part,
and the linked file is read as the legacy config file.

### Resolution order

`at:` returns the first of:

1. The setting's **environment variable**, when set and not empty.
2. The **active preferences class**, then each superclass in turn.
3. The legacy **config file**, until slice 4 removes it.
4. The setting's **declared default**.

The build compiles the values of every preferences class into `.settings.bash`
as one table per class, plus its superclass. Lookup walks that chain in Bash.

### Build validation

The build rejects, with the file and line:

- a preferences line naming an unknown group or a selector the group does not
  declare;
- a value that fails the setting's type;
- the same key set twice in one preferences class;
- anything in a preferences class besides comments, preference lines, and
  `host:`, including a `package:`;
- preference lines or `host:` in a class that is not a preferences class;
- a malformed `setting:` or preference line (a parse error, not a warning);
- two groups with the same prefix, two classes with the same `host:`, or a
  group with no prefix;
- a default that fails its own type, a duplicate setting, or a setting named
  after a reserved selector;
- `prefix:` or `setting:` outside a direct subclass of `Settings`, and instance
  variables or traits in a group.

These checks run in the graph build alongside protocol validation. A
preferences class depends on the groups it names, so changing a group
revalidates the preferences that use it. The prefix and host checks run when
the settings table is published, after the classes compile. A key that a
superclass sets and a subclass sets again is an override, not an error.

### Discoverability

| Send | Behavior |
| --- | --- |
| `@ Settings groups` | Every group with its prefix and number of settings |
| `@ Agent::JcodeSettings describe` | The group's settings: effective value and source, description, type, default, and environment variable |
| `@ Config list` | Every key with its value and source; a preferences value names the class that set it (`preferences Chaz`) |
| `@ Preferences all` (or `@ Config preferences`) | Every preferences class with its superclass, host, and source; the active one is marked |
| `@ Preferences active` | The active preferences class |
| `@ Config check` | Doctor findings, as before, plus stale, ambiguous, or unknown preferences and keys set both in preferences and the legacy file |

All of these read the manifest and the compiled preferences table; none loads
a group class. Innards can render a settings panel from the same manifest.

### Writing from the REPL

Each group gets a class-side setter per setting that writes to the active
preferences class:

```smalltalk
@ Agent::JcodeSettings model: 'gpt-5.6-terra'.
@ Agent::JcodeSettings reset: 'model'.
```

`@ Config at:put:` and `@ Config reset:` do the same by key. On a machine with
a `host:` class, a write lands in that class and affects only that machine.
`@ Config at: key put: value in: 'Chaz'` (and `reset: key in: 'Chaz'`) writes
to a superclass instead, so the value applies everywhere that does not override
it.

- The value is checked against the declaration first. Nothing is written on
  failure. A string value may not hold a backslash, or both kinds of quote.
- The write edits the active class's source file: it replaces the setting's
  line, keeping a trailing comment, or appends a new line. Comments, blank
  lines, and order are preserved. If the class no longer compiles afterwards,
  the original file is restored and the send fails.
- The file is written to a temp file and renamed into place. A symlink is
  followed, so a dotfiles link survives.
- After writing, it recompiles that class alone, as `make single` does.
- When the setting's environment variable is set, the write still happens and
  the send warns that the environment shadows it.
- With no active preferences class, the write fails. It suggests
  `@ Config import:` when the legacy file has values, and
  `@ Trash newPreferencesClass:subclassing:` otherwise.

Because a preferences class has one setting per line and literal values only,
this edit is as reliable as the TOML rewrite it replaces.

### Stale preferences

A hand edit to a preferences file takes effect at the next build. Doctor
compares each preferences source with the hash recorded in `.settings.bash` and
reports any that changed since. `at:` does not check, so a read still forks
nothing.

## Migration

The 22 declarations in `lib/config.bash` move into groups next to their
readers. Keys and environment variable names do not change.

| Prefix | Group | Reader |
| --- | --- | --- |
| `gusgus` | `GusgusSettings` | `trash/Gusgus.trash` |
| `jcode`, `codex`, `maki`, `pi` | `Agent::JcodeSettings`, `Agent::CodexSettings`, `Agent::MakiSettings`, `Agent::PiSettings` | `trash/Agent/*Driver.trash` |
| `agent` | `Agent::WorkerSettings` (`env: TRASHTALK_CONTROL_WAIT`) | `trash/Agent/Worker.trash` |
| `assignment` | `AssignmentSettings` | `trash/Assignment.trash` |
| `decision`, `jev`, `decider` | `Decision::TargetSettings`, `OpenRouter::JevSettings`, `Decision::DeciderSettings` | `trash/Decision/Target.trash`, `trash/OpenRouter/Jev.trash` |

Each reader now calls its group's getter. `Settings` and `Preferences` join
`Object`, `Tool`, `TestCase`, and `Protocol` as root classes that a packaged
class names without qualification.

The user file keeps working during migration as a layer between the active
preferences and the default. It is read-only: writes go to a preferences class.
`@ Config import: 'Name'` (or `import: 'Name' subclassing: 'Chaz'`) writes a
preferences class from an existing file and leaves the file in place, and
doctor reports keys set in both. The file reader is removed once dotfiles have
moved and machine setups no longer ship a `trashtalk.config`.

## Slices

1. **Groups and manifest.** `Settings`, `prefix:`, `setting:`, and `env:`;
   generated getters; `.settings-manifest.json` and `.settings.bash`; `Config`
   reads the generated table instead of `_trash_config_declare`. Move the
   `jcode` keys first, then the rest. The TOML file is unchanged. This slice
   puts declarations next to their owners and makes `describe` work.
2. **Preferences.** The `Preferences` class kind, build validation, the active
   class rule, and lookup through the chain. `@ Config import:`, `host:`, and
   `@ Trash newPreferencesClass:subclassing:` and `newUserClass:`.
3. **Writes.** Generated setters, `reset:`, and `Config at:put:` retargeted at
   the active preferences source, with single-class recompilation.
4. **Retire the file.** Remove the TOML reader and the `trashtalk.config`
   machine part once existing files have been imported. Not yet done.

## Later work

- **Scoped settings.** Instances of a group bound to a workspace or session,
  falling back to the class-side value. This needs the same trust rule the
  configuration design gives for a workspace layer: a cloned repository must
  not redirect `decider.url` or choose a harness without the user's consent.
- **Cross-setting validation**, such as requiring `jcode.baseUrl` when
  `jcode.provider` is not `openai`, as a declared check on the group rather
  than a method.
- **Enum coverage.** The build could check that each enum value has a
  `caseOf:` branch in the class that dispatches on it, which the configuration
  design currently leaves to a test.

## Alternatives considered

- **A singleton trait (`include: Configurable`).** Traits are method
  collections: they carry no declarations, and subclasses do not inherit them.
  The setting declarations are the part a configuration surface needs, and the
  class side is already a singleton. A trait would add a second way to get
  per-class state without describing the settings.
- **Settings as class instance variables stored in SQLite.** Assigning
  `classInstanceVars` already persists per-class state. The configuration
  design rejected stored settings because the values cannot be diffed or
  committed; that still holds.
- **Unrestricted configuration code.** Preferences with methods could compute
  values, but could not be validated at build time, listed without running
  them, or rewritten by a REPL send. `.trashrc` covers computed values.
- **Declarations in Trashtalk, values still in TOML.** This gets the first
  slice's benefits without a new class kind for values. It keeps two
  languages, leaves typos to doctor instead of the build, and has no
  inheritance for machine layering. It is the state after slice 1, so it
  remains available if slice 2 is not worth building.
- **Deriving the key prefix from the package name.** Several current prefixes
  (`jcode`, `codex`, `decider`) name a harness or service rather than the
  package that reads them, so an explicit `prefix:` keeps every existing key
  and environment variable unchanged.
