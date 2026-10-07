# Machine setups

Settings that differ between machines live in `machines/<name>/`, so a machine's
setup is versioned instead of living only in dotfiles under `$HOME`. This
directory holds the settings of the tools around Trashtalk (omlx, pi,
Hindsight). Trashtalk's own settings are per user: put a machine's values in a
`host:` subclass of your preferences class (see the [README](../README.md#configure)
and [docs/settings-design.md](../docs/settings-design.md)).

```bash
bin/trash-machine list
bin/trash-machine diff  omlx-pi    # what would change; changes nothing
bin/trash-machine apply omlx-pi    # backs up each file it changes
```

A machine directory holds any of these. Missing parts are skipped, so a machine
that only picks a Gusgus harness needs just `trashtalk.config`.

| Part | Applied as |
| --- | --- |
| `trashtalk.config` | Legacy, being retired: symlinked to `~/.config/trashtalk/config`, which is still read after your preferences. A different existing file is moved to `config.bak-trashtalk-<time>`. Move its values into a `host:` preferences class with `@ Config import: 'Name'`. |
| `omlx/settings.json`, `model_settings.json`, `model_profiles.json` | Merged into `~/.omlx/`. Skipped when omlx is not installed. |
| `pi/settings.json` | Merged into `~/.pi/agent/settings.json`, created if absent. |
| `hindsight/config.json` | Merged into `~/.hindsight/config.json`, created if absent. |
| `patches/*.patch` | Applied to the installed `@walodayeet/hindsight-pi` package, and re-applied after `pi update` replaces it. |

JSON parts are fragments. Objects merge key by key and arrays are unioned, so
anything a fragment does not name stays as it is: omlx's `auth` block, the pi
`packages` list, downloaded models. Keep only the settings that differ from the
defaults, and never put keys or tokens in a machine directory. Each changed file
is backed up as `<file>.bak-trashtalk-<time>` first, and a second `apply` changes
nothing.

## Adding a machine

Copy the closest directory, or start empty:

```bash
mkdir machines/laptop                                  # omlx, pi, Hindsight parts
bin/trash-machine apply laptop
@ Trash newPreferencesClass: 'Laptop' subclassing: 'Chaz'   # Trashtalk settings: add host: laptop
```

A machine that runs Gusgus on a hosted harness needs no omlx, pi, or Hindsight
parts at all. Compare [jcode-sol](jcode-sol/README.md) with
[omlx-pi](omlx-pi/README.md).
