#!/usr/bin/env bash
# Build-time checks for Settings groups and Preferences classes.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
driver="$ROOT/lib/jq-compiler/driver.bash"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
export TRASHTALK_DIR="$work" TRASHDIR="$work/trash" TRASHTALK_COMPILED_DIR="$work/trash/.compiled"
mkdir -p "$TRASHDIR/user" "$TRASHDIR/App"
printf 'Object subclass: nil\n' > "$TRASHDIR/Object.trash"
cp "$ROOT/trash/Settings.trash" "$ROOT/trash/Preferences.trash" "$TRASHDIR/"
write() { printf '%s\n' "$2" > "$TRASHDIR/$1.trash"; }
output() { case $1 in user/*) echo "$TRASHTALK_COMPILED_DIR/${1#user/}" ;; *) echo "$TRASHTALK_COMPILED_DIR/${1//\//__}" ;; esac; }
build() { "$driver" compile-cached "$TRASHDIR/$1.trash" "$(output "$1")" > "$work/out" 2> "$work/err" || { cat "$work/err"; exit 1; }; }
reject() {
    if "$driver" compile-cached "$TRASHDIR/$1.trash" "$(output "$1")" > "$work/out" 2> "$work/err"; then
        echo "FAIL: accepted $1 (expected: $2)"; exit 1
    fi
    if ! grep -qF -- "$2" "$work/err"; then echo "FAIL: $1 did not report: $2"; cat "$work/err"; exit 1; fi
}

write App/ModelSettings 'package: App

ModelSettings subclass: Settings
  prefix: model
  setting: name type: string default: '"'"'small'"'"'
    doc: "The model'"'"'s name"
  setting: size type: integer default: 7 doc: '"'"'Size'"'"'
  setting: mode type: #(fast slow) default: '"'"'fast'"'"' doc: '"'"'Mode'"'"'
    env: APP_MODE'
build App/ModelSettings
grep -qF "trash_config_at 'model.name'" "$TRASHTALK_COMPILED_DIR/App__ModelSettings"
grep -qF "trash_settings_put 'model.size' \"\$1\"" "$TRASHTALK_COMPILED_DIR/App__ModelSettings"
table="$TRASHTALK_COMPILED_DIR/.settings.bash"
grep -qF "_trash_config_declare 'model.name' 'TRASHTALK_MODEL_NAME' 'string' 'small' 'The model'\\''s name' 'App::ModelSettings'" "$table"
grep -qF "_trash_config_declare 'model.mode' 'APP_MODE' 'enum:fast slow' 'fast'" "$table"

write user/Me 'Me subclass: Preferences
  App::ModelSettings name: '"'"'large'"'"'   # trailing comment
  App::ModelSettings size: 9'
build user/Me
write user/Laptop 'Laptop subclass: Me
  host: laptop
  App::ModelSettings mode: "slow"'
build user/Laptop
grep -qF "_trash_prefs_class 'Laptop' 'Me' 'laptop'" "$table"
grep -qF "_trash_prefs_value 'Me' 'model.size' '9'" "$table"
grep -qF "_trash_prefs_value 'Laptop' 'model.mode' 'slow'" "$table"

# Preferences are checked against the groups they name.
write user/Bad 'Bad subclass: Preferences
  App::Nope name: '"'"'x'"'"''
reject user/Bad 'no settings group has that name'
write user/Bad 'Bad subclass: Preferences
  App::ModelSettings colour: '"'"'x'"'"''
reject user/Bad 'App::ModelSettings declares no setting colour'
write user/Bad 'Bad subclass: Preferences
  App::ModelSettings size: '"'"'nine'"'"''
reject user/Bad 'model.size must be an integer'
write user/Bad 'Bad subclass: Preferences
  App::ModelSettings mode: '"'"'medium'"'"''
reject user/Bad 'model.mode must be one of fast, slow'
write user/Bad 'Bad subclass: Preferences
  App::ModelSettings size: 1
  App::ModelSettings size: 2'
reject user/Bad 'model.size is set twice'
write user/Bad 'Bad subclass: Preferences
  App::ModelSettings size: 1
  classMethod: hello [ ^ 1 ]'
reject user/Bad 'holds only preference lines'
write user/Bad 'Bad subclass: Preferences
  App::ModelSettings size: 1 + 2'
reject user/Bad 'Expected a preference'
write user/Bad 'Bad subclass: Object
  App::ModelSettings size: 1'
reject user/Bad 'belong in a subclass of Preferences'
write user/Bad 'Bad subclass: Laptop
  host: laptop'
reject user/Bad 'host laptop is claimed by'
rm -f "$TRASHDIR/user/Bad.trash"

# Groups are checked on their own.
write App/BadSettings 'package: App
BadSettings subclass: Settings
  setting: a type: string default: '"'"'x'"'"' doc: '"'"'d'"'"''
reject App/BadSettings 'a settings group needs prefix: name'
write App/BadSettings 'package: App
BadSettings subclass: Settings
  prefix: bad
  setting: a type: integer default: '"'"'x'"'"' doc: '"'"'d'"'"''
reject App/BadSettings 'default for a must be an integer'
write App/BadSettings 'package: App
BadSettings subclass: Settings
  prefix: bad
  setting: describe type: string default: '"'"'x'"'"' doc: '"'"'d'"'"''
reject App/BadSettings 'would shadow a class method'
write App/BadSettings 'package: App
BadSettings subclass: Settings
  prefix: bad
  setting: a type: string default: '"'"'x'"'"''
reject App/BadSettings 'Expected `setting: name type: T default: value'
write App/BadSettings 'package: App
BadSettings subclass: Object
  prefix: bad'
reject App/BadSettings 'only allowed on a direct Settings subclass'
write App/BadSettings 'package: App
BadSettings subclass: Settings
  prefix: model
  setting: a type: string default: '"'"'x'"'"' doc: '"'"'d'"'"''
reject App/BadSettings 'prefix model is declared by'
rm -f "$TRASHDIR/App/BadSettings.trash"

# Changing a group revalidates the preferences that name it.
write App/ModelSettings 'package: App

ModelSettings subclass: Settings
  prefix: model
  setting: name type: string default: '"'"'small'"'"' doc: '"'"'Name'"'"'
  setting: size type: string default: '"'"'7'"'"' doc: '"'"'Size'"'"'
  setting: mode type: #(fast slow) default: '"'"'fast'"'"' doc: '"'"'Mode'"'"''
reject App/ModelSettings 'model.size must be a string'

echo "settings validation: ok"
