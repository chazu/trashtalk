#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
driver="$ROOT/lib/jq-compiler/driver.bash"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
export TRASHTALK_DIR="$work" TRASHDIR="$work/trash" TRASHTALK_COMPILED_DIR="$work/trash/.compiled"
mkdir -p "$TRASHDIR/traits" "$TRASHDIR/Local" "$TRASHDIR/Remote"
cp "$ROOT/trash/Protocol.trash" "$TRASHDIR/Protocol.trash"
printf 'Object subclass: nil\n' > "$TRASHDIR/Object.trash"
write() { printf '%s\n' "$2" > "$TRASHDIR/$1.trash"; }
build() { "$driver" compile-cached "$TRASHDIR/$1.trash" "$TRASHTALK_COMPILED_DIR/${1//\//__}" > "$work/out" 2> "$work/err" || { cat "$work/err"; return 1; }; }
reject() {
    if "$driver" compile-cached "$TRASHDIR/$1.trash" "$TRASHTALK_COMPILED_DIR/${1//\//__}" > "$work/out" 2> "$work/err"; then
        echo "FAIL: accepted $1"; exit 1
    fi
    if ! rg -q "$2" "$work/err"; then cat "$work/err"; exit 1; fi
}
write Contract 'Contract subclass: Protocol
 requires: endpoint
 requires: request: options:
 requires: size
 requires: value:'
write Base 'Base subclass: Object
 method: endpoint [ ^ "base" ]'
write traits/Requests 'Requests trait
 method: request: a options: b [ ^ a ]'
write Client 'Client subclass: Base
 include: Requests
 implements: Contract
 instanceVars: value:0
 method: count [ ^ "1" ]
 alias: size for: count'
build Client
jq -e '.validation.schema==1 and .validation.result=="valid" and (.validation.protocol_hashes.Contract|length)>0' "$TRASHTALK_COMPILED_DIR/.buildcache/Client.json" >/dev/null
cp "$TRASHTALK_COMPILED_DIR/.buildcache/Client.json" "$work/receipt"
build Client
cmp "$work/receipt" "$TRASHTALK_COMPILED_DIR/.buildcache/Client.json"
rg -q 'unchanged' "$work/out"
source "$ROOT/lib/protocols.bash"
_conforms_to Client Contract > "$work/result"
test "$(cat "$work/result")" = true
# Warm boundary must spawn no jq/hash/reflection process.
jq() { echo 'unexpected jq' >&2; return 99; }
shasum() { echo 'unexpected hash' >&2; return 99; }
_conforms_to Client Contract > "$work/result"
test "$(cat "$work/result")" = true
unset -f jq shasum
_class_has_method Client request:options:
! _class_has_method Client _private
write Missing 'Missing subclass: Object
 classMethod: endpoint [ ^ "class-only" ]'
build Missing
_conforms_to Missing Contract > "$work/result"
test "$(cat "$work/result")" = false
jq() { return 99; }; shasum() { return 99; }
_conforms_to Missing Contract > "$work/result"
test "$(cat "$work/result")" = false
unset -f jq shasum
write Missing 'Missing subclass: Object
 implements: Contract
 classMethod: endpoint [ ^ "class-only" ]'
reject Missing 'endpoint, request:options:, size, value:'
write Missing 'Missing subclass: Object'
write Clash 'Clash trait
 method: request: a options: b [ ^ b ]'
# Move into trait directory; the graph must preserve direct-trait conflicts.
mv "$TRASHDIR/Clash.trash" "$TRASHDIR/traits/Clash.trash"
write AmbiguousClient 'AmbiguousClient subclass: Object
 include: Requests
 include: Clash'
build AmbiguousClient
write TraitContract 'TraitContract subclass: Protocol
 requires: request: options:'
build TraitContract
_conforms_to AmbiguousClient TraitContract > "$work/result"
test "$(cat "$work/result")" = false
printf ' include: Clash\n'  >> "$TRASHDIR/Client.trash"
reject Client 'conflicting direct traits: request:options:'
printf ' method: request: a options: b [ ^ a ]\n' >> "$TRASHDIR/Client.trash"
build Client
_conforms_to Client Contract > "$work/result"
test "$(cat "$work/result")" = true
write TraitContract 'TraitContract subclass: Protocol
 requires: request: options:'
write TraitParent 'TraitParent subclass: Object
 include: Requests'
write TraitChild 'TraitChild subclass: TraitParent'
build TraitChild
build TraitContract
_conforms_to TraitChild TraitContract > "$work/result"
test "$(cat "$work/result")" = false
printf ' implements: TraitContract\n' >> "$TRASHDIR/TraitChild.trash"
reject TraitChild 'lacks: request:options:'
write TraitChild 'TraitChild subclass: TraitParent'
write Bad 'Bad subclass: Object
 requires: endpoint'
reject Bad 'selector requires:'
write Bad 'Bad subclass: Protocol
 requires: _private'
reject Bad 'private required selector'
write Bad 'Bad subclass: Contract'
reject Bad 'protocol inheritance'
write Bad 'Bad trait
 implements: Contract'
reject Bad 'only allowed on classes'
write Bad 'Bad subclass: Protocol
 implements: Contract'
reject Bad 'only allowed on classes'
write Bad 'Bad subclass: Object
 implements: Base'
reject Bad 'not a protocol'
write Bad 'Bad subclass: Object
 implements: Absent'
reject Bad 'Missing build dependency'
write Bad 'Bad subclass: Object
 implements: Contract
 implements: Contract'
reject Bad 'duplicate implements:'
write Remote/Ready 'package: Remote
Ready subclass: Protocol
 requires: ready'
write Local/Consumer 'package: Local
 import: Remote
Consumer subclass: Object
 implements: Ready
 method: ready [ ^ "yes" ]'
reject Local/Consumer 'Remote::Ready'
write Local/Consumer 'package: Local
 import: Remote
Consumer subclass: Object
 implements: Remote::Ready
 method: ready [ ^ "yes" ]'
build Local/Consumer
_conforms_to Local::Consumer Remote::Ready > "$work/result"
test "$(cat "$work/result")" = true
# Standalone compilation cannot bypass validation.
if "$driver" compile "$TRASHDIR/Local/Consumer.trash" > "$work/standalone" 2> "$work/err"; then exit 1; fi
rg -q 'Unresolved protocol dependencies' "$work/err"
# Reverse closure: rebuilding only a protocol still validates its clients.
cp "$TRASHDIR/Contract.trash" "$work/contract"
printf ' requires: added\n' >> "$TRASHDIR/Contract.trash"
reject Contract 'lacks: added'
cp "$work/contract" "$TRASHDIR/Contract.trash"
build Contract
cp "$TRASHDIR/Base.trash" "$work/base"
write Base 'Base subclass: Object'
reject Base 'lacks: endpoint'
cp "$work/base" "$TRASHDIR/Base.trash"
build Base
# Dynamic-only checks change after a trait rebuild as well as nominal checks.
write Structural 'Structural subclass: Object
 include: Requests'
build Structural
_conforms_to Structural TraitContract > "$work/result"
test "$(cat "$work/result")" = true
write traits/Requests 'Requests trait'
build traits/Requests
_conforms_to Structural TraitContract > "$work/result"
test "$(cat "$work/result")" = false
# Same-package resolution and canonical duplicate detection.
write Local/Ready 'package: Local
Ready subclass: Protocol
 requires: ready'
write Local/LocalClient 'package: Local
LocalClient subclass: Object
 implements: Ready
 method: ready [ ^ "yes" ]'
build Local/LocalClient
_conforms_to Local::LocalClient Local::Ready > "$work/result"
test "$(cat "$work/result")" = true
printf ' implements: Local::Ready\n' >> "$TRASHDIR/Local/LocalClient.trash"
reject Local/LocalClient 'duplicate implements:'
sed '$d' "$TRASHDIR/Local/LocalClient.trash" > "$work/restored"
cp "$work/restored" "$TRASHDIR/Local/LocalClient.trash"
# Public aliases must wrap real instance functions; private targets/class-only
# wrappers cannot certify an instance capability.
write PrivateClient 'PrivateClient subclass: Object
 method: _hidden [ ^ "secret" ]
 alias: publicAlias for: _hidden
 classMethod: onlyClass [ ^ "class" ]
 alias: classAlias for: onlyClass'
build PrivateClient
! _class_has_method PrivateClient _hidden
! _class_has_method PrivateClient publicAlias
! _class_has_method PrivateClient classAlias
# Ambiguous filenames and a changed registration are rejected before writing.
mkdir -p "$TRASHDIR/user"
cp "$TRASHDIR/Contract.trash" "$TRASHDIR/user/Contract.trash"
reject Client 'Ambiguous build identity'
rm "$TRASHDIR/user/Contract.trash"
mv "$TRASHDIR/Contract.trash" "$TRASHDIR/user/Contract.trash"
reject Client 'identity shadowed'
mv "$TRASHDIR/user/Contract.trash" "$TRASHDIR/Contract.trash"
# Unpromised protocols can change, invalidating a successful dynamic cache.
_conforms_to Local::Consumer Remote::Ready > "$work/result"
printf ' requires: another\n' >> "$TRASHDIR/Remote/Ready.trash"
reject Remote/Ready 'lacks: another'
sed '$d' "$TRASHDIR/Remote/Ready.trash" > "$work/restored"
cp "$work/restored" "$TRASHDIR/Remote/Ready.trash"
write DynamicContract 'DynamicContract subclass: Protocol
 requires: ready'
build DynamicContract
_conforms_to Local::Consumer DynamicContract > "$work/result"
test "$(cat "$work/result")" = true
printf ' requires: another\n' >> "$TRASHDIR/DynamicContract.trash"
build DynamicContract
_conforms_to Local::Consumer DynamicContract > "$work/result"
test "$(cat "$work/result")" = false
# Invalid names never construct a path or source executable artifacts.
printf 'touch "%s"\n' "$work/pwned" > "$TRASHTALK_COMPILED_DIR/Evil"
if _conforms_to Client '../Evil' > "$work/result" 2> "$work/err"; then exit 1; fi
test ! -e "$work/pwned"
# Stale ordinary receipts and altered artifacts fail closed after reload.
printf '{}\n' > "$TRASHTALK_COMPILED_DIR/.buildcache/Client.json"
_protocol_invalidate
if _conforms_to Client Contract > "$work/result" 2> "$work/err"; then exit 1; fi
rg -q 'stale receipt' "$work/err"
build Client
printf '# tampered\n' >> "$TRASHTALK_COMPILED_DIR/Client"
_protocol_invalidate
if _conforms_to Client Contract > "$work/result" 2> "$work/err"; then exit 1; fi
rg -q 'stale artifact' "$work/err"
echo 'PASS: protocol surfaces, nominal errors, cache reuse, dynamic agreement, invalidation, and manifest rejection'
