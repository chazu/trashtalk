# JSON reads and collection traversal

Trashtalk carries JSON as text. These primitives validate one complete JSON
document and keep serialization in the shared Bash/jq boundary.

## Reading values

```smalltalk
value := data jsonAt: 'settings.enabled'.
present := data jsonHas: 'settings.enabled'.
label := data jsonTextAt: 'label'.
limit := data jsonAt: 'limit' ifAbsent: '10'.
```

`jsonAt:` returns encoded JSON: a string retains its quotes, `false` remains
`false`, and an explicit null remains `null`. Missing paths fail the method.
`jsonHas:` returns `true` even for a present null. `ifAbsent:` supplies an encoded
JSON default only for a missing path; it does not replace false, null, zero, or
an empty string. The fallback is evaluated normally and parsed only if needed.

`jsonTextAt:` decodes a string for Bash use; other values are compact JSON text.
Bash cannot represent NUL bytes, so text extraction rejects them. Encoded JSON
from `jsonAt:` can contain `\u0000` safely. Ordinary command substitution follows
Bash's trailing-newline rules; bulk field binding below preserves trailing
newlines in local variables.

Paths are dot-separated object keys. To address array indices or keys containing
dots, pass an encoded path array, such as `'["users",0,"display.name"]'`. The
empty path selects the whole document. Paths are data, never jq programs.

External Bash callers can use the same primitives through public messages:

```bash
@ String jsonAt: settings.enabled from: "$data"
@ String jsonTextAt: label from: "$data"
@ String jsonHas: settings.enabled in: "$data"
@ String jsonAt: limit from: "$data" ifAbsent: 10
```

The older `jsonPath:`, `arrayAt:`, and `objectAt:` keep their legacy text/absence
behavior, including collapsing false and null to empty output. Use the explicit
read primitives when presence and JSON type matter. `String isJson:` accepts any
single valid JSON value, including false and null, and rejects multiple documents.

## Binding several fields

```smalltalk
capture jsonUnpack: #('exit_code' 'stdout' 'stderr') into: [:code :out :err |
  ^ #{status: (code jsonValue) output: out diagnostic: err} asJson
]
```

All fields are read with one jq process, before any binding or block execution.
Strings are decoded; other values become compact JSON text. Every path must be
present. Malformed JSON, missing fields, NUL text, and a field/binding count
mismatch fail without executing the block. Literal path arrays are supported:
`#('status' #('users' 0 'display.name'))`.

The inline block runs in the method's shell, so `^` returns from the enclosing
method and assignments can update its locals or instance variables. Binding
names beginning with `__tj_` or `__json_` are reserved for the implementation.

## Traversing collections

```smalltalk
total := 0.
values arrayEach: [:value | total := total + value].
settings objectEach: [:key :value | @ Console print: key , ': ' , value].
settings objectKeysEach: [:key | @ Console print: key].
settings objectValuesEach: [:value | @ Console print: value].
```

Each traversal decodes its input once into a Bash array. Object keys follow the
same sorted order as the legacy `objectKeys` primitive. `objectValuesEach:`
preserves the input object's insertion order, matching `objectValues` and
`Dictionary valuesDo:`. Key-only traversal does not decode values. Traversals use a snapshot:
mutating the input during a callback does not change the remaining iteration.
The inline body runs in the method's shell; nested loops and early method returns
are supported. Decode errors run no iterations.

These collection primitives preserve the legacy text representation used by
Array and Dictionary: false/null become empty strings, and nested containers
become JSON text. Whitespace and quotes in string elements are preserved.

`arrayCollect:`, `arraySelect:`, `objectCollect:`, and `objectSelect:` take an
existing callback receiver. They decode once, invoke callbacks through public
message sends, and assemble the result once. Array callbacks receive a value;
object collect callbacks receive a value; object select callbacks receive a key
and value. Results retain the existing collection contract: mapped values are
strings, and select accepts nonempty callback output. Callback stdout is captured
using the ordinary Bash message-result contract.

The public Array/Dictionary methods use these primitives and create the final
result object once. `do:`, `detect:`, and `inject:into:` also use one decoded
snapshot. Browser instance traversal uses the same `objectEach:` primitive.
