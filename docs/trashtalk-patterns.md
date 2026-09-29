# DSL recipes

Use ordinary methods for control flow and domain behavior. These examples form
one compilable class; the documentation smoke test checks their public results.
See [LANGUAGE](../LANGUAGE.md) for syntax and
[The Way of Trashtalk](the-way-of-trashtalk.md) for design idioms.

<!-- smoke: recipes -->
```smalltalk
RecipeExamples subclass: Object
  instanceVars: total:0

  # Updating a field needs no _ivar/_ivar_set calls.
  method: add: amount [
    total := total + amount.
    ^ total
  ]

  # The range excludes its upper bound. Accumulate in this method's shell.
  classMethod: sumBelow: maximum [
    | sum |
    sum := 0.
    1 to: maximum do: [:n | sum := sum + n].
    ^ sum
  ]

  # Printing multiple lines is intentional, so preserve every send's output.
  classMethod: printRange [
    pragma: stream
    1 to: 4 do: [:n | @ Console print: n].
    @ Console print: 'done'
  ]

  classMethod: category: number [
    (number > 0) and: [number < 10] ifTrue: [^ 'small positive'].
    ^ 'other'
  ]

  classMethod: label: text [
    (text isEmpty) ifTrue: [^ 'unnamed'].
    ^ text trimmed
  ]

  # Bind several fields in one JSON decode. Use jsonAt: for encoded JSON and
  # jsonTextAt: for text when only one field is needed.
  classMethod: describe: record [
    record jsonUnpack: #('name' 'count') into: [:name :count |
      ^ name , ': ' , count
    ]
  ]

  classMethod: status: name [
    ^ name caseOf: {
      'ready' -> ['Ready to run'].
      #('failed' 'cancelled') -> ['Needs review']
    } otherwise: ['Pending']
  ]

  classMethod: requireName: name [
    (name isEmpty) ifTrue: [@ InputError signal: 'A name is required'].
    ^ name
  ]

  classMethod: nameOrDefault: name [
    ^ (@ self requireName: name) ifFailed: [^ 'anonymous']
  ]
```

For newline-separated IDs, `ids linesDo: [:id | @ id reload]` avoids splitting
on spaces. For JSON arrays, use `arrayEach:`; for persistent Array objects, use
`@ items do:`. Their collection and callback contracts are documented in
[JSON values](json-values.md) and [LANGUAGE](../LANGUAGE.md#array-class).

Use `ifFailed:` when subsequent work depends on a send succeeding. A handler
can return a fallback or re-raise with `[:error | @ error signal]`. Failed
process commands may instead return a result record: inspect that API's exit
code contract rather than assuming all nonempty output means success.

Keep unavoidable shell code small and say why it is raw. For example, this
method needs an external command and output redirection:

```smalltalk
rawClassMethod: write: data to: path [
  printf '%s' "$1" > "$2"
]
```

Prefer an existing `File`, `Process`, or Tool message when it already provides
the operation. Heredocs, traps, process substitution, and external command
pipelines remain Bash boundaries. DSL triple strings handle multiline values
without a heredoc. Converting raw code to DSL alone does not establish a
performance improvement; measure the public workflow.
