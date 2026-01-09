# Pure Trashtalk Patterns for Common Bash Idioms

This guide shows how to rewrite common Bash patterns in pure Trashtalk DSL,
enabling native compilation with Procyon and cleaner, more maintainable code.

## Why Pure Trashtalk?

Methods written in pure Trashtalk DSL (using `method:`) can be:
- **Natively compiled** to Go binaries via Procyon
- **Automatically transformed** with proper ivar handling
- **More readable** and maintainable

Methods using `rawMethod:` require Bash fallback and cannot be natively compiled.

---

## Loop Patterns

### Bash: for loop with counter
```bash
# Bash
for ((i=0; i<10; i++)); do
  echo "Number: $i"
done
```

```smalltalk
# Trashtalk DSL
method: countToTen [
  | i |
  i := 0.
  [i < 10] whileTrue: [
    @ Transcript print: i.
    i := i + 1
  ]
]
```

### Bash: iterating over array elements
```bash
# Bash
for item in "${array[@]}"; do
  process "$item"
done
```

```smalltalk
# Trashtalk DSL - use Array do:
method: processAll [
  @ items do: [:item |
    @ self process: item
  ]
]
```

### Bash: while loop with condition
```bash
# Bash
while [[ $count -lt $max ]]; do
  ((count++))
done
```

```smalltalk
# Trashtalk DSL
method: countUp [
  [count < max] whileTrue: [
    count := count + 1
  ]
]
```

### Bash: repeat N times
```bash
# Bash
for ((i=0; i<5; i++)); do
  doWork
done
```

```smalltalk
# Trashtalk DSL
method: doWorkFiveTimes [
  5 timesRepeat: [
    @ self doWork
  ]
]
```

### Bash: range iteration
```bash
# Bash
for i in {1..10}; do
  echo $i
done
```

```smalltalk
# Trashtalk DSL
method: printOneToTen [
  1 to: 10 do: [:i |
    @ Transcript print: i
  ]
]
```

---

## Conditional Patterns

### Bash: if-then
```bash
# Bash
if [[ $value -gt 0 ]]; then
  echo "positive"
fi
```

```smalltalk
# Trashtalk DSL
method: checkPositive [
  (value > 0) ifTrue: [
    @ Transcript print: 'positive'
  ]
]
```

### Bash: if-then-else
```bash
# Bash
if [[ $x -eq $y ]]; then
  echo "equal"
else
  echo "not equal"
fi
```

```smalltalk
# Trashtalk DSL
method: compareXY [
  (x = y) ifTrue: [
    @ Transcript print: 'equal'
  ] ifFalse: [
    @ Transcript print: 'not equal'
  ]
]
```

### Bash: compound conditions (AND)
```bash
# Bash
if [[ $a -gt 0 && $b -gt 0 ]]; then
  echo "both positive"
fi
```

```smalltalk
# Trashtalk DSL
method: checkBothPositive [
  (a > 0) and: [b > 0] ifTrue: [
    @ Transcript print: 'both positive'
  ]
]
```

### Bash: compound conditions (OR)
```bash
# Bash
if [[ $status = "done" || $status = "complete" ]]; then
  echo "finished"
fi
```

```smalltalk
# Trashtalk DSL
method: checkFinished [
  (status = 'done') or: [status = 'complete'] ifTrue: [
    @ Transcript print: 'finished'
  ]
]
```

### Bash: negation
```bash
# Bash
if [[ ! -z "$value" ]]; then
  process "$value"
fi
```

```smalltalk
# Trashtalk DSL
method: processIfNotEmpty [
  (value notEmpty) ifTrue: [
    @ self process: value
  ]
]
```

---

## String Patterns

### Bash: empty string check
```bash
# Bash
if [[ -z "$str" ]]; then
  echo "empty"
fi
```

```smalltalk
# Trashtalk DSL
method: checkEmpty [
  (str isEmpty) ifTrue: [
    @ Transcript print: 'empty'
  ]
]
```

### Bash: non-empty string check
```bash
# Bash
if [[ -n "$str" ]]; then
  process "$str"
fi
```

```smalltalk
# Trashtalk DSL
method: processIfPresent [
  (str notEmpty) ifTrue: [
    @ self process: str
  ]
]
```

### Bash: string comparison
```bash
# Bash
if [[ "$a" = "$b" ]]; then
  echo "match"
fi
```

```smalltalk
# Trashtalk DSL
method: compareStrings [
  (a = b) ifTrue: [
    @ Transcript print: 'match'
  ]
]
```

### Bash: pattern matching
```bash
# Bash
if [[ "$str" =~ ^[0-9]+$ ]]; then
  echo "is number"
fi
```

```smalltalk
# Trashtalk DSL
method: checkIsNumber [
  (str matches: '^[0-9]+$') ifTrue: [
    @ Transcript print: 'is number'
  ]
]
```

---

## Array/Collection Patterns

### Bash: get array length
```bash
# Bash
len=${#array[@]}
```

```smalltalk
# Trashtalk DSL
method: getLength [
  | len |
  len := items arrayLength.
  ^ len
]
```

### Bash: get element at index
```bash
# Bash
value="${array[$i]}"
```

```smalltalk
# Trashtalk DSL
method: getAt: index [
  ^ items arrayAt: index
]
```

### Bash: set element at index
```bash
# Bash
array[$i]="$value"
```

```smalltalk
# Trashtalk DSL
method: at: index put: value [
  items := items arrayAt: index put: value
]
```

### Bash: append to array
```bash
# Bash
array+=("$value")
```

```smalltalk
# Trashtalk DSL
method: add: value [
  items := items arrayPush: value
]
```

### Bash: transform array (map)
```bash
# Bash
result=()
for item in "${array[@]}"; do
  result+=("$(transform "$item")")
done
```

```smalltalk
# Trashtalk DSL
method: transformAll [
  ^ @ items collect: [:item |
    @ self transform: item
  ]
]
```

### Bash: filter array
```bash
# Bash
result=()
for item in "${array[@]}"; do
  if [[ $(test "$item") = "true" ]]; then
    result+=("$item")
  fi
done
```

```smalltalk
# Trashtalk DSL
method: filterValid [
  ^ @ items select: [:item |
    @ self isValid: item
  ]
]
```

### Bash: reduce/fold array
```bash
# Bash
sum=0
for item in "${array[@]}"; do
  sum=$((sum + item))
done
```

```smalltalk
# Trashtalk DSL
method: sum [
  ^ @ items inject: 0 into: [:acc :item |
    acc + item
  ]
]
```

---

## Instance Variable Patterns

### Bash: get instance variable
```bash
# Bash (in rawMethod)
value="$(_ivar myVar)"
```

```smalltalk
# Trashtalk DSL - automatic inference
method: getValue [
  ^ myVar + 0
]
# Compiles to: echo "$(_ivar myVar)"
```

### Bash: set instance variable
```bash
# Bash (in rawMethod)
_ivar_set myVar "$newValue"
```

```smalltalk
# Trashtalk DSL - automatic inference
method: setValue: val [
  myVar := val + 0
]
# Compiles to: _ivar_set myVar "$val"
```

---

## Block Patterns

### Bash: callback/closure
```bash
# Bash - complex, requires eval
eval "$callback \"$arg\""
```

```smalltalk
# Trashtalk DSL
method: executeWith: arg [
  @ callback valueWith: arg
]
```

### Bash: two-argument callback
```bash
# Bash
eval "$callback \"$arg1\" \"$arg2\""
```

```smalltalk
# Trashtalk DSL
method: executeWithBoth: a and: b [
  @ callback valueWith: a and: b
]
```

---

## Return Value Patterns

### Bash: return string
```bash
# Bash
echo "$result"
return
```

```smalltalk
# Trashtalk DSL
method: getResult [
  ^ result
]
```

### Bash: return computed value
```bash
# Bash
echo "$((a + b))"
return
```

```smalltalk
# Trashtalk DSL
method: addAB [
  ^ a + b
]
```

---

## When rawMethod is Required

Some patterns cannot be expressed in pure Trashtalk and require `rawMethod:`:

1. **Heredocs**: Multi-line string literals
2. **File I/O redirection**: `>`, `>>`, `<`
3. **Process substitution**: `<(...)`, `>(...)`
4. **Complex pipes**: Multi-stage pipelines
5. **Trap handlers**: Signal handling
6. **Direct shell features**: `eval`, `exec`, complex quoting

Example:
```smalltalk
rawMethod: writeToFile: path contents: data [
  local path="$1" data="$2"
  printf '%s' "$data" > "$path"
]
```

---

## Compilation Status

| Pattern | Pure Trashtalk | Native Compilation |
|---------|---------------|-------------------|
| whileTrue:/whileFalse: | ✓ | ✓ |
| timesRepeat: | ✓ | ✓ |
| to:do: | ✓ | ✓ |
| ifTrue:/ifFalse: | ✓ | ✓ |
| and:/or: | ✓ | ✓ |
| isEmpty/notEmpty | ✓ | ✓ |
| matches: | ✓ | ✓ |
| arrayAt:/arrayPush: | ✓ | ✓ |
| do:/collect:/select: | ✓ | ✓ |
| Block valueWith: | ✓ | ✓ |
| Ivar access/assignment | ✓ | ✓ |
| File I/O | rawMethod | ✗ |
| Heredocs | rawMethod | ✗ |
| Process substitution | rawMethod | ✗ |

---

## Summary

To maximize native compilation:

1. **Use `method:`** instead of `rawMethod:` when possible
2. **Use DSL control flow**: `whileTrue:`, `ifTrue:`, `timesRepeat:`
3. **Use Array methods**: `do:`, `collect:`, `select:`, `inject:into:`
4. **Use string predicates**: `isEmpty`, `notEmpty`, `matches:`
5. **Let the compiler infer ivars**: Just use the variable name directly
6. **Use blocks for callbacks**: `[:arg | ...]` syntax

Methods following these patterns can be natively compiled by Procyon,
resulting in faster execution and smaller memory footprint.
