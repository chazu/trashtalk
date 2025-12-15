# ==============================================================================
# PEG Combinators for Trashtalk Parser
# ==============================================================================
#
# This module provides core parsing expression grammar (PEG) combinators
# for building the Trashtalk parser. Each combinator is a jq filter that
# transforms parser state.
#
# Parser State Structure:
#   {
#     "tokens": [...],     # Array of token objects from tokenizer
#     "pos": 0,            # Current position in token array
#     "result": null,      # Result of last successful parse
#     "errors": []         # Accumulated parse errors
#   }
#
# Each combinator either:
#   - Succeeds: advances pos and sets result
#   - Fails: leaves pos unchanged and sets result to null
#
# Usage:
#   Import this module and chain combinators:
#   include "peg/combinators";
#   . | token("IDENTIFIER") | map(.value)
#
# ==============================================================================

# ------------------------------------------------------------------------------
# Basic Accessors
# ------------------------------------------------------------------------------

# Get the current token (or null if at end)
def current:
  if .pos < (.tokens | length) then
    .tokens[.pos]
  else
    null
  end;

# Check if at end of token stream
def eof:
  .pos >= (.tokens | length);

# Advance position by one token
def advance:
  .pos += 1;

# ------------------------------------------------------------------------------
# Result Management
# ------------------------------------------------------------------------------

# Set the parse result
def succeed($v):
  .result = $v;

# Mark parse as failed with error message
def fail($msg):
  .result = null |
  .errors += [{
    message: $msg,
    line: (current.line // "EOF"),
    col: (current.col // 0)
  }];

# Clear previous errors (used in choice when alternative succeeds)
def clearErrors:
  .errors = [];

# Transform the result if parse succeeded
def map(f):
  if .result != null then
    .result |= f
  else
    .
  end;

# ------------------------------------------------------------------------------
# Core Combinators
# ------------------------------------------------------------------------------

# Match a specific token type
# Example: token("IDENTIFIER") matches any identifier token
def token($t):
  if current.type == $t then
    succeed(current) | advance
  else
    fail("expected \($t), got \(current.type // "EOF")")
  end;

# Match a token with specific value
# Example: literal("subclass:") matches the keyword "subclass:"
def literal($v):
  if current.value == $v then
    succeed(current) | advance
  else
    fail("expected '\($v)', got '\(current.value // "EOF")'")
  end;

# Match a token type with specific value
# Example: tokenValue("KEYWORD"; "method:") matches keyword "method:"
def tokenValue($t; $v):
  if current.type == $t and current.value == $v then
    succeed(current) | advance
  else
    fail("expected \($t) '\($v)', got \(current.type // "EOF") '\(current.value // "")'")
  end;

# Sequence: run two parsers in order, return array of results
# Example: seq(token("IDENTIFIER"); literal("subclass:"))
def seq($p1; $p2):
  . as $initial |
  ($p1) as $r1 |
  if $r1.result != null then
    $r1 | ($p2) as $r2 |
    if $r2.result != null then
      $r2 | succeed([$r1.result, $r2.result])
    else
      $r2
    end
  else
    $r1
  end;

# Sequence of three parsers
def seq3($p1; $p2; $p3):
  seq($p1; $p2) as $r12 |
  if $r12.result != null then
    $r12 | ($p3) as $r3 |
    if $r3.result != null then
      $r3 | succeed($r12.result + [$r3.result])
    else
      $r3
    end
  else
    $r12
  end;

# Choice: try parsers in order, return first success (PEG ordered choice)
# This is the key PEG feature - no backtracking after success
def choice($parsers):
  . as $initial |
  reduce $parsers[] as $p (
    {found: false, state: ($initial | .errors = [])};
    if .found then
      .
    else
      ($initial | .errors = [] | $p) as $result |
      if $result.result != null then
        {found: true, state: $result}
      else
        .
      end
    end
  ) |
  if .found then
    .state
  else
    $initial | fail("no alternative matched")
  end;

# Choice of two (convenience)
def choice2($p1; $p2):
  choice([$p1, $p2]);

# Choice of three (convenience)
def choice3($p1; $p2; $p3):
  choice([$p1, $p2, $p3]);

# Zero or more repetitions
# Returns array of results (possibly empty)
def many($p):
  . as $initial |
  def go:
    ($p) as $r |
    if $r.result != null then
      $r | .result as $v | go | .result = [$v] + .result
    else
      $initial | .pos = $r.pos | succeed([])  # Reset errors, keep position
    end;
  go;

# One or more repetitions
# Fails if no matches; returns array of at least one result
def many1($p):
  ($p) as $first |
  if $first.result != null then
    $first | .result as $v | many($p) | .result = [$v] + .result
  else
    fail("expected at least one match")
  end;

# Optional: zero or one
# Returns result or null (never fails)
def optional($p):
  . as $initial |
  ($p) as $r |
  if $r.result != null then
    $r
  else
    $initial | succeed(null)
  end;

# Lookahead: succeed if parser matches, but don't consume input
def lookahead($p):
  . as $initial |
  ($p) as $r |
  if $r.result != null then
    $initial | succeed(true)
  else
    $initial | succeed(false)
  end;

# Negative lookahead: succeed if parser does NOT match
def notFollowedBy($p):
  . as $initial |
  ($p) as $r |
  if $r.result != null then
    $initial | fail("unexpected match")
  else
    $initial | succeed(true)
  end;

# ------------------------------------------------------------------------------
# Whitespace Handling
# ------------------------------------------------------------------------------

# Skip all NEWLINE tokens
def skipNewlines:
  if current.type == "NEWLINE" then
    advance | skipNewlines
  else
    .
  end;

# Skip newlines and return state (doesn't set result)
def ws:
  skipNewlines;

# ------------------------------------------------------------------------------
# Utility Combinators
# ------------------------------------------------------------------------------

# Parse between delimiters (e.g., brackets)
# Example: between(literal("["); literal("]"); methodBody)
def between($open; $close; $content):
  ($open) as $o |
  if $o.result != null then
    $o | ($content) as $c |
    if $c.result != null then
      $c | .result as $inner | ($close) as $cl |
      if $cl.result != null then
        $cl | succeed($inner)
      else
        $cl
      end
    else
      $c
    end
  else
    $o
  end;

# Parse separated by delimiter (e.g., comma-separated)
# Example: sepBy(token("IDENTIFIER"); literal(","))
def sepBy($item; $sep):
  optional(
    ($item) as $first |
    if $first.result != null then
      $first | .result as $v |
      many(($sep) | ($item)) |
      .result = [$v] + .result
    else
      succeed([])
    end
  ) |
  if .result == null then succeed([]) else . end;

# Chain: parse one or more items with a separator
# Returns array of items (not separators)
def sepBy1($item; $sep):
  ($item) as $first |
  if $first.result != null then
    $first | .result as $v |
    many(($sep) | ($item)) |
    .result = [$v] + .result
  else
    $first
  end;

# Try a parser, reset position on failure
def try($p):
  . as $initial |
  ($p) as $r |
  if $r.result != null then
    $r
  else
    $initial | .result = null
  end;

# Expect: like token but with better error messages
def expect($t; $context):
  if current.type == $t then
    succeed(current) | advance
  else
    fail("expected \($t) in \($context), got \(current.type // "EOF")")
  end;

# ==============================================================================
# End of combinators.jq
# ==============================================================================
