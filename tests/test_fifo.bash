#!/usr/bin/env bash
# Test FIFO class - TDD

cd ~/.trashtalk
source lib/trash.bash

TESTS_RUN=0
ASSERTIONS_PASSED=0
ASSERTIONS_FAILED=0

pass() {
  ((ASSERTIONS_PASSED++))
  echo "  ✓ $1"
}

fail() {
  ((ASSERTIONS_FAILED++))
  echo "  ✗ $1: $2"
}

test_start() {
  ((TESTS_RUN++))
  echo "Testing: $1"
}

# =============================================================================
# Test: FIFO creation
# =============================================================================
test_start "FIFO can be created with a path"
fifo=$(@ FIFO at: "/tmp/test_fifo_$$")
if [[ -n "$fifo" ]]; then
  pass "FIFO instance created"
else
  fail "FIFO instance created" "got empty instance"
fi

# =============================================================================
# Test: FIFO path accessor
# =============================================================================
test_start "FIFO returns its path"
path=$(@ "$fifo" path)
if [[ "$path" == "/tmp/test_fifo_$$" ]]; then
  pass "path returns correct value"
else
  fail "path returns correct value" "got '$path'"
fi

# =============================================================================
# Test: FIFO create actually creates the named pipe
# =============================================================================
test_start "FIFO create creates named pipe on disk"
@ "$fifo" create
if [[ -p "/tmp/test_fifo_$$" ]]; then
  pass "named pipe exists on disk"
else
  fail "named pipe exists on disk" "file is not a pipe or doesn't exist"
fi

# =============================================================================
# Test: FIFO exists returns true after create
# =============================================================================
test_start "FIFO exists returns true after create"
exists=$(@ "$fifo" exists)
if [[ "$exists" == "true" ]]; then
  pass "exists returns true"
else
  fail "exists returns true" "got '$exists'"
fi

# =============================================================================
# Test: FIFO remove deletes the pipe
# =============================================================================
test_start "FIFO remove deletes the pipe"
@ "$fifo" remove
if [[ ! -e "/tmp/test_fifo_$$" ]]; then
  pass "pipe removed from disk"
else
  fail "pipe removed from disk" "file still exists"
fi

# =============================================================================
# Test: FIFO exists returns false after remove
# =============================================================================
test_start "FIFO exists returns false after remove"
exists=$(@ "$fifo" exists)
if [[ "$exists" == "false" ]]; then
  pass "exists returns false"
else
  fail "exists returns false" "got '$exists'"
fi

# =============================================================================
# Test: FIFO open/write/read cycle
# =============================================================================
test_start "FIFO can write and read a line"
fifo2=$(@ FIFO at: "/tmp/test_fifo2_$$")
@ "$fifo2" create

# Open must be called first to avoid blocking
@ "$fifo2" open

# Write in background (writer won't block since we opened read-write)
@ "$fifo2" writeLine: "hello world" &
write_pid=$!

# Read the line
result=$(@ "$fifo2" readLine)
wait $write_pid 2>/dev/null

if [[ "$result" == "hello world" ]]; then
  pass "read/write cycle works"
else
  fail "read/write cycle works" "got '$result'"
fi

@ "$fifo2" close
@ "$fifo2" remove

# =============================================================================
# Test: FIFO readLine with timeout
# =============================================================================
test_start "FIFO readLine with timeout returns empty on no data"
fifo3=$(@ FIFO at: "/tmp/test_fifo3_$$")
@ "$fifo3" create
@ "$fifo3" open

# Read with short timeout, nothing written
result=$(@ "$fifo3" readLineTimeout: 1)
if [[ -z "$result" ]]; then
  pass "readLineTimeout returns empty when no data"
else
  fail "readLineTimeout returns empty when no data" "got '$result'"
fi

@ "$fifo3" close
@ "$fifo3" remove

# =============================================================================
# Test: FIFO readLinesDo reads multiple lines with handler
# =============================================================================
test_start "FIFO readLinesDo: calls handler for each line"
fifo4=$(@ FIFO at: "/tmp/test_fifo4_$$")
@ "$fifo4" create
@ "$fifo4" open

# Use a file to track lines (works across subshells)
trackfile="/tmp/test_fifo_track_$$"
rm -f "$trackfile"

# Write 3 lines in background, then close
(
  p=$(@ "$fifo4" path)
  echo "line1" > "$p"
  echo "line2" > "$p"
  echo "line3" > "$p"
) &
writer_pid=$!

# Handler appends each line to the trackfile
handler='echo "$__FIFO_LINE" >> "'"$trackfile"'"'

# Read in background (will block waiting for input)
@ "$fifo4" readLinesDo: "$handler" &
reader_pid=$!

# Wait for writer to finish and reader to process
wait $writer_pid 2>/dev/null
sleep 0.5
kill $reader_pid 2>/dev/null
wait $reader_pid 2>/dev/null

# Check the trackfile
if [[ -f "$trackfile" ]]; then
  linecount=$(wc -l < "$trackfile" | tr -d ' ')
  firstline=$(sed -n '1p' "$trackfile")
  secondline=$(sed -n '2p' "$trackfile")
  thirdline=$(sed -n '3p' "$trackfile")

  if [[ "$linecount" -ge 3 && "$firstline" == "line1" && "$secondline" == "line2" && "$thirdline" == "line3" ]]; then
    pass "readLinesDo called handler for each line"
  else
    fail "readLinesDo called handler for each line" "got $linecount lines: $firstline, $secondline, $thirdline"
  fi
else
  fail "readLinesDo called handler for each line" "trackfile not created"
fi

rm -f "$trackfile"
@ "$fifo4" close
@ "$fifo4" remove

# =============================================================================
# Test: startWriter spawns a command writing to the FIFO
# =============================================================================
test_start "startWriter: spawns command and writerPid returns its PID"
fifo5=$(@ FIFO at: "/tmp/test_fifo5_$$")
@ "$fifo5" create
@ "$fifo5" open

# Start a writer that outputs lines with delays (long enough to check PID)
@ "$fifo5" startWriter: 'echo line1; sleep 0.5; echo line2; sleep 0.5; echo line3; sleep 1'

# Check we got a non-empty PID (process may or may not still be running)
wpid=$(@ "$fifo5" writerPid)
if [[ -n "$wpid" && "$wpid" =~ ^[0-9]+$ ]]; then
  pass "writerPid returns valid PID"
else
  fail "writerPid returns valid PID" "got '$wpid'"
fi

# Read the lines
line1=$(@ "$fifo5" readLine)
line2=$(@ "$fifo5" readLine)
line3=$(@ "$fifo5" readLine)

if [[ "$line1" == "line1" && "$line2" == "line2" && "$line3" == "line3" ]]; then
  pass "startWriter output readable via readLine"
else
  fail "startWriter output readable via readLine" "got '$line1', '$line2', '$line3'"
fi

@ "$fifo5" stopWriter
@ "$fifo5" close
@ "$fifo5" remove

# =============================================================================
# Test: stopWriter kills the writer process
# =============================================================================
test_start "stopWriter kills the writer process"
fifo6=$(@ FIFO at: "/tmp/test_fifo6_$$")
@ "$fifo6" create
@ "$fifo6" open

# Start a long-running writer
@ "$fifo6" startWriter: 'while true; do echo tick; sleep 1; done'
wpid=$(@ "$fifo6" writerPid)

# Verify it's running
if kill -0 "$wpid" 2>/dev/null; then
  @ "$fifo6" stopWriter
  sleep 0.2
  if kill -0 "$wpid" 2>/dev/null; then
    fail "stopWriter kills the process" "process still running"
  else
    pass "stopWriter kills the process"
  fi
else
  fail "stopWriter kills the process" "writer never started"
fi

@ "$fifo6" close
@ "$fifo6" remove

# =============================================================================
# Test: startReader spawns background reader with handler
# =============================================================================
test_start "startReader: spawns background reader calling handler"
fifo7=$(@ FIFO at: "/tmp/test_fifo7_$$")
@ "$fifo7" create
@ "$fifo7" open

trackfile7="/tmp/test_fifo_track7_$$"
rm -f "$trackfile7"

# Start reader with handler that appends to trackfile
@ "$fifo7" startReader: 'echo "$__FIFO_LINE" >> "'"$trackfile7"'"'

rpid=$(@ "$fifo7" readerPid)
if [[ -n "$rpid" && "$rpid" =~ ^[0-9]+$ ]]; then
  pass "readerPid returns valid PID"
else
  fail "readerPid returns valid PID" "got '$rpid'"
fi

# Write some lines (background to avoid blocking)
(
  p=$(@ "$fifo7" path)
  echo "async1" > "$p"
  echo "async2" > "$p"
  echo "async3" > "$p"
) &
wait $!
sleep 0.3

# Check trackfile
if [[ -f "$trackfile7" ]]; then
  linecount=$(wc -l < "$trackfile7" | tr -d ' ')
  if [[ "$linecount" -ge 3 ]]; then
    pass "startReader handler received all lines"
  else
    fail "startReader handler received all lines" "got $linecount lines"
  fi
else
  fail "startReader handler received all lines" "trackfile not created"
fi

@ "$fifo7" stopReader
rm -f "$trackfile7"
@ "$fifo7" close
@ "$fifo7" remove

# =============================================================================
# Test: stopReader kills the reader process
# =============================================================================
test_start "stopReader kills the reader process"
fifo8=$(@ FIFO at: "/tmp/test_fifo8_$$")
@ "$fifo8" create
@ "$fifo8" open

@ "$fifo8" startReader: 'true'
rpid=$(@ "$fifo8" readerPid)

if kill -0 "$rpid" 2>/dev/null; then
  @ "$fifo8" stopReader
  sleep 0.2
  if kill -0 "$rpid" 2>/dev/null; then
    fail "stopReader kills the process" "process still running"
  else
    pass "stopReader kills the process"
  fi
else
  fail "stopReader kills the process" "reader never started"
fi

@ "$fifo8" close
@ "$fifo8" remove

# =============================================================================
# Test: Integration - startWriter and startReader together
# =============================================================================
test_start "startWriter and startReader work together"
fifo9=$(@ FIFO at: "/tmp/test_fifo9_$$")
@ "$fifo9" create
@ "$fifo9" open

trackfile9="/tmp/test_fifo_track9_$$"
rm -f "$trackfile9"

# Start reader first
@ "$fifo9" startReader: 'echo "$__FIFO_LINE" >> "'"$trackfile9"'"'

# Start writer
@ "$fifo9" startWriter: 'for i in 1 2 3; do echo "msg$i"; sleep 0.1; done'

# Wait for writer to finish
sleep 0.5

# Check results
if [[ -f "$trackfile9" ]]; then
  lines=$(wc -l < "$trackfile9" | tr -d ' ')
  if [[ "$lines" -ge 3 ]]; then
    pass "reader received all messages from writer"
  else
    fail "reader received all messages from writer" "only got $lines lines"
  fi
else
  fail "reader received all messages from writer" "trackfile not created"
fi

@ "$fifo9" stopReader
@ "$fifo9" stopWriter
rm -f "$trackfile9"
@ "$fifo9" close
@ "$fifo9" remove

# =============================================================================
# Cleanup
# =============================================================================
rm -f "/tmp/test_fifo_$$" "/tmp/test_fifo2_$$" "/tmp/test_fifo3_$$" "/tmp/test_fifo4_$$" 2>/dev/null
rm -f "/tmp/test_fifo5_$$" "/tmp/test_fifo6_$$" "/tmp/test_fifo7_$$" "/tmp/test_fifo8_$$" "/tmp/test_fifo9_$$" 2>/dev/null
pkill -f "sleep 86400" 2>/dev/null

# =============================================================================
# Summary
# =============================================================================
echo ""
echo "========================================="
echo "Tests: $TESTS_RUN, Assertions: $ASSERTIONS_PASSED passed, $ASSERTIONS_FAILED failed"
if [[ $ASSERTIONS_FAILED -eq 0 ]]; then
  echo "All tests passed!"
  exit 0
else
  echo "Some tests failed!"
  exit 1
fi
