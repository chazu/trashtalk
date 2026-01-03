#!/usr/bin/env bash
# Yutani Demo Script - tests YutaniSession class

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TRASHTALK_ROOT="${SCRIPT_DIR}/.."

echo "=== Yutani Demo ==="
echo ""

# Check prerequisites
echo "Checking prerequisites..."

if ! command -v grpcurl &>/dev/null; then
  echo "  ERROR: grpcurl not found"
  echo "  Install with: go install github.com/fullstorydev/grpcurl/cmd/grpcurl@latest"
  exit 1
fi
echo "  grpcurl: OK"

if ! command -v jo &>/dev/null; then
  echo "  ERROR: jo not found"
  echo "  Install with: brew install jo"
  exit 1
fi
echo "  jo: OK"

if ! grpcurl -plaintext localhost:7755 list &>/dev/null; then
  echo "  ERROR: Yutani server not running on localhost:7755"
  echo "  Start with: ~/dev/go/yutani/bin/yutani-server"
  exit 1
fi
echo "  yutani server: OK"
echo ""

# Load Trashtalk
echo "Loading Trashtalk runtime..."
source "${TRASHTALK_ROOT}/lib/trash.bash"
echo ""

# Connect
echo "Connecting to Yutani..."
session=$(@ YutaniSession connect 2>&1 | grep -v "^\[INFO\]" | tail -1)
echo "  Session: $session"

if [[ -z "$session" || "$session" == *"error"* || "$session" == *"Error"* ]]; then
  echo "  ERROR: Failed to connect"
  exit 1
fi
echo ""

# Get server info
echo "Getting server info..."
info=$(@ "$session" serverInfo)
echo "  Version: $(echo "$info" | jq -r '.version // "unknown"')"
echo "  Screen: $(echo "$info" | jq -r '.screenSize.width')x$(echo "$info" | jq -r '.screenSize.height')"
echo ""

# Create a list widget
echo "Creating list widget..."
list_id=$(@ "$session" createList: 'Trashtalk Classes')
echo "  List ID: $list_id"
echo ""

# Add items
echo "Adding items..."
for class in Array Block Counter Dictionary File Object Process; do
  @ "$session" addItem: "$class" to: "$list_id" >/dev/null
  echo "  Added: $class"
done
echo ""

# Display it
echo "Displaying list..."
@ "$session" setRoot: "$list_id" >/dev/null
echo "  List is now visible on the Yutani server!"
echo ""

# Wait
echo "Press Enter to disconnect (or wait 10 seconds)..."
read -t 10 || true
echo ""

# Cleanup
echo "Disconnecting..."
@ "$session" disconnect >/dev/null
echo "  Done!"
echo ""
echo "=== Demo Complete ==="
