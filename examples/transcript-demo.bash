#!/usr/bin/env bash
# Transcript Demo - opens a Trashtalk transcript in Yutani

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TRASHTALK_ROOT="${SCRIPT_DIR}/.."

echo "=== Trashtalk Transcript ==="
echo ""

# Check prerequisites
if ! command -v grpcurl &>/dev/null; then
  echo "ERROR: grpcurl not found"
  echo "Install with: go install github.com/fullstorydev/grpcurl/cmd/grpcurl@latest"
  exit 1
fi

if ! grpcurl -plaintext localhost:7755 list &>/dev/null; then
  echo "ERROR: Yutani server not running on localhost:7755"
  echo "Start with: ~/dev/go/yutani/bin/yutani-server"
  exit 1
fi

echo "Loading Trashtalk..."
source "${TRASHTALK_ROOT}/lib/trash.bash"
echo ""

echo "Usage:"
echo "  - Type Trashtalk code in the input field at the bottom"
echo "  - Press Enter to evaluate"
echo "  - Results appear in the output panel above"
echo "  - Press Ctrl+C to exit"
echo ""
echo "Example expressions to try:"
echo "  @ Counter new"
echo "  @ Trash info"
echo "  @ Trash listClasses"
echo ""
echo "Opening Transcript..."
echo ""

@ Transcript open
