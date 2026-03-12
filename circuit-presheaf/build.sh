#!/bin/bash
# Build circuit-presheaf using the progressive Idris2 compiler
set -e

IDRIS2_DIR="/home/natanh/Idris2"
export PATH="/home/natanh/chez/bin:$IDRIS2_DIR/build/exec/idris2_app:$PATH"
export IDRIS2_INC_SRC="$IDRIS2_DIR/build/exec/idris2_app"

IDRIS2="$IDRIS2_DIR/build/exec/idris2"

cd "$(dirname "$0")"

echo "=== Building circuit-presheaf ==="
$IDRIS2 --build circuit-presheaf.ipkg

echo "=== Build succeeded ==="
