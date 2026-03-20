#!/usr/bin/env bash
set -euo pipefail

echo "=== Formatting ==="
nix fmt

echo ""
echo "=== Build (-Wall -Werror) ==="
nix develop --command cabal build --project-file=cabal-local.project --ghc-options="-Wall -Werror"

echo ""
echo "=== Tests (-Wall -Werror) ==="
nix develop --command cabal test --project-file=cabal-local.project --ghc-options="-Wall -Werror"

echo ""
echo "=== All Checks Passed ==="
