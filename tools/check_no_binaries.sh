#!/usr/bin/env bash
set -euo pipefail

matches="$(git ls-files | grep -E '(^src/|^inst/).*\.(o|so|dll|dylib|a)$' || true)"

if [[ -n "$matches" ]]; then
  echo "Tracked compiled artefacts are not allowed:"
  echo "$matches"
  exit 1
fi

echo "OK: no tracked compiled artefacts under src/ or inst/"
