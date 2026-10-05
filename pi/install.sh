#!/usr/bin/env bash
# Symlink tracked pi config into ~/.pi/agent. Idempotent; never deletes —
# existing non-symlink targets are moved aside to <name>.bak-<timestamp>.
set -euo pipefail
SRC="$(cd "$(dirname "$0")/agent" && pwd)"
DST="${PI_AGENT_DIR:-$HOME/.pi/agent}"
ITEMS=(settings.json presets.json extensions prompts/plan.md)

for item in "${ITEMS[@]}"; do
  src="$SRC/$item"; dst="$DST/$item"
  mkdir -p "$(dirname "$dst")"
  if [ -L "$dst" ] && [ "$(readlink "$dst")" = "$src" ]; then
    echo "ok       $item"; continue
  fi
  if [ -e "$dst" ] || [ -L "$dst" ]; then
    bak="$dst.bak-$(date +%Y%m%d%H%M%S)"
    mv "$dst" "$bak"; echo "backup   $item -> $(basename "$bak")"
  fi
  ln -s "$src" "$dst"; echo "linked   $item"
done
