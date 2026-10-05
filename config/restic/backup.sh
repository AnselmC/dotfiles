#!/usr/bin/env bash
# Hourly home backup to Hetzner storage box via restic.
set -euo pipefail
export RESTIC_REPOSITORY="sftp:storagebox:restic-home"
export RESTIC_PASSWORD_FILE="$HOME/.secrets/RESTIC_PASSWORD"
RESTIC=/opt/homebrew/bin/restic
LOG=/tmp/restic-backup.log
{
  echo "=== backup $(date)"
  "$RESTIC" backup "$HOME" \
    --exclude-file="$HOME/code/dotfiles/config/restic/excludes.txt" \
    --exclude-caches --one-file-system
  echo "=== forget/prune $(date)"
  "$RESTIC" forget --keep-hourly 24 --keep-daily 30 --keep-weekly 8 --keep-monthly 12 --prune
} >> "$LOG" 2>&1
