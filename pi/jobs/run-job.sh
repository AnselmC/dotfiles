#!/usr/bin/env bash
# Run a scheduled pi job: headless pi -p with the job's prompt file,
# log + auditable session, notification via ntfy (if ~/.secrets/NTFY_TOPIC
# exists) or macOS notification otherwise.
set -uo pipefail
JOB="${1:?usage: run-job.sh <job-name>}"
JOBS_DIR="$HOME/code/dotfiles/pi/jobs"
PROMPT_FILE="$JOBS_DIR/$JOB.md"
[ -f "$PROMPT_FILE" ] || {
	echo "no such job: $JOB" >&2
	exit 1
}

# launchd provides no login-shell PATH: node via nvm default alias + homebrew
NVM_DEFAULT="$(cat "$HOME/.nvm/alias/default" 2>/dev/null || true)"
export PATH="$HOME/.nvm/versions/node/v$NVM_DEFAULT/bin:$HOME/.local/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin"

LOG_DIR="$HOME/.pi/agent/job-logs"
SESSION_DIR="$HOME/.pi/agent/job-sessions"
mkdir -p "$LOG_DIR" "$SESSION_DIR"
LOG="$LOG_DIR/$JOB.log"

PROMPT="Current date/time: $(date '+%Y-%m-%d %H:%M %Z')

$(cat "$PROMPT_FILE")"
OUT="$(pi -p --session-dir "$SESSION_DIR" "$PROMPT" 2>>"$LOG")"
STATUS=$?
{
	echo "=== $JOB $(date) exit=$STATUS"
	echo "$OUT"
	echo
} >>"$LOG"

TITLE="pi job: $JOB"
[ "$STATUS" -ne 0 ] && TITLE="FAILED pi job: $JOB (exit $STATUS)"
SUMMARY="$(printf '%s' "$OUT" | head -c 900)"
if [ -s "$HOME/.secrets/NTFY_TOPIC" ]; then
	curl -s -m 10 -H "Title: $TITLE" -d "$SUMMARY" \
		"https://ntfy.sh/$(cat "$HOME/.secrets/NTFY_TOPIC")" >/dev/null || true
else
	/usr/bin/osascript -e 'on run argv' \
		-e 'display notification (item 1 of argv) with title (item 2 of argv)' \
		-e 'end run' "$SUMMARY" "$TITLE" 2>/dev/null || true
fi
printf '%s\n' "$OUT"
exit "$STATUS"
