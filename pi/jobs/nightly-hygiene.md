You are running as an unattended nightly job. Be concise. Perform ONLY
read-only checks — never modify, push, commit, or delete anything.

Produce a short "Nightly hygiene report" covering:

1. **Backups**: run
   `export RESTIC_REPOSITORY="sftp:storagebox:restic-home" RESTIC_PASSWORD_FILE=~/.secrets/RESTIC_PASSWORD && restic snapshots --latest 1 --json`
   and report the age of the newest snapshot (warn if older than 26 hours).
   Check `tail -5 /tmp/restic-backup.log` for "Fatal" and verify
   `launchctl list | grep restic` shows the hourly job loaded.
2. **Git hygiene**: for each of ~/code/dotfiles, ~/code/pm-quant,
   ~/code/yuppielife, ~/code/vllm-lens, ~/code/le-gpt.el, ~/code/pi,
   ~/code/flights-mcp, ~/.emacs.d/straight/repos/pimacs.el —
   report ONLY repos with uncommitted changes (`git status --porcelain`)
   or unpushed commits (`git log @{u}..HEAD --oneline`), one line each.
3. **Disk**: warn if the Data volume is >85% used
   (`df -h /System/Volumes/Data`).

Format: start the report with "⚠️" as the very first character if ANY
check needs attention, otherwise start with "✅". Keep the whole report
under 15 lines. No preamble, just the report.
