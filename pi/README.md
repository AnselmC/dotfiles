# pi config

Tracked config for the [pi](https://github.com/earendil-works/pi) coding agent. `./install.sh` symlinks it into `~/.pi/agent`.

| Path | What |
|------|------|
| `agent/settings.json` | default model, packages, theme, compaction |
| `agent/presets.json` | `/preset` bundles: quick, deep, plan, review |
| `agent/extensions/guardrails.ts` | confirm destructive bash / sensitive file edits |
| `agent/extensions/git-checkpoint.ts` | per-prompt working-tree snapshots, `/checkpoints`, restore on `/fork` |
| `agent/extensions/notify.ts` | macOS notification when long runs finish / wait for input |
| `agent/extensions/handoff.ts` | `/handoff <goal>` — fresh session with generated context prompt |
| `agent/extensions/preset.ts` | `/preset`, `--preset`, Ctrl+Shift+U |
| `agent/prompts/plan.md` | `/plan` prompt template |

Not tracked (secrets / employer-internal / machine state): `auth.json`, `mcp.json`, `mcp-oauth/`, `sessions/`, work skills & prompts.
Machine-local extensions: name them `*.local.ts` in `agent/extensions/` (gitignored).
