/**
 * Git Checkpoint
 *
 * Before each prompt runs, snapshots the full working tree (tracked + untracked,
 * respecting .gitignore) into a dangling commit — without touching your index,
 * stash, branch, or HEAD. Commit is pinned under refs/pi-checkpoints/<session>/
 * so gc won't collect it; refs older than 14 days are pruned on session start.
 *
 * - /fork (before a message): offers to restore code to the state before that message
 * - /checkpoints: pick any checkpoint on the current branch and restore it
 *
 * Restore = working tree only: files are rewritten to the snapshot, files created
 * since are deleted. Index is left alone. Current state is snapshotted first, so
 * restore itself is undoable via /checkpoints.
 *
 * Skipped in headless mode (subagent children, `pi -p`) and outside git repos.
 */

import { rm } from "node:fs/promises";
import { join } from "node:path";
import type { ExtensionAPI, ExtensionContext, SessionEntry } from "@earendil-works/pi-coding-agent";

const TYPE = "git-checkpoint";
const REF_PREFIX = "refs/pi-checkpoints";
const MAX_AGE_DAYS = 14;

interface Checkpoint {
	commit: string;
	tree: string;
	repo: string;
	prompt: string;
	ts: number;
}

// Snapshot via throwaway index seeded from the real one (keeps `add -A` incremental).
// $1 = commit message. Prints "<tree> <commit>".
const SNAPSHOT_SH = `
set -euo pipefail
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
idx=$(git rev-parse --git-path index)
[ -f "$idx" ] && cp "$idx" "$dir/index"
export GIT_INDEX_FILE="$dir/index"
git add -A
tree=$(git write-tree)
if parent=$(git rev-parse --verify -q HEAD); then
  commit=$(git commit-tree "$tree" -p "$parent" -m "$1")
else
  commit=$(git commit-tree "$tree" -m "$1")
fi
echo "$tree $commit"
`;

export default function (pi: ExtensionAPI) {
	let repo: string | null | undefined; // undefined = not probed, null = not a repo
	let lastSnap: { tree: string; commit: string } | undefined;

	async function repoRoot(ctx: ExtensionContext): Promise<string | null> {
		if (repo !== undefined) return repo;
		const r = await pi.exec("git", ["rev-parse", "--show-toplevel"], { cwd: ctx.cwd, timeout: 5000 });
		const root = r.code === 0 ? r.stdout.trim() : null;
		repo = root;
		return root;
	}

	async function snapshot(root: string, label: string, sessionId: string): Promise<{ tree: string; commit: string } | undefined> {
		const r = await pi.exec("bash", ["-c", SNAPSHOT_SH, "bash", `pi checkpoint: ${label}`], { cwd: root, timeout: 30_000 });
		if (r.code !== 0) return undefined;
		const [tree, commit] = r.stdout.trim().split(/\s+/);
		if (!tree || !commit) return undefined;
		if (lastSnap?.tree === tree) return lastSnap; // nothing changed: reuse, no new ref
		await pi.exec("git", ["update-ref", `${REF_PREFIX}/${sessionId}/${Date.now()}`, commit], { cwd: root, timeout: 5000 });
		lastSnap = { tree, commit };
		return lastSnap;
	}

	async function restore(ctx: ExtensionContext, cp: Checkpoint): Promise<boolean> {
		const sessionId = ctx.sessionManager.getSessionId();
		const current = await snapshot(cp.repo, `before restore to ${cp.commit.slice(0, 8)}`, sessionId);
		if (!current) {
			ctx.ui.notify("Checkpoint restore aborted: could not snapshot current state", "error");
			return false;
		}
		pi.appendEntry<Checkpoint>(TYPE, { ...current, repo: cp.repo, prompt: `(before restore to ${cp.commit.slice(0, 8)})`, ts: Date.now() });

		// Delete files that exist now but not in the checkpoint.
		const added = await pi.exec("git", ["diff", "--name-only", "-z", "--diff-filter=A", cp.tree, current.tree], { cwd: cp.repo });
		for (const f of added.stdout.split("\0").filter(Boolean)) {
			await rm(join(cp.repo, f), { force: true });
		}
		const r = await pi.exec("git", ["restore", `--source=${cp.commit}`, "--worktree", "--", ":/"], { cwd: cp.repo, timeout: 30_000 });
		if (r.code !== 0) {
			ctx.ui.notify(`git restore failed: ${r.stderr.trim()}`, "error");
			return false;
		}
		lastSnap = { tree: cp.tree, commit: cp.commit };
		ctx.ui.notify(`Restored working tree to checkpoint ${cp.commit.slice(0, 8)}`, "info");
		return true;
	}

	function asCheckpoint(e: SessionEntry | undefined): Checkpoint | undefined {
		return e?.type === "custom" && e.customType === TYPE ? (e.data as Checkpoint) : undefined;
	}

	/** Checkpoint taken for the prompt in `userEntryId` (appended just before or just after it). */
	function checkpointFor(ctx: ExtensionContext, userEntryId: string): Checkpoint | undefined {
		const entries = ctx.sessionManager.getEntries();
		const idx = entries.findIndex((e) => e.id === userEntryId);
		if (idx < 0) return undefined;
		const before = asCheckpoint(entries[idx - 1]);
		if (before) return before;
		for (let i = idx + 1; i < entries.length; i++) {
			const e = entries[i];
			const cp = asCheckpoint(e);
			if (cp) return cp;
			if (e.type === "message" && e.message.role !== "user") break;
		}
		return undefined;
	}

	pi.on("session_start", async (_e, ctx) => {
		repo = undefined;
		lastSnap = undefined;
		if (!ctx.hasUI) return;
		const root = await repoRoot(ctx);
		if (!root) return;
		// prune old refs (fire and forget)
		const cutoff = Date.now() / 1000 - MAX_AGE_DAYS * 86400;
		void pi
			.exec("git", ["for-each-ref", "--format=%(refname) %(committerdate:unix)", REF_PREFIX], { cwd: root })
			.then(async (r) => {
				for (const line of r.stdout.split("\n").filter(Boolean)) {
					const [ref, ts] = line.split(" ");
					if (Number(ts) < cutoff) await pi.exec("git", ["update-ref", "-d", ref], { cwd: root });
				}
			})
			.catch(() => {});
	});

	pi.on("before_agent_start", async (event, ctx) => {
		if (!ctx.hasUI) return;
		const root = await repoRoot(ctx);
		if (!root) return;
		const label = event.prompt.replace(/\s+/g, " ").trim().slice(0, 80);
		const snap = await snapshot(root, label, ctx.sessionManager.getSessionId()).catch(() => undefined);
		if (snap) pi.appendEntry<Checkpoint>(TYPE, { ...snap, repo: root, prompt: label, ts: Date.now() });
	});

	pi.on("session_before_fork", async (event, ctx) => {
		if (!ctx.hasUI || event.position !== "before") return;
		const cp = checkpointFor(ctx, event.entryId);
		if (!cp) return;
		const keep = "Keep current code";
		const choice = await ctx.ui.select(`Restore code to checkpoint before this message (${cp.commit.slice(0, 8)})?`, [
			keep,
			"Restore code",
		]);
		if (choice && choice !== keep) await restore(ctx, cp);
	});

	pi.registerCommand("checkpoints", {
		description: "Restore the working tree to a git checkpoint from this session branch",
		handler: async (_args, ctx) => {
			const cps = ctx.sessionManager
				.getBranch()
				.map(asCheckpoint)
				.filter((c): c is Checkpoint => !!c)
				.reverse();
			if (cps.length === 0) {
				ctx.ui.notify("No checkpoints on this branch", "info");
				return;
			}
			const labels = cps.map((c) => {
				const t = new Date(c.ts).toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" });
				return `${t}  ${c.commit.slice(0, 8)}  ${c.prompt || "(empty prompt)"}`;
			});
			const picked = await ctx.ui.select("Restore working tree to state before:", labels);
			if (!picked) return;
			const cp = cps[labels.indexOf(picked)];
			await ctx.waitForIdle();
			const ok = await ctx.ui.confirm(
				"Restore checkpoint?",
				`Overwrite working tree in ${cp.repo} with ${cp.commit.slice(0, 8)}.\nCurrent state is checkpointed first (undo via /checkpoints).`,
			);
			if (ok) await restore(ctx, cp);
		},
	});
}
