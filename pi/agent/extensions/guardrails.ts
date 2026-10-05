/**
 * Guardrails
 *
 * - bash: asks before destructive/irreversible commands (rm -r outside scratch
 *   dirs, force push, reset --hard, sudo, terraform/kubectl/helm mutations, ...)
 * - write/edit: hard-blocks .git internals, asks before touching secrets
 *   (.env, credentials, pi auth/mcp config, ssh/aws keys).
 *
 * Interactive (TUI / RPC e.g. pimacs): select dialog with
 *   Allow once / Allow rule for this session / Block / Block with feedback.
 * Headless (print/json, subagent children): blocked, reason sent to the model.
 *
 * Disable for one run: PI_GUARDRAILS=off pi
 */

import { homedir } from "node:os";
import { isAbsolute, resolve } from "node:path";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

interface Rule {
	id: string;
	why: string;
	test: (cmd: string) => boolean;
}

// One shell "segment" = text between ; && || | or newline. Keeps per-command regexes from
// matching across unrelated commands (e.g. `git status && echo main`).
const SEG = "[^;&|\\n]*";
const re = (src: string, flags = "") => new RegExp(src, flags);

// ---------- rm -r with scratch-dir allowlist ----------

const SCRATCH_PREFIXES = ["/tmp/", "/private/tmp/", "/var/folders/"];
const SCRATCH_DIRS = new Set([
	"node_modules",
	"dist",
	"build",
	"out",
	"coverage",
	".turbo",
	".next",
	".cache",
	".pytest_cache",
	"__pycache__",
	".mypy_cache",
	".ruff_cache",
]);

function isScratchTarget(t: string): boolean {
	if (/[`$]|\.\./.test(t)) return false; // variables, substitutions, parent traversal
	if (SCRATCH_PREFIXES.some((p) => t.startsWith(p) && t.length > p.length)) return true;
	if (t.includes("*")) return false;
	const last = t.replace(/\/+$/, "").split("/").pop() ?? "";
	return SCRATCH_DIRS.has(last);
}

function dangerousRm(cmd: string): boolean {
	for (const seg of cmd.split(/&&|\|\||[;|\n]/)) {
		const tokens = seg
			.trim()
			.split(/\s+/)
			.map((t) => t.replace(/^['"]|['"]$/g, ""))
			.filter(Boolean);
		while (tokens.length && (/^\w+=/.test(tokens[0]) || ["sudo", "command", "exec", "xargs"].includes(tokens[0])))
			tokens.shift();
		if (!tokens.length || tokens[0].split("/").pop() !== "rm") continue;
		const args = tokens.slice(1);
		const recursive = args.some((a) => a === "--recursive" || /^-[a-zA-Z]*[rR]/.test(a));
		if (!recursive) continue;
		const targets = args.filter((a) => !a.startsWith("-"));
		if (targets.length === 0 || !targets.every(isScratchTarget)) return true;
	}
	return false;
}

// ---------- rules ----------

const BASH_RULES: Rule[] = [
	{ id: "rm-recursive", why: "recursive delete outside scratch dirs", test: dangerousRm },
	{ id: "sudo", why: "runs as root", test: (c) => /(^|[\s;&|(])sudo\s/.test(c) },
	{
		id: "git-force-push",
		why: "rewrites remote history",
		test: (c) => re(`\\bgit\\b${SEG}\\bpush\\b${SEG}(\\s--force(-with-lease)?\\b|\\s-[a-zA-Z]*f\\b|\\s\\+\\S)`).test(c),
	},
	{
		id: "git-push-main",
		why: "pushes directly to main/master",
		test: (c) => re(`\\bgit\\b${SEG}\\bpush\\b${SEG}\\s(\\S+:)?(main|master)\\b`).test(c),
	},
	{ id: "git-reset-hard", why: "discards uncommitted changes", test: (c) => re(`\\bgit\\b${SEG}\\breset\\b${SEG}--hard`).test(c) },
	{ id: "git-clean", why: "deletes untracked files", test: (c) => re(`\\bgit\\b${SEG}\\bclean\\b${SEG}\\s-[a-zA-Z]*f`).test(c) },
	{
		id: "git-discard-all",
		why: "discards all working-tree changes",
		test: (c) => re(`\\bgit\\b${SEG}\\b(checkout|restore)\\b${SEG}\\s(--\\s+)?(\\.|:/)(\\s|$)`).test(c),
	},
	{ id: "git-branch-delete", why: "force-deletes a branch", test: (c) => re(`\\bgit\\b${SEG}\\bbranch\\b${SEG}\\s-D\\b`).test(c) },
	{ id: "git-stash-drop", why: "drops stashed work", test: (c) => re(`\\bgit\\b${SEG}\\bstash\\s+(drop|clear)\\b`).test(c) },
	{
		id: "git-worktree-force-remove",
		why: "removes worktree incl. uncommitted changes",
		test: (c) => re(`\\bgit\\b${SEG}\\bworktree\\s+remove\\b${SEG}(--force|\\s-f\\b)`).test(c),
	},
	{
		id: "gh-destructive",
		why: "merges/deletes on GitHub",
		test: (c) => /\bgh\s+(pr\s+merge|repo\s+delete|release\s+delete|repo\s+archive)\b/.test(c),
	},
	{ id: "publish", why: "publishes a package", test: (c) => /\b(npm|yarn|pnpm|bun)\s+publish\b/.test(c) },
	{
		id: "infra",
		why: "mutates infrastructure",
		test: (c) =>
			/\bterraform\s+(apply|destroy|import|state\s+rm)\b/.test(c) ||
			/\bkubectl\s+(delete|apply|replace|patch|scale|drain|cordon|rollout\s+restart)\b/.test(c) ||
			/\bhelm\s+(install|upgrade|uninstall|delete|rollback)\b/.test(c),
	},
	{
		id: "sql-destructive",
		why: "destructive SQL",
		test: (c) => /\b(DROP\s+(TABLE|DATABASE|SCHEMA)|TRUNCATE\s+(TABLE\s+)?\w)/i.test(c),
	},
	{ id: "pipe-to-shell", why: "executes remote script", test: (c) => /\b(curl|wget)\b[^\n]*\|\s*(sudo\s+)?(ba|z)?sh\b/.test(c) },
	{ id: "chmod-recursive", why: "recursive permission change", test: (c) => /\b(chmod|chown)\s+(-[a-zA-Z]*R|--recursive)\b/.test(c) },
	{ id: "disk", why: "low-level disk operation", test: (c) => /\b(mkfs|diskutil\s+erase\w*)\b|\bdd\b[^\n]*\bof=/.test(c) },
];

const HOME = homedir();

const BLOCKED_PATHS: Rule[] = [
	{ id: "git-internals", why: ".git internals must not be edited directly", test: (p) => /(^|\/)\.git(\/|$)/.test(p) },
];

const SENSITIVE_PATHS: Rule[] = [
	{
		id: "dotenv",
		why: "environment/secrets file",
		test: (p) => /(^|\/)\.env(\.[^/]+)?$/.test(p) && !/\.(example|sample|template)$/.test(p),
	},
	{
		id: "pi-config",
		why: "pi credentials / MCP config",
		test: (p) => p === `${HOME}/.pi/agent/auth.json` || p === `${HOME}/.pi/agent/mcp.json`,
	},
	{
		id: "credentials",
		why: "credential store",
		test: (p) =>
			p.startsWith(`${HOME}/.ssh/`) ||
			p.startsWith(`${HOME}/.aws/`) ||
			p.startsWith(`${HOME}/.kube/`) ||
			/\.(pem|key|p12)$/.test(p) ||
			/(^|\/)id_(rsa|ed25519|ecdsa)/.test(p),
	},
];

function normalizePath(raw: string, cwd: string): string {
	const p = raw.startsWith("~/") ? `${HOME}/${raw.slice(2)}` : raw;
	return isAbsolute(p) ? resolve(p) : resolve(cwd, p);
}

// ---------- extension ----------

export default function (pi: ExtensionAPI) {
	if (process.env.PI_GUARDRAILS === "off") return;

	let sessionAllowed = new Set<string>();
	pi.on("session_start", () => {
		sessionAllowed = new Set();
	});

	async function ask(ctx: ExtensionContext, rule: Rule, subject: string) {
		if (sessionAllowed.has(rule.id)) return undefined;
		if (!ctx.hasUI) {
			return {
				block: true,
				reason: `Guardrail "${rule.id}" (${rule.why}): needs user approval, unavailable in non-interactive mode. Use a safer alternative or ask the user to run it.`,
			};
		}
		const once = "Allow once";
		const session = `Allow "${rule.id}" for this session`;
		const block = "Block";
		const feedback = "Block with feedback…";
		const choice = await ctx.ui.select(`⚠️  ${rule.id}: ${rule.why}\n\n  ${subject}\n`, [once, session, block, feedback]);
		if (choice === once) return undefined;
		if (choice === session) {
			sessionAllowed.add(rule.id);
			return undefined;
		}
		if (choice === feedback) {
			const note = await ctx.ui.input("Why blocked / what instead?", "");
			return { block: true, reason: `User blocked (${rule.id}): ${note || "no reason given"}` };
		}
		return { block: true, reason: `User blocked (${rule.id}: ${rule.why}). Do not retry the same command.` };
	}

	pi.on("tool_call", async (event, ctx) => {
		if (event.toolName === "bash") {
			const cmd = String((event.input as { command?: unknown }).command ?? "");
			for (const rule of BASH_RULES) {
				if (rule.test(cmd)) {
					const res = await ask(ctx, rule, cmd.length > 600 ? `${cmd.slice(0, 600)}…` : cmd);
					if (res) return res;
				}
			}
			return undefined;
		}

		if (event.toolName === "write" || event.toolName === "edit") {
			const raw = (event.input as { path?: unknown }).path;
			if (typeof raw !== "string") return undefined;
			const path = normalizePath(raw, ctx.cwd);
			for (const rule of BLOCKED_PATHS) {
				if (rule.test(path)) return { block: true, reason: `Guardrail "${rule.id}": ${rule.why} (${path})` };
			}
			for (const rule of SENSITIVE_PATHS) {
				if (rule.test(path)) {
					const res = await ask(ctx, rule, `${event.toolName} ${path}`);
					if (res) return res;
				}
			}
		}
		return undefined;
	});

	pi.registerCommand("guardrails", {
		description: "Show guardrail rules and session allowances",
		handler: async (_args, ctx) => {
			const lines = [
				...BASH_RULES.map((r) => `bash  ${r.id}${sessionAllowed.has(r.id) ? "  (allowed this session)" : ""} — ${r.why}`),
				...BLOCKED_PATHS.map((r) => `path  ${r.id} [hard block] — ${r.why}`),
				...SENSITIVE_PATHS.map((r) => `path  ${r.id}${sessionAllowed.has(r.id) ? "  (allowed this session)" : ""} — ${r.why}`),
			];
			ctx.ui.notify(lines.join("\n"), "info");
		},
	});
}

// exported for tests
export const _internal = { dangerousRm, BASH_RULES, SENSITIVE_PATHS, BLOCKED_PATHS, normalizePath };
