/**
 * Presets — named bundles of model + thinking level + tools + instructions.
 *
 * Config (merged, project wins): ~/.pi/agent/presets.json, <cwd>/.pi/presets.json
 *
 *   {
 *     "quick": { "provider": "anthropic", "model": "claude-sonnet-5-5", "thinkingLevel": "low" },
 *     "plan":  { "excludeTools": ["edit", "write"], "readOnlyBash": true,
 *                "instructions": ["PLANNING MODE.", "Do not modify files."] }
 *   }
 *
 * Fields (all optional):
 *   provider + model   switch model
 *   thinkingLevel      off|minimal|low|medium|high|xhigh|max
 *   tools              exact active-tool allowlist
 *   excludeTools       remove these from the tools active before the first preset
 *   readOnlyBash       block bash commands that obviously mutate files/git state
 *   instructions       string | string[] — added as a <preset> system prompt section
 *
 * Usage: /preset [name|none], `pi --preset <name>`, Ctrl+Shift+U cycles (TUI).
 * Active preset persists in the session and is restored on resume.
 * Works in TUI and RPC (pimacs) — uses plain select dialogs.
 */

import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import type { Api, Model } from "@earendil-works/pi-ai";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { CONFIG_DIR_NAME, getAgentDir } from "@earendil-works/pi-coding-agent";

type ThinkingLevel = "off" | "minimal" | "low" | "medium" | "high" | "xhigh" | "max";

interface Preset {
	description?: string;
	provider?: string;
	model?: string;
	thinkingLevel?: ThinkingLevel;
	tools?: string[];
	excludeTools?: string[];
	readOnlyBash?: boolean;
	instructions?: string | string[];
}

const STATE_TYPE = "preset-state";
const NONE = "none";

// Obvious mutations: redirects to files, file ops, git state changes, installs, in-place edits.
const MUTATING_BASH =
	/(^|[;&|(]\s*|\s)(rm|mv|cp|mkdir|rmdir|touch|chmod|chown|ln|tee|truncate|patch)\s|\bsed\s+(-[a-zA-Z]*i|--in-place)|\bgit\s+(commit|push|pull|checkout|switch|reset|merge|rebase|cherry-pick|revert|stash|add|rm|mv|restore|clean|apply|am|tag|branch\s+-[dDmM])\b|\b(npm|pnpm|yarn|bun)\s+(i|install|ci|add|remove|uninstall|update|publish)\b|\bpip\s+install\b|(^|[^0-9&>])>{1,2}\s*(?!\/dev\/null|&)[^\s&|]/;

function readJson(path: string): Record<string, Preset> {
	if (!existsSync(path)) return {};
	try {
		return JSON.parse(readFileSync(path, "utf-8"));
	} catch (err) {
		console.error(`preset: failed to parse ${path}: ${err}`);
		return {};
	}
}

function describe(p: Preset): string {
	if (p.description) return p.description;
	const parts: string[] = [];
	if (p.model) parts.push(p.model);
	if (p.thinkingLevel) parts.push(`thinking:${p.thinkingLevel}`);
	if (p.tools) parts.push(`tools:${p.tools.length}`);
	if (p.excludeTools) parts.push(`-${p.excludeTools.join(",-")}`);
	if (p.readOnlyBash) parts.push("read-only bash");
	if (p.instructions) parts.push("+instructions");
	return parts.join(" · ");
}

export default function (pi: ExtensionAPI) {
	let presets: Record<string, Preset> = {};
	let activeName: string | undefined;
	let original: { model: Model<Api> | undefined; thinking: ThinkingLevel; tools: string[] } | undefined;

	pi.registerFlag("preset", { description: "Preset to activate at startup", type: "string" });

	function status(ctx: ExtensionContext) {
		ctx.ui.setStatus("preset", activeName ? `preset:${activeName}` : undefined);
	}

	function applyTools(name: string, p: Preset, ctx: ExtensionContext) {
		const all = new Set(pi.getAllTools().map((t) => t.name));
		let next: string[] | undefined;
		if (p.tools?.length) {
			const unknown = p.tools.filter((t) => !all.has(t));
			if (unknown.length) ctx.ui.notify(`Preset "${name}": unknown tools ${unknown.join(", ")}`, "warning");
			next = p.tools.filter((t) => all.has(t));
		} else if (p.excludeTools?.length) {
			const base = original?.tools ?? pi.getActiveTools();
			next = base.filter((t) => !p.excludeTools!.includes(t));
		} else if (original) {
			next = original.tools;
		}
		if (next?.length) pi.setActiveTools(next);
	}

	/** full = also switch model/thinking (skipped on session restore; pi restores those itself) */
	async function apply(name: string, ctx: ExtensionContext, full = true): Promise<boolean> {
		const p = presets[name];
		if (!p) {
			ctx.ui.notify(`Unknown preset "${name}". Available: ${Object.keys(presets).join(", ") || "(none)"}`, "error");
			return false;
		}
		original ??= { model: ctx.model, thinking: pi.getThinkingLevel() as ThinkingLevel, tools: pi.getActiveTools() };
		if (full && p.provider && p.model) {
			const model = ctx.modelRegistry.find(p.provider, p.model);
			if (!model) ctx.ui.notify(`Preset "${name}": model ${p.provider}/${p.model} not found`, "warning");
			else if (!(await pi.setModel(model))) ctx.ui.notify(`Preset "${name}": no auth for ${p.provider}`, "warning");
		}
		if (full && p.thinkingLevel) pi.setThinkingLevel(p.thinkingLevel);
		applyTools(name, p, ctx);
		activeName = name;
		status(ctx);
		return true;
	}

	async function clear(ctx: ExtensionContext) {
		if (original) {
			if (original.model) await pi.setModel(original.model);
			pi.setThinkingLevel(original.thinking);
			pi.setActiveTools(original.tools);
		}
		activeName = undefined;
		original = undefined;
		status(ctx);
	}

	async function switchTo(name: string, ctx: ExtensionContext) {
		if (name === NONE) {
			await clear(ctx);
			ctx.ui.notify("Preset cleared", "info");
		} else if (await apply(name, ctx)) {
			ctx.ui.notify(`Preset "${name}" active — ${describe(presets[name])}`, "info");
		} else return;
		pi.appendEntry(STATE_TYPE, { name: activeName ?? null });
	}

	pi.registerCommand("preset", {
		description: "Switch preset (model / thinking / tools / instructions)",
		getArgumentCompletions: (prefix) => {
			const items = [...Object.keys(presets), NONE]
				.filter((n) => n.startsWith(prefix))
				.map((n) => ({ value: n, label: n, description: n === NONE ? "restore defaults" : describe(presets[n]) }));
			return items.length ? items : null;
		},
		handler: async (args, ctx) => {
			const arg = args.trim();
			if (arg) return switchTo(arg, ctx);
			const names = Object.keys(presets);
			if (!names.length) {
				ctx.ui.notify(`No presets. Define them in ${join(getAgentDir(), "presets.json")}`, "warning");
				return;
			}
			const labels = [...names, NONE].map((n) => {
				const mark = (n === NONE ? !activeName : n === activeName) ? "● " : "  ";
				return `${mark}${n} — ${n === NONE ? "restore defaults" : describe(presets[n])}`;
			});
			const picked = await ctx.ui.select("Preset", labels);
			if (picked) await switchTo([...names, NONE][labels.indexOf(picked)], ctx);
		},
	});

	pi.registerShortcut("ctrl+shift+u", {
		description: "Cycle presets",
		handler: async (ctx) => {
			const cycle = [NONE, ...Object.keys(presets).sort()];
			const i = cycle.indexOf(activeName ?? NONE);
			await switchTo(cycle[(i + 1) % cycle.length], ctx);
		},
	});

	pi.on("session_start", async (_e, ctx) => {
		presets = { ...readJson(join(getAgentDir(), "presets.json")) };
		if (ctx.isProjectTrusted()) Object.assign(presets, readJson(join(ctx.cwd, CONFIG_DIR_NAME, "presets.json")));
		activeName = undefined;
		original = undefined;

		const flag = pi.getFlag("preset");
		if (typeof flag === "string" && flag) {
			await apply(flag, ctx);
			return;
		}
		// restore from session branch
		let saved: string | null | undefined;
		for (const e of ctx.sessionManager.getBranch()) {
			if (e.type === "custom" && e.customType === STATE_TYPE) saved = (e.data as { name: string | null }).name;
		}
		if (saved && presets[saved]) await apply(saved, ctx, false);
		status(ctx);
	});

	pi.on("before_agent_start", (event) => {
		const sections = (event.systemPromptOptions.sections ??= {});
		const instr = activeName ? presets[activeName]?.instructions : undefined;
		if (instr) sections.preset = Array.isArray(instr) ? instr.join("\n") : instr;
		else delete sections.preset;
	});

	pi.on("tool_call", (event) => {
		if (!activeName || !presets[activeName]?.readOnlyBash || event.toolName !== "bash") return;
		const cmd = String((event.input as { command?: unknown }).command ?? "");
		if (MUTATING_BASH.test(cmd)) {
			return {
				block: true,
				reason: `Preset "${activeName}" is read-only: bash may only inspect (no writes, redirects to files, git state changes, installs). Switch preset with /preset to make changes.`,
			};
		}
	});
}

export const _internal = { MUTATING_BASH };
