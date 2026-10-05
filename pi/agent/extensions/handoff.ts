/**
 * Handoff — move to a fresh session with a focused, generated kickoff prompt
 * instead of compacting (lossy) or dragging a long context along.
 *
 *   /handoff implement phase 2 of the plan
 *   /handoff            (asks for the goal)
 *
 * Summarizes the current branch (respecting compaction) + current git state with
 * the active model, lets you edit the result, then opens a new session (parent
 * linked) with the prompt pre-filled in the editor. Works in TUI and RPC (pimacs).
 */

import type { AgentMessage } from "@earendil-works/pi-agent-core";
import { type Message, uuidv7 } from "@earendil-works/pi-ai";
import type { ExtensionAPI, ExtensionCommandContext, SessionEntry } from "@earendil-works/pi-coding-agent";
import { BorderedLoader, convertToLlm, serializeConversation } from "@earendil-works/pi-coding-agent";

const MAX_CONVERSATION_CHARS = 400_000;

const SYSTEM_PROMPT = `You write handoff prompts that let a fresh coding-agent session continue work from a previous one.

Given the previous conversation, the current git state, and the user's goal for the new session, write a self-contained prompt:

## Context
- What we are working on and why (1-3 sentences)
- Decisions made and approaches rejected (with the reason, briefly)
- Key findings: root causes, constraints, gotchas, conventions discovered
- Current state: what is done, what is half-done, what is verified vs. unverified

## Files
- Relevant paths, each with a few words on its role / what changed

## Task
- The next task, derived from the user's goal, with concrete acceptance criteria
- Commands to run to verify (tests, typecheck, etc.) if known

Rules: be dense and specific (names, paths, commands, error messages verbatim). Omit chit-chat, dead ends that don't matter, and anything unrelated to the goal. No preamble — output only the prompt.`;

function entryToMessage(entry: SessionEntry): AgentMessage | undefined {
	if (entry.type === "message") return entry.message;
	if (entry.type === "compaction") {
		return {
			role: "compactionSummary",
			summary: entry.summary,
			tokensBefore: entry.tokensBefore,
			timestamp: new Date(entry.timestamp).getTime(),
		};
	}
	return undefined;
}

/** Branch messages, collapsed at the latest compaction (summary + kept tail). */
function branchMessages(branch: SessionEntry[]): AgentMessage[] {
	let ci = -1;
	for (let i = branch.length - 1; i >= 0; i--) {
		if (branch[i].type === "compaction") {
			ci = i;
			break;
		}
	}
	let entries = branch;
	if (ci >= 0) {
		const c = branch[ci];
		const keptFrom = c.type === "compaction" ? branch.findIndex((e) => e.id === c.firstKeptEntryId) : -1;
		entries = [c, ...(keptFrom >= 0 ? branch.slice(keptFrom, ci) : []), ...branch.slice(ci + 1)];
	}
	return entries.map(entryToMessage).filter((m): m is AgentMessage => m !== undefined);
}

function truncateMiddle(text: string, max: number): string {
	if (text.length <= max) return text;
	const head = Math.floor(max * 0.25);
	const tail = max - head;
	return `${text.slice(0, head)}\n\n[… ${text.length - max} chars of middle conversation omitted …]\n\n${text.slice(-tail)}`;
}

export default function (pi: ExtensionAPI) {
	async function gitState(cwd: string): Promise<string> {
		const branch = await pi.exec("git", ["branch", "--show-current"], { cwd, timeout: 5000 });
		if (branch.code !== 0) return "(not a git repository)";
		const status = await pi.exec("git", ["status", "--short"], { cwd, timeout: 10_000 });
		const stat = await pi.exec("git", ["diff", "HEAD", "--stat"], { cwd, timeout: 10_000 });
		const log = await pi.exec("git", ["log", "--oneline", "-5"], { cwd, timeout: 5000 });
		return [
			`cwd: ${cwd}`,
			`branch: ${branch.stdout.trim() || "(detached)"}`,
			`recent commits:\n${log.stdout.trim()}`,
			`status:\n${status.stdout.trim().slice(0, 4000) || "(clean)"}`,
			`diff vs HEAD:\n${stat.stdout.trim().slice(0, 4000) || "(none)"}`,
		].join("\n\n");
	}

	async function generate(ctx: ExtensionCommandContext, input: string): Promise<string | null> {
		const run = async (signal?: AbortSignal) => {
			const userMessage: Message = { role: "user", content: [{ type: "text", text: input }], timestamp: Date.now() };
			const res = await ctx.modelRegistry.complete(
				ctx.model!,
				{ systemPrompt: SYSTEM_PROMPT, messages: [userMessage] },
				{ signal, cacheRetention: "none", sessionId: uuidv7() },
			);
			if (res.stopReason === "aborted") return null;
			if (res.stopReason === "error") throw new Error(res.errorMessage ?? "model error");
			return res.content
				.filter((c): c is { type: "text"; text: string } => c.type === "text")
				.map((c) => c.text)
				.join("\n")
				.trim();
		};

		if (ctx.mode === "tui") {
			return ctx.ui.custom<string | null>((tui, theme, _kb, done) => {
				const loader = new BorderedLoader(tui, theme, "Generating handoff prompt…");
				loader.onAbort = () => done(null);
				run(loader.signal)
					.then(done)
					.catch((err) => {
						ctx.ui.notify(`Handoff failed: ${err instanceof Error ? err.message : err}`, "error");
						done(null);
					});
				return loader;
			});
		}
		// RPC (pimacs) / other: status line while generating
		ctx.ui.setStatus("handoff", "Generating handoff prompt…");
		try {
			return await run();
		} catch (err) {
			ctx.ui.notify(`Handoff failed: ${err instanceof Error ? err.message : err}`, "error");
			return null;
		} finally {
			ctx.ui.setStatus("handoff", undefined);
		}
	}

	pi.registerCommand("handoff", {
		description: "Start a fresh session with a generated context-transfer prompt",
		handler: async (args, ctx) => {
			if (!ctx.hasUI) return;
			if (!ctx.model) {
				ctx.ui.notify("No model selected", "error");
				return;
			}
			const goal = args.trim() || (await ctx.ui.input("Goal for the new session?", "continue where we left off"))?.trim();
			if (!goal) return;

			await ctx.waitForIdle();
			const messages = branchMessages(ctx.sessionManager.getBranch());
			if (messages.length === 0) {
				ctx.ui.notify("No conversation to hand off", "error");
				return;
			}
			const conversation = truncateMiddle(serializeConversation(convertToLlm(messages)), MAX_CONVERSATION_CHARS);
			const input = `## Previous conversation\n\n${conversation}\n\n## Current git state\n\n${await gitState(ctx.cwd)}\n\n## User's goal for the new session\n\n${goal}`;

			const draft = await generate(ctx, input);
			if (!draft) {
				ctx.ui.notify("Handoff cancelled", "info");
				return;
			}
			const prompt = await ctx.ui.editor("Edit handoff prompt (saved → new session)", draft);
			if (prompt === undefined || !prompt.trim()) {
				ctx.ui.notify("Handoff cancelled", "info");
				return;
			}

			const result = await ctx.newSession({
				parentSession: ctx.sessionManager.getSessionFile(),
				// only the replacement ctx is valid in here (old pi/ctx are stale)
				withSession: async (newCtx) => {
					newCtx.ui.setEditorText(prompt);
					newCtx.ui.notify("Handoff ready — review and submit.", "info");
				},
			});
			if (result.cancelled) ctx.ui.notify("New session cancelled", "info");
		},
	});
}
