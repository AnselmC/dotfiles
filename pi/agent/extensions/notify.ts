/**
 * Notify
 *
 * macOS notification when:
 * - an agent run settles after >= PI_NOTIFY_MIN_SECONDS (default 20s)
 * - pi blocks on a UI prompt (e.g. guardrails approval) during a long run
 *
 * Uses osascript (works from pimacs/RPC, Terminal.app, anywhere). Falls back to
 * OSC 777 on non-macOS. Skipped in headless mode (subagent children, `pi -p`).
 */

import { basename } from "node:path";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

const MIN_MS = Number(process.env.PI_NOTIFY_MIN_SECONDS ?? 20) * 1000;

function fmtDuration(ms: number): string {
	const s = Math.round(ms / 1000);
	return s < 60 ? `${s}s` : `${Math.floor(s / 60)}m${String(s % 60).padStart(2, "0")}s`;
}

function lastAssistantText(ctx: ExtensionContext): string {
	const branch = ctx.sessionManager.getBranch();
	for (let i = branch.length - 1; i >= 0; i--) {
		const e = branch[i];
		if (e.type !== "message" || e.message.role !== "assistant") continue;
		const content = e.message.content;
		const text = Array.isArray(content)
			? content
					.filter((c): c is { type: "text"; text: string } => c?.type === "text")
					.map((c) => c.text)
					.join(" ")
			: String(content ?? "");
		const clean = text.replace(/[`*#>_]/g, "").replace(/\s+/g, " ").trim();
		if (clean) return clean.length > 140 ? `${clean.slice(0, 140)}…` : clean;
	}
	return "";
}

export default function (pi: ExtensionAPI) {
	let runStart: number | undefined;

	function title(ctx: ExtensionContext): string {
		return `pi — ${pi.getSessionName() ?? basename(ctx.cwd)}`;
	}

	async function send(t: string, body: string) {
		if (process.platform === "darwin") {
			await pi.exec(
				"osascript",
				[
					"-e",
					"on run argv",
					"-e",
					'display notification (item 2 of argv) with title (item 1 of argv) sound name "Glass"',
					"-e",
					"end run",
					t,
					body,
				],
				{ timeout: 5000 },
			);
		} else {
			process.stdout.write(`\x1b]777;notify;${t};${body.replace(/[;\x07\x1b]/g, " ")}\x07`);
		}
	}

	pi.on("agent_start", () => {
		runStart ??= Date.now(); // keep first start across retries/continuations
	});

	pi.on("agent_settled", async (_e, ctx) => {
		const start = runStart;
		runStart = undefined;
		if (!ctx.hasUI || start === undefined) return;
		const elapsed = Date.now() - start;
		if (elapsed < MIN_MS) return;
		const summary = lastAssistantText(ctx);
		await send(title(ctx), `✓ ${fmtDuration(elapsed)}${summary ? ` · ${summary}` : ""}`).catch(() => {});
	});

	pi.on("ui_prompt_start", async (event, ctx) => {
		if (!ctx.hasUI || runStart === undefined || Date.now() - runStart < MIN_MS) return;
		const what = event.title?.split("\n")[0] ?? event.kind;
		await send(title(ctx), `⏸ Waiting for you: ${what}`).catch(() => {});
	});

	pi.registerCommand("notify-test", {
		description: "Send a test notification",
		handler: async (_args, ctx) => {
			await send(title(ctx), "Test notification");
		},
	});
}
