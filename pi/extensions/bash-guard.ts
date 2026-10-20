/**
 * Bash Guard Extension
 *
 * Pi has no built-in permission system: every tool call runs with the full
 * permissions of the user. This extension adds a guard layer in front of the
 * `bash` and `read` tools.
 *
 * Two levels of enforcement:
 *
 *  1. `deny`  - the command matches a blocked pattern. The call is rejected
 *                outright, no prompt shown. The model is told why.
 *  2. `ask`   - the command looks risky but is not categorically forbidden.
 *                The user is prompted to approve it.
 *
 * The rule set mirrors the `permissions.deny` list from ~/.claude/settings.json
 * so both harnesses enforce the same policy.
 *
 * Configuration:
 *   ~/.pi/agent/bash-guard.json  (optional)
 *   {
 *     "enabled": true,
 *     "additionalDeny": ["Bash(git push --force *)"],
 *     "additionalAsk": ["Bash(npm publish *)"],
 *     "allow": ["Bash(git status)"]
 *   }
 *
 * A rule is `Tool(pattern)`. Patterns match with fnmatch-style globs.
 * `allow` takes precedence over `ask`, which takes precedence over `deny`.
 */

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import type { ExtensionAPI, ToolCallEventResult } from "@earendil-works/pi-coding-agent";

type RuleKind = "deny" | "ask" | "allow";

interface GuardConfig {
	enabled: boolean;
	additionalDeny: string[];
	additionalAsk: string[];
	allow: string[];
}

const DEFAULT_DENY: string[] = [
	// Filesystem destruction
	"Bash(rm -rf /)",
	"Bash(rm -rf /*)",
	"Bash(rm -rf ~)",
	"Bash(rm -rf ~/*)",
	"Bash(rm -rf $HOME)",
	"Bash(rm -rf $HOME/*)",
	"Bash(chmod -R 777 *)",
	"Bash(mkfs *)",
	"Bash(dd if=/dev/zero *)",
	"Bash(dd if=/dev/random *)",
	// Git history destruction
	"Bash(git push --force origin main *)",
	"Bash(git push --force origin main)",
	"Bash(git push --force origin master *)",
	"Bash(git push --force origin master)",
	"Bash(git push -f origin main *)",
	"Bash(git push -f origin main)",
	"Bash(git push -f origin master *)",
	"Bash(git push -f origin master)",
	"Bash(git push --force-with-lease origin main *)",
	"Bash(git push --force-with-lease origin main)",
	"Bash(git push --force-with-lease origin master *)",
	"Bash(git push --force-with-lease origin master)",
	"Bash(git branch -D main)",
	"Bash(git branch -D master)",
	"Bash(git reset --hard *)",
	"Bash(git reset --hard)",
	"Bash(git clean -fd *)",
	"Bash(git clean -fd)",
	"Bash(git clean -fdx *)",
	"Bash(git clean -fdx)",
	// Destructive GitHub operations
	"Bash(gh repo delete *)",
	"Bash(gh repo archive *)",
	"Bash(gh repo rename *)",
	"Bash(gh repo edit *)",
	"Bash(gh release delete *)",
	"Bash(gh gist delete *)",
	"Bash(gh gist edit *)",
	"Bash(gh workflow disable *)",
];

/** Paths the `read` tool must not open. Matched against the resolved path. */
const DEFAULT_DENY_READ: string[] = [
	"**/.env",
	"**/.env.*",
	"**/.op-service-account*",
	"**/.netrc",
	"**/.pgpass",
	"**/.htpasswd",
	"**/.ssh/*",
];

/**
 * Commands that are asked about first: risky but legitimately used.
 * Deliberately narrow. Anything that shows up in ordinary day-to-day work
 * (git commit, git status, make, npm test) must stay out of this list, or the
 * guard becomes noise and gets disabled.
 */
const DEFAULT_ASK: string[] = [
	"Bash(git push *)",
	"Bash(npm publish *)",
	"Bash(docker compose up *)",
	"Bash(docker compose start *)",
	"Bash(docker compose down *)",
	"Bash(docker compose restart *)",
	"Bash(brew uninstall *)",
	"Bash(pip uninstall *)",
	"Bash(pkill *)",
	"Bash(killall *)",
	"Bash(sudo *)",
	"Bash(curl * | sh)",
	"Bash(curl * | bash)",
	"Bash(wget * | sh)",
	"Bash(chmod 777 *)",
	"Bash(chown *)",
];

/** fnmatch-style glob match (`*` and `?`, `**` crosses directory boundaries). */
function globToRegExp(pattern: string): RegExp {
	let out = "";
	let i = 0;
	while (i < pattern.length) {
		const char = pattern[i];
		if (char === "*") {
			if (pattern[i + 1] === "*") {
				// `**/` matches zero or more path segments, `**` matches anything
				if (pattern[i + 2] === "/") {
					out += "(?:.*/)?";
					i += 3;
					continue;
				}
				out += ".*";
				i += 2;
				continue;
			}
			out += "[^/]*";
			i += 1;
			continue;
		}
		if (char === "?") {
			out += "[^/]";
			i += 1;
			continue;
		}
		out += char.replace(/[.+^${}()|[\]\\]/g, "\\$&");
		i += 1;
	}
	return new RegExp(`^${out}$`);
}

interface CompiledRule {
	kind: RuleKind;
	tool: string;
	pattern: string;
	regex: RegExp;
}

function compileRules(kind: RuleKind, rules: string[]): CompiledRule[] {
	const compiled: CompiledRule[] = [];
	for (const rule of rules) {
		const match = /^([A-Za-z_]+)\((.*)\)$/.exec(rule);
		if (!match) continue;
		const tool = match[1];
		const pattern = match[2];
		compiled.push({ kind, tool, pattern, regex: globToRegExp(pattern) });
	}
	return compiled;
}

function loadConfig(): GuardConfig {
	const configPath = path.join(os.homedir(), ".pi", "agent", "bash-guard.json");
	const base: GuardConfig = { enabled: true, additionalDeny: [], additionalAsk: [], allow: [] };
	if (!fs.existsSync(configPath)) return base;
	try {
		const parsed = JSON.parse(fs.readFileSync(configPath, "utf-8")) as Partial<GuardConfig>;
		return {
			enabled: parsed.enabled !== false,
			additionalDeny: parsed.additionalDeny ?? [],
			additionalAsk: parsed.additionalAsk ?? [],
			allow: parsed.allow ?? [],
		};
	} catch {
		return base;
	}
}

/**
 * A rule matches when the tool matches and the command matches the glob.
 * Claude's `Bash(git status)` style rules are prefix-ish; we also treat a
 * rule without a trailing glob as a prefix match on the whole command.
 */
function ruleMatches(rule: CompiledRule, tool: string, subject: string): boolean {
	if (rule.tool.toLowerCase() !== tool.toLowerCase()) return false;
	if (rule.regex.test(subject)) return true;
	// A rule with no wildcard matches a command that starts with the pattern.
	if (!rule.pattern.includes("*") && !rule.pattern.includes("?")) {
		return subject === rule.pattern || subject.startsWith(`${rule.pattern} `);
	}
	return false;
}

/** Normalize a command for matching: collapse whitespace, strip leading env assignments. */
function normalizeCommand(command: string): string {
	return command.replace(/\s+/g, " ").trim();
}

export default function (pi: ExtensionAPI) {
	const config = loadConfig();
	if (!config.enabled) return;

	const allowRules = compileRules("allow", config.allow);
	const denyRules = compileRules("deny", [...DEFAULT_DENY, ...config.additionalDeny]);
	const askRules = compileRules("ask", [...DEFAULT_ASK, ...config.additionalAsk]);
	const denyReadPatterns = DEFAULT_DENY_READ.map((p) => globToRegExp(p));

	function decide(tool: string, subject: string): { kind: RuleKind; rule?: CompiledRule } | undefined {
		const all = [...allowRules, ...askRules, ...denyRules];
		// allow wins over ask wins over deny
		for (const rule of allowRules) if (ruleMatches(rule, tool, subject)) return { kind: "allow", rule };
		for (const rule of denyRules) if (ruleMatches(rule, tool, subject)) return { kind: "deny", rule };
		for (const rule of askRules) if (ruleMatches(rule, tool, subject)) return { kind: "ask", rule };
		void all;
		return undefined;
	}

	pi.on("tool_call", async (event, ctx): Promise<ToolCallEventResult | undefined> => {
		if (event.toolName === "bash") {
			const command = normalizeCommand(String((event.input as { command?: string }).command ?? ""));
			if (!command) return undefined;

			const decision = decide("bash", command);
			if (!decision) return undefined;

			if (decision.kind === "deny") {
				const reason = `Blocked by bash-guard: \`${decision.rule?.pattern}\` is on the deny list.`;
				ctx.ui.notify(reason, "error");
				return { block: true, reason };
			}

			if (decision.kind === "ask") {
				if (!ctx.hasUI) {
					// No UI to ask with (print mode, RPC). Fail closed for a rule the user
					// would have had to approve, and say so.
					return {
						block: true,
						reason: `bash-guard requires approval for \`${decision.rule?.pattern}\`, but there is no UI to ask. Run interactively or add an "allow" rule in ~/.pi/agent/bash-guard.json.`,
					};
				}
				const approved = await ctx.ui.confirm(
					"Run this command?",
					`bash-guard wants confirmation for:\n\n${command}\n\n(rule: ${decision.rule?.pattern})`,
				);
				if (!approved) {
					return { block: true, reason: "The user declined the command." };
				}
			}
			return undefined;
		}

		if (event.toolName === "read") {
			const filePath = String((event.input as { path?: string }).path ?? "");
			if (!filePath) return undefined;
			const absolute = path.resolve(filePath);
			const homeRelative = absolute.startsWith(os.homedir())
				? `~/${path.relative(os.homedir(), absolute)}`
				: absolute;
			for (const regex of denyReadPatterns) {
				if (regex.test(homeRelative) || regex.test(absolute)) {
					const reason = `Blocked by bash-guard: reading ${homeRelative} is not allowed.`;
					ctx.ui.notify(reason, "error");
					return { block: true, reason };
				}
			}
		}

		return undefined;
	});
}
