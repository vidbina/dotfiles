/**
 * Worklog heartbeat plugin for OpenCode.
 *
 * Records a heartbeat to ~/.claude/worklog.db on every user message,
 * mirroring the Claude Code UserPromptSubmit hook. Uses source='opencode'
 * to distinguish from Claude Code heartbeats.
 *
 * Auto-loads from ~/.config/opencode/plugins/ (symlinked from dotfiles).
 */

import { Database } from "bun:sqlite";
import { execSync } from "child_process";
import { existsSync, readFileSync } from "fs";
import { join, basename } from "path";
import { homedir } from "os";

const DB_PATH = join(homedir(), ".claude", "worklog.db");

const CREATE_TABLE = `
  CREATE TABLE IF NOT EXISTS heartbeats (
    ts      TEXT    NOT NULL,
    project TEXT    NOT NULL,
    cwd     TEXT    NOT NULL,
    source  TEXT    DEFAULT 'claude-code',
    flushed INTEGER DEFAULT 0
  )`;

const CREATE_INDEX =
  "CREATE INDEX IF NOT EXISTS idx_hb ON heartbeats(project, flushed)";

const INSERT =
  "INSERT INTO heartbeats(ts, project, cwd, source) VALUES(?, ?, ?, 'opencode')";

function resolveProject(cwd: string): string | null {
  // Check for .worklog-project file (explicit project override)
  try {
    const root = execSync("git rev-parse --show-toplevel", {
      cwd,
      encoding: "utf-8",
      stdio: ["pipe", "pipe", "pipe"],
    }).trim();
    const wpf = join(root, ".worklog-project");
    if (existsSync(wpf)) {
      return readFileSync(wpf, "utf-8").trim();
    }
  } catch {
    // Not a git repo — fall through to heuristic
  }

  // Heuristic: first segment of repo name from git remote
  try {
    const remoteUrl = execSync("git remote get-url origin", {
      cwd,
      encoding: "utf-8",
      stdio: ["pipe", "pipe", "pipe"],
    }).trim();
    const repoName = basename(remoteUrl, ".git");
    const project = repoName.split("-")[0].toLowerCase();
    return project || null;
  } catch {
    // No git remote — use directory name
    const project = basename(cwd).split("-")[0].toLowerCase();
    return project || null;
  }
}

function localTimestamp(): string {
  const now = new Date();
  const pad = (n: number) => String(n).padStart(2, "0");
  return `${now.getFullYear()}-${pad(now.getMonth() + 1)}-${pad(now.getDate())}T${pad(now.getHours())}:${pad(now.getMinutes())}:${pad(now.getSeconds())}`;
}

export default () => {
  let db: Database | null = null;

  function getDb(): Database {
    if (!db) {
      db = new Database(DB_PATH, { create: true });
      db.run(CREATE_TABLE);
      db.run(CREATE_INDEX);
    }
    return db;
  }

  return {
    "chat.message": async () => {
      try {
        const cwd = process.cwd();
        const project = resolveProject(cwd);
        if (!project) return;

        const database = getDb();
        database.run(INSERT, [localTimestamp(), project, cwd]);
      } catch {
        // Silent — worklog must never disrupt the session
      }
    },

    dispose: async () => {
      if (db) {
        db.close();
        db = null;
      }
    },
  };
};
