# rich (repo-local)

This repository uses a **repo-local** `rich/` directory (gitignored) to manage an incremental taskgraph and to run work in a controlled, auditable way.

## Layout

Inside the repo root:

- `rich/taskgraphs/<repo_id>.yaml` — full taskgraph (generated from `rich/specs/<repo_id>_spec.md`).
- `rich/state/<repo_id>.yaml` — runner state (done tasks, CI gating, pinned tasks).
- `rich/audit.jsonl` — append-only event log (task complete/failed, CI polls).
- `rich/reports/<tick_ts>.md` — per-run markdown reports.
- `rich/outcomes/<tick_ts>/<repo_id>.json` — worker outcome JSON for a tick.
- `rich/logs/<tick_ts>.log` — raw OpenClaw worker transcript for a tick.

The engine code lives in `rich/engine/rich/*.py`.

## Execution model (Cascade-style)

Each `rich tick` does **exactly one** unit of work:

1. Load taskgraph + state.
2. Select the next eligible task (respecting dependencies and pinned/CI-fix tasks).
3. Ensure the repo is clean and up to date on `origin/<default_branch>`.
4. Spawn a **fresh isolated worker agent session** for the selected task.
5. The worker performs *all* repo edits, runs acceptance checks, makes **one commit**, and pushes to `origin/<default_branch>`.
6. The worker writes an outcome JSON to `rich/outcomes/<tick_ts>/<repo_id>.json`.
7. The runner verifies the push (fetches `origin/<default_branch>` and checks the SHA), then marks the task done and writes audit + report.

If the worker cannot finish, it must write `status=BLOCKED` and must not push.

## CI gating

Tasks ending with `.END` transition the runner to `AWAITING_CI`.

While `AWAITING_CI`, ticks will poll GitHub Actions via `gh api`. If CI is red, the runner pins a synthetic `CI.FIX.<milestone>` task to be executed next.

## Running

From the repo root:

```bash
./rich/bin/rich tick --repo-id chlaa
# or
./rich/bin/rich run-loop --repo-id chlaa
```

`run-loop` is a conservative sleep-loop (not systemd). It enforces cooldowns and caps task pushes per hour.
