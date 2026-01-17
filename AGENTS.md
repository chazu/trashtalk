# Agent Instructions

This project uses **bd** (beads) for issue tracking. Run `bd onboard` to get started.

**Read CLAUDE.md first** - it contains critical architecture information, invariants, and patterns you must understand before making changes.

## Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --status in_progress  # Claim work
bd close <id>         # Complete work
bd sync               # Sync with git
```

## Key Invariants (from CLAUDE.md)

1. **Primitive classes** (`pragma: primitiveClass`): ALL methods must be `rawMethod`/`rawClassMethod`, must have semantic parity between bash and native Procyon
2. **Non-primitive classes**: ZERO raw methods allowed, pure DSL only
3. **Current goal**: Convert all non-primitive classes to pure Trashtalk that executes fully in native mode

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
