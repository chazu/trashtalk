# Agent Instructions

**Read CLAUDE.md first** - it contains critical architecture information, invariants, and patterns you must understand before making changes.

## Key Invariants (from CLAUDE.md)

1. **Primitive classes** (`pragma: primitiveClass`): ALL methods must be `rawMethod`/`rawClassMethod`, must have semantic parity between bash and native Procyon
2. **Non-primitive classes**: ZERO raw methods allowed, pure DSL only
3. **Current goal**: Convert all non-primitive classes to pure Trashtalk that executes fully in native mode

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **Run quality gates** (if code changed) - Tests, linters, builds
2. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   git push
   git status  # MUST show "up to date with origin"
   ```
3. **Clean up** - Clear stashes, prune remote branches
4. **Verify** - All changes committed AND pushed
5. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
