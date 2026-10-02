<!--
Agent prompt for a boy-scout cleanup pass. Paste everything below this comment.
Tips: add a budget ("stop after ~15 commits or when remaining items are all
medium risk"), run it on a branch or worktree, and turn the "Patterns noticed"
section into lint rules, helpers, or CLAUDE.md entries afterward.
-->

# Mission: Boy-scout this codebase

Leave this codebase measurably better than you found it. Look for many small,
safe improvements, especially ones that **compound**: each one should make
later work (by people or agents) faster, safer, or clearer. You are not
rewriting anything. You are a careful maintainer on a cleanup pass.

## What "compounding" means here
Rank work by how often its benefit recurs, not by how impressive it looks.
High-leverage targets, roughly in priority order:

1. **Traps that cost time over and over.** Misleading names, comments that no
   longer match the code, error messages that don't say what went wrong or how
   to fix it, silent failures, footguns that keep getting worked around.
2. **Duplication of knowledge.** The same logic, constant, regex, or path
   written in 3+ places. Consolidate only when the copies really mean the same
   thing, not when they just look alike.
3. **Feedback loops.** Slow, flaky, or noisy tests; missing tests for code
   paths that clearly break often (look at git log for repeated fixes);
   build or lint steps people have to remember to run by hand.
4. **Discoverability.** Missing or wrong docs at the point of use, a stale
   README or CLAUDE.md, undocumented env vars and config, dead links between
   docs.
5. **Dead weight.** Unused code, stale flags, commented-out blocks, leftover
   workarounds for bugs that have since been fixed. Only remove something
   after you've shown it's actually unreferenced (see the rules below).
6. **Local clarity.** Overlong functions with an obvious seam, deep nesting
   that an early return would flatten, unclear variable names in hot paths.

Rank these lowest: pure style preferences, reformatting, and reshuffling that
moves code around without making anything easier.

## Process
1. **Orient first (no edits).** Read the top-level docs, contributor or agent
   instruction files, the build and test setup, and `git log --oneline -100`.
   Find out how to build and test, and run the full suite once to record a
   baseline. Note any tests that already fail; they are not yours to fix
   unless trivially related.
2. **Survey broadly.** Skim every major area before changing any of them.
   Mine history for signal: files that change most often, commits titled
   "fix"/"again"/"workaround", TODO/FIXME/HACK markers, and recurring review
   remarks. Hotspots are where compounding fixes live.
3. **Build a ranked list.** For each candidate, note: location, problem,
   proposed change, why it compounds, risk (low/med/high), and how you'll
   verify it. Drop anything you can't verify.
4. **Execute low-risk, high-leverage items first**, one logical change at a
   time:
   - Make the change.
   - Run the relevant tests (and the full suite periodically).
   - Commit with a message that states the *why*, one concern per commit.
5. **Stop and report** when the remaining items are medium or high risk, need
   a design decision, or would take you outside the improvements described
   above.

## Rules (non-negotiable)
- **Behavior-preserving by default.** If a change alters observable behavior
  (output, exit codes, APIs, file formats, persisted data), don't make it.
  List it as a proposal instead, unless it's an unambiguous bug fix with a
  test that proves it.
- **Respect intentional decisions.** If code looks odd, check the comments,
  docs, design notes, and git blame before "fixing" it. Odd-but-deliberate
  code gets a clarifying comment, not a rewrite.
- **Match local conventions.** Follow the naming, idioms, comment density, and
  error-handling style of the surrounding code. Don't introduce new
  dependencies, frameworks, or patterns.
- **Prove before deleting.** Search for every way the code could be reached:
  dynamic dispatch, string-built names, reflection, config, scripts, docs,
  tests, other repos. If you can't prove it's dead, leave it and list it.
- **No speculative abstraction.** Don't create helpers, layers, or config for
  needs that don't exist yet. Three real duplicates justify a helper; two
  usually don't.
- **Small diffs.** Keep each commit under ~100 changed lines where you can.
  Never mix a refactor with a behavior change.
- **Tests stay green.** Never weaken, skip, or delete a test to get a pass.
  If a test is wrong, say so and leave it for review.
- **Don't touch:** generated or compiled output, vendored code, lockfiles,
  migrations, or anything with uncommitted changes you didn't make.

## Deliverable
When done, report:
1. **Done:** each commit with a one-line why, grouped by category above.
2. **Proposed, not done:** ranked list of medium/high-risk or behavior-changing
   items, each with evidence and a suggested approach.
3. **Patterns noticed:** recurring problems that point to a systemic fix (a
   missing helper, a doc, a lint rule, a test harness gap), the items that
   would compound the most if someone tackled them next.
4. **Baseline vs. final test results**, plus any already-failing tests you
   left alone.

## Repo-specific notes
- Build/test: `make` then `make verify`. Use `TRASH_TEST_TIMEOUT=300`.
  `test_bytecode_blocks.bash` already fails; it's not a regression.
- Dispatch is dynamic via `@`. A method `foo:bar:` compiles to `__Class__foo_bar`
  and is invoked by selector string. Before calling anything unused, grep for
  the selector (`foo:`), the compiled name, and `bin/trash-send` usage.
- Never edit `trash/.compiled/*`. Edit `.trash` sources and rebuild.
- Prefer `method:` over `rawMethod:` where the DSL now covers it (see CLAUDE.md
  intrinsics). Converting raw methods that only exist because of now-fixed
  compiler limits is a high-value target. Check docs/COMPILER_CAPABILITIES.md.
- Known compiler pitfalls (hyphenated identifiers, `]]` on one line, ANSI-C
  quoting) are documented in LANGUAGE.md. Don't "simplify" code that's shaped
  around them.
- Design docs are in `docs/`; start at docs/README.md to tell current designs
  from historical ones.
