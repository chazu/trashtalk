# Agent Instructions

Read `CLAUDE.md` first. It contains the compiler and runtime architecture,
build commands, and established Trashtalk patterns.

## Project Direction

1. Trashtalk is a Bash-only language and runtime. The jq compiler is canonical.
   Do not reintroduce the retired native Procyon compiler, plugin mode, `tt`
   daemon, or Bash/native parity requirements.
2. Prefer `method:` and `classMethod:` and express behavior in the Trashtalk DSL
   wherever the DSL can state it clearly.
3. Use `rawMethod:` and `rawClassMethod:` only at real Bash/OS boundaries or
   where the current DSL cannot express the behavior. Keep raw methods small,
   isolate them behind a DSL-facing abstraction, and document why raw code is
   required.
4. When the same raw pattern is needed more than once, or substantial domain
   logic would have to be raw, consider extending the DSL/compiler or adding a
   reusable primitive before duplicating Bash.
5. Preserve semantic compatibility with the existing Bash runtime and validate
   compiler changes against the repository test suite.
6. Keep agent integrations as narrow external CLI adapters behind `Agent`.
   `TRASHTALK_AGENT_BACKEND` selects the one-shot backend (`axe` by default or
   `codex`). The Codex backend is subscription-oriented: require ChatGPT CLI
   authentication, strip API-key variables, and retain its ephemeral read-only
   execution boundary.

## Current Goal

Deepen Trashtalk as a self-contained Bash DSL: move avoidable raw behavior into
the language, keep unavoidable shell integration narrow, and build tooling on
the public message-send surface.
