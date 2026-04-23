# Agent Instructions

**Read CLAUDE.md first** - it contains critical architecture information, invariants, and patterns you must understand before making changes.

## Key Invariants (from CLAUDE.md)

1. **Primitive classes** (`pragma: primitiveClass`): ALL methods must be `rawMethod`/`rawClassMethod`, must have semantic parity between bash and native Procyon
2. **Non-primitive classes**: ZERO raw methods allowed, pure DSL only
3. **Current goal**: Convert all non-primitive classes to pure Trashtalk that executes fully in native mode
