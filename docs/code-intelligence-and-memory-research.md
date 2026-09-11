# Code intelligence and persistent memory for Trashtalk

## Recommendations

**Start code intelligence with a small `Tools::Roam` adapter, complemented by
`Tools::AstGrep`. Evaluate Hindsight first for learned personal and shared
repository memory, with Basic Memory as the explicit-note baseline and
Supermemory Local as the lighter service alternative.** These are recommendations
for subsequent experiments, not installed integrations or measured winners.

| Need | First choice | Reason | Alternative and decision trigger |
|---|---|---|---|
| Understand code relationships and likely change impact | Roam | Local graph, machine-readable CLI, explicit index control | GitNexus if its graph adds measurable value and its license fits the intended use; Serena when language-server resolution matters more |
| Find structural code patterns | ast-grep | Small standalone executable with structured output | Keep `rg` for text and unsupported languages |
| Navigate Bash, Go, Rust, and Zig symbols | Serena evaluation | Uses language servers and explicitly documents these languages | Existing language-specific tools where maintaining an MCP service is excessive |
| Find code from a description of its purpose | grepai, only if needed | Semantic search with local embedding options | Test against Roam/GitNexus search before adding another index |
| Retain experience and retrieve useful learned facts | Hindsight evaluation | Explicit retain/recall/reflect operations and scoped memory banks | Supermemory Local if a smaller deployment proves equally reliable |
| Keep exact, inspectable personal and repository notes | Basic Memory baseline | Markdown is the source; CLI supports reading, editing, and search | Versioned Markdown plus `rg` if the extra index adds little value |
| Query changing relationships over time | Graphiti, later | Temporal fact graph and episode provenance | Hindsight until concrete temporal queries justify another service |

The provider findings and sources below support this ranking. It weights
local/self-hosted operation, a narrow Bash boundary, explicit scope, provenance,
and freedom to change harnesses above the number of advertised features.
**One memory engine can support both individual and collective memory.** Separate
stores or namespaces should express ownership; separate vendors are unnecessary.
Do not adopt Hindsight, Basic Memory, and Supermemory simultaneously as competing
authorities for the same facts.

## Scope and evidence

This assessment covers publicly accessible official documentation, repository
source, licenses, and release metadata available on **September 10, 2026**.
Capabilities described in current documentation or a repository's default branch
may be newer than its latest packaged release. The release snapshot below is a
pinning aid, not a claim that every described feature exists in that release.
No candidate has been installed, benchmarked, or qualified against Trashtalk in
this assessment.

Trashtalk already has durable `AgentIdentity`, temporary `AgentSession`, and
persisted Assignment behavior. Its `Tool` subclasses expose class methods around
external programs. Provider execution belongs there; repository identity, memory
ownership, and assignment authority belong in domain objects. Relevant local
sources are [Tool](../trash/Tool.trash), [Trash](../trash/Trash.trash), and the
[delegation implementation plan](agent-delegation-implementation.md).

Three different kinds of information need different treatment:

| Information | Example | Scope and authority |
|---|---|---|
| Current code intelligence | Callers of a function in this checkout | A particular local copy/worktree and indexed source snapshot; validate against source |
| Durable knowledge | Why a migration uses a particular order | Personal or shared repository memory, with evidence and applicability |
| Execution state | Assignment owner, current session, pending question | Trashtalk's persisted domain objects and messaging plane |

A memory service should not decide whether an Assignment is complete. A graph
index should not turn a branch-specific implementation detail into a universal
repository fact. An agent's persistent identity should survive replacing either
its harness or its memory provider.

## Code intelligence

### Comparison

| Candidate | Primary capability | Bash integration | Local requirements | Important limit |
|---|---|---|---|---|
| Roam | Symbol/dependency graph, context, impact, analysis | JSON CLI; MCP also available | Python, parsers, SQLite index | Parser coverage and static resolution vary; no established `.trash` semantics [^1][^2] |
| GitNexus | Code graph, communities, execution flows, hybrid search | Direct CLI commands; MCP and local HTTP options | Node, native parsers, LadybugDB | Noncommercial license; parser graphs do not prove runtime behavior [^4][^5][^6] |
| Serena | Symbol navigation, references, structured edits | Primarily MCP/programmatic tools | Python service plus language servers or JetBrains backend | More service/project lifecycle; capability varies by language server [^7][^8] |
| grepai | Semantic code retrieval and call tracing | JSON CLI and MCP | Go executable, index/watch process, embedding provider | Embedding retrieval is not exact symbol resolution [^10][^11] |
| ast-grep | Syntax-aware search and rewriting | Standalone CLI with JSON | Executable and supported grammar | AST pattern matching, not a repository-wide semantic graph [^12][^13] |
| SCIP ecosystem | Portable semantic symbol/reference indexes | Wrap indexers and a query consumer | Language-specific indexers; often build configuration | Protocol and ecosystem, not one ready-to-wrap intelligence CLI [^14] |

### Roam: best first graph adapter

Roam builds a local SQLite code graph and exposes queries for symbols, context,
dependencies, impact, and related analysis. Its documented first-class language
list includes Go and Rust. Bash and Zig are not established as equivalent
first-class support in that table; a generic parser fallback should not be
represented as full call-resolution support. Local static indexing does not
require an LLM account. [^1]

The adapter-friendly part is explicit control. `roam index` builds an index;
`roam init` also initializes project configuration. Global `--json` selects
machine output. `ROAM_DB_DIR` relocates the index and associated state, while
`ROAM_NO_AUTO_INDEX=1` makes an unprepared query fail rather than silently start
indexing. These allow a wrapper to distinguish preparation from a bounded read.
They do not by themselves prove that sharing an index between divergent
worktrees is safe. [^2][^3]

Recommended initial surface: prepare/status, find symbol, get context, and
inspect likely impact. Keep an index for each selected worktree/snapshot and
return its freshness information with results. Roam has expanded into agent
coordination, memory, and run management too; leave those features outside this
adapter because Trashtalk already owns those responsibilities. [^1]

Roam's Apache-2.0 license and CLI make it the most straightforward first graph
experiment. Its breadth is also a maintenance concern: pin a version and qualify
the few commands actually exposed. “Impact” should mean a set of relationships
worth investigating, not a guarantee that everything affected was found. [^3][^15]

### GitNexus: credible competitor, with an adoption constraint

GitNexus combines parsed code relationships with graph communities, execution
flows, and search. Current documentation names **LadybugDB** as its embedded
database; descriptions that simply say Kuzu are outdated for the inspected main
branch. Its supported-language list includes Go, Rust, and Zig. Bash and
Trashtalk are not listed as equivalent supported languages. More advanced
dependence analysis is language- and mode-specific. [^4]

There is a genuine terminal integration path: `query`, `context`, `impact`,
`trace`, and related commands call the local backend directly. The CLI source
also exposes repository selection and branch selection. `analyze --index-only`
suppresses injection into `AGENTS.md`, `CLAUDE.md`, and skills; this is the mode
a Trashtalk adapter should use. A branch selector is useful, but does not prove
correct handling of two independently dirty worktrees. [^5]

The significant constraint is its current **PolyForm Noncommercial 1.0.0**
license. It should not be described as interchangeable with MIT/Apache projects
for adoption. Resolve suitability for the intended deployment before selecting
it as a default dependency. This assessment records the published license rather
than making a legal determination about a particular use. [^6]

GitNexus merits a bounded comparison if its flow-oriented results outperform
Roam on real investigations. Its local registry also means the adapter should
select an explicit repository identity instead of relying on the current default
or a checkout basename. Validate output contracts command by command; do not
assume every subcommand accepts the same JSON flag. [^5][^16]

### Serena: strongest reason to add an MCP bridge

Serena obtains code knowledge through language servers, with an alternative
JetBrains integration. Its documented coverage includes Bash, Go, Rust, and
Zig, using their corresponding language tooling. This makes it especially
relevant to a workspace spanning those languages. Resolution quality still
depends on the server, project setup, and the operation requested. [^7][^8]

The integration tradeoff is lifecycle. Serena is primarily a persistent MCP
tool service, not a set of independent one-shot query commands. Projects must
be activated/configured, language servers started, and failures distinguished
from empty results. A shared service's mutable active-project state must not
route concurrent agents into the wrong checkout; use dedicated project contexts
or qualify explicit per-request selection. [^9]

Recommend it when “find actual references to this symbol” consistently defeats
the graph tools, especially for Bash or Zig. Start with navigation. Its edit
tools are a separate capability to evaluate later. Project notes supplied by a
code-navigation server are useful, but are not sufficient evidence of the
shared, independently scoped memory service Trashtalk requires. [^7][^9]

### grepai, ast-grep, and SCIP

**grepai** answers natural-language code questions with embeddings and provides
call tracing. Its CLI has JSON output and explicit workspace/project filtering.
The documented local-provider options make a local-only deployment possible,
but configuring a remote embedding provider changes that property. Prefer one
selected repository/project in an adapter; a multi-project workspace can
otherwise deliberately broaden retrieval. Adopt it only if semantic searches
find useful entry points missed by the first graph tool and ordinary search.
[^10][^11][^17]

**ast-grep** is a small, complementary wrapper. It matches syntax patterns and
can emit structured locations; its JSON documentation specifies zero-based
positions, which must be normalized for Trashtalk's presentation. Bash, Go, and
Rust grammars are useful here. Do not assume built-in Zig or `.trash` support.
Keep search separate from rewrite operations, and describe matches as syntactic
rather than type-resolved references. [^12][^13]

**SCIP** is a language-independent format for semantic indexes, with separate
indexers for different languages. It becomes attractive if portable, precise
cross-repository reference indexes are a demonstrated need. Adopting it now
would require both indexing and query infrastructure; it is a later architectural
option rather than the first Tool class. [^14]

### Trashtalk and worktrees

None of these sources establishes native understanding of Trashtalk's class,
trait, selector, or message-send semantics. The compiler-backed `Trash
symbolRecords`, `implementorsOf:`, and `sendersOf:` operations already provide
the right starting point. External tools can inspect the Bash runtime and other
languages; generated Bash is not a substitute for a source-level `.trash` index.

For every external code index, record logical repository ID, local-copy ID,
worktree ID/path, commit, dirty-state fingerprint, tool/parser version, and
index time. If files change during indexing, either retry or mark the result
as an inconsistent snapshot. Never share one mutable index across divergent
checkouts merely because their remotes match. Whether a provider's own branch
or worktree features meet this contract remains an acceptance-test question.

## Persistent memory

### Memory types and ownership

| Type | Example | Proposed scope | Suitable approach |
|---|---|---|---|
| Personal semantic memory | An agent's durable working preferences | `AgentIdentity` | Basic Memory; Hindsight/Mem0 if extraction is useful |
| Episodic memory | An investigation, its evidence, and its outcome | Author plus repository/Assignment associations | Hindsight; Supermemory; Graphiti episodes |
| Shared repository knowledge | A build prerequisite or architectural decision | Stable logical `Repository` | Any qualified provider with explicit scope and correction |
| Procedural memory | A tested release procedure | Repository or explicitly shared collection | Versioned notes/skills, optionally indexed |
| Temporal relational knowledge | A component depended on one service before a migration | Repository plus validity interval/evidence | Graphiti; evaluate other providers against actual queries |
| Working context | Current prompt and intermediate reasoning | Session/run | Harness context and Trashtalk records; not automatically durable memory |

These categories describe requirements, not six new domain classes. A personal
store can be reused across an identity's sessions. A repository store can be
read by several specialists and survive their retirement. An individual's
repository-specific observation can remain private until explicitly published
to the collective store.

### Comparison

| Candidate | Memory model | Tool boundary | Persistent operation | Assessment |
|---|---|---|---|---|
| Hindsight | Extracted experience/facts, recall, reflection | JSON CLI, REST, MCP | Service backed by PostgreSQL; local/cloud | First learned-memory experiment [^18][^19] |
| Basic Memory | Markdown notes, observations, relations | CLI, MCP | Files plus local index; optional cloud | Best exact-note baseline [^23][^24] |
| Supermemory Local | Documents, extracted memories, profiles, graph search | Local HTTP API | Single server binary with embedded engine | Strong lighter-service challenger [^27] |
| Mem0 | Extracted and explicitly stored memories with entity scope | OSS SDK/server; Platform CLI/API | Configurable local stores or hosted platform | Good conventional personalization alternative [^31][^32][^33] |
| OpenViking | Hierarchical context filesystem, resources, session-derived memory | `ov` CLI, HTTP, SDK | Server plus embedding/extraction configuration | Promising broader context system; larger scope [^35][^36] |
| Letta | Agent-owned Git memory and shared memory repositories | Agent CLI/App Server/SDK | Agent runtime; cloud shared repositories | Better considered as a harness decision [^40][^41][^42] |
| Cognee | Document pipelines, vector retrieval, knowledge graph | CLI, REST, SDK | SQL/vector/graph providers | Stronger fit for a larger document knowledge system [^44][^45] |
| Graphiti / Zep | Temporal relationships and episodes | Graphiti library/MCP; Zep hosted API | Graph backend or managed service | Later temporal knowledge option [^49][^50] |

### Hindsight: first learned-memory experiment

Hindsight separates **retain** (ingest experience), **recall** (retrieve relevant
memory), and **reflect** (synthesize an answer using memory). Its design includes
world facts, experiences, and consolidated observations. An adapter can therefore
offer retrieval without silently converting every query into an additional
generative reasoning step. The project is MIT-licensed and supports self-hosting
as well as a managed service. [^18]

Memory banks supply an explicit scope. Use separate stable bank IDs for an
agent's private knowledge and a repository's collective knowledge. Metadata,
tags, and temporal information can preserve additional context. A bank name
is a routing boundary; Trashtalk must still control which identities may access
it. Do not infer permissions merely from possession of an identifier. [^20]

The CLI exposes JSON output and document/operation inspection alongside memory
commands. Retention can be asynchronous, so “accepted” and “available for recall”
must remain distinct outcomes. The retain documentation says input is transformed
into extracted facts rather than preserved verbatim, and reusing a document ID
replaces/reprocesses its previous content. Preserve original evidence in its
authoritative source and retain a resolvable reference. Do not use repeated
retention under one document ID as an append operation. [^19][^21]

For Trashtalk, the attraction is a narrow API that can serve both memory scopes
without becoming the agent harness. The cost is a PostgreSQL-backed service and
model work during extraction/consolidation, plus optional reflection. “Embedded”
or a single Docker command simplifies packaging; it does not make this an
ordinary local text file. [^18][^22]

Hindsight's coding-agent integration already has worktree-aware bank naming,
but its project-derived convention is not Trashtalk's durable logical repository
identity. Supply our own bank IDs. Do not install automatic transcript-ingestion
hooks merely to obtain the underlying memory operations. [^22]

**Decision:** evaluate explicit retain and bounded recall first. Add reflection
only after retrieval, evidence preservation, correction, and deletion pass.
If extraction loses important qualifiers, keep curated facts as exact records
instead of accepting a plausible paraphrase as truth.

### Basic Memory: exact notes with minimal infrastructure

Basic Memory stores knowledge in Markdown and builds a searchable local index.
Projects organize collections; notes can contain observations and relationships.
This is well suited to durable procedures, explanations, and facts that a human
should be able to inspect or edit without the original provider. Its current
license is AGPL-3.0, not an assumed permissive license. [^23][^25]

The CLI supports writing, reading, editing, deleting, and searching notes, with
project selection and structured-output facilities. Explicit project IDs are
preferable to mutable default-project selection. An adapter could expose those
operations with little service management. The note format also offers a natural
place for source Assignment, author, revision applicability, and supersession
metadata. [^24][^26]

Its strongest property is fidelity: an explicitly written note stays inspectable
as authored. Automatic distillation is work for the writing agent or an optional
pipeline, rather than an inherent promise of storing a note. Full-text search
can start without a model; optional semantic search introduces embedding and
possibly reranking dependencies. [^23][^24]

For collective memory, choose one authoritative project location per repository.
Multiple agents need coordinated writes; shared files or Git synchronization
alone do not establish conflict-free concurrent edits. Qualify overwrite guards,
revision checks, and recovery. For multiple machines, distinguish syncing durable
notes from syncing their derived indexes.

**Decision:** use as the baseline against which learned-memory services must
justify extraction cost and operational complexity. If explicit facts and notes
cover the first use case, it may be the first actual memory adapter. Plain
versioned Markdown plus `rg` remains a valid lower-complexity baseline.

### Supermemory Local: a materially stronger option than older comparisons suggest

Current official documentation describes a self-hosted **single binary** with
an embedded graph engine, local embeddings, and the same main memory HTTP API
as the hosted platform. It accepts an external model for extraction, including
a locally served compatible model. It is therefore inaccurate to dismiss
Supermemory as hosted-only. Its repository currently declares MIT. [^27][^30]

The local/Enterprise distinction matters. Local has one organization and one
API key; Enterprise adds organizational access controls and managed infrastructure.
Hosted extraction uses proprietary models, while local extraction uses the
configured model. API compatibility consequently does not establish equal
retrieval quality or equal isolation controls. [^28]

Configuration documents a persistent data directory, noninteractive model
settings, and queued ingestion. A successful document submission may still be
waiting for extraction and indexing. Container tags can represent personal and
repository collections, but a local shared credential places access enforcement
in the Trashtalk boundary. Avoid exposing it directly to identities that must
not read one another's private memory. [^29][^57]

**Decision:** include it in the memory bakeoff alongside Hindsight. It may win
on deployment simplicity for a single operator's local agent system. Hindsight
currently gets the first evaluation slot for its explicit recall/reflection
separation and inspectable document/operation workflow; that is an integration
judgment, not evidence of superior memory accuracy.

### Mem0: clean memory APIs, with an OSS/Platform distinction

Mem0 provides memory creation, search, updates, deletion, and history, with
user/agent/run identifiers and metadata. Its Apache-2.0 OSS package allows
configurable model and storage providers. Current documentation also describes
a self-hosted REST server and dashboard, so the older characterization “Python
library only; build the service yourself” is incomplete. [^31][^32]

The official terminal client is documented for **Mem0 Platform**. Its agent mode
provides JSON output and noninteractive behavior; `--no-infer` supports literal
storage rather than extraction. Do not assume that this client and the OSS
server are wire-compatible simply because both carry the Mem0 name. For an OSS
experiment, pin and wrap the documented self-hosted API or a small stable CLI
facade over the SDK. [^33]

Defaults are deployment-specific: the library overview documents a local Qdrant
path under `/tmp` and a SQLite history file, while the self-hosted server setup
uses PostgreSQL/pgvector. A persistent-memory installation must choose durable
paths and volumes explicitly. Remote model defaults also mean self-hosting the
database alone does not keep content local. [^31][^32]

For shared facts, always include a repository scope in writes and searches;
`run_id` is too short-lived and a broad user scope is too wide. Treat extracted
updates as proposed knowledge changes with evidence, not indisputable truth.
Mem0 is a good alternative if its simpler CRUD model or hosted service is
preferred. It is less directly aligned with the desired experience/recall
workflow than Hindsight, but could win an empirical comparison.

### OpenViking and VikingMem: distinguish the products

OpenViking organizes resources, memories, and skills in a virtual context
filesystem. Hierarchical abstraction levels let retrieval move from short
overviews toward detailed content. It includes a real `ov` CLI with JSON output,
backed by its server, rather than requiring a harness-specific plugin. This is
appealing for a general context store. [^35][^36]

Scope is richer than one arbitrary collection string. Current concept docs
describe user and peer memory and account-level resources/capabilities; URI
resolution can depend on authenticated identity. An adapter must map
AgentIdentity and Repository explicitly into this policy. Do not copy older
examples that assume every memory belongs under one agent-memory path. [^37][^38]

Session commit archives context and initiates asynchronous extraction. That
operation is not a synchronous guarantee that new facts are available. It also
encourages a session-oriented ingestion model; Trashtalk should start with
selected observations rather than hand over entire conversations by default.
The server is currently AGPL-3.0; the Rust CLI is separately Apache-2.0.
[^39][^58]

**VikingMem** also names a Volcengine/VikingDB memory offering documented with
its own SDK and service endpoint. Treat this as a separate deployment/API
candidate, not an alias guaranteeing compatibility with OpenViking. Regional
availability and account access would need qualification before a hosted trial.
[^59]

**Decision:** keep OpenViking on the shortlist if one hierarchical store for
documents, resources, skills, and learned memory becomes the requirement. For
the first repository-fact slice, its context model adds more integration choices
than Hindsight or an explicit-note store.

### Letta: valuable memory design, a broader integration commitment

Letta has changed substantially. The `letta-ai/letta` repository now directs
active development to `letta-ai/letta-code`; its old releases describe the
retired V1 server. Current App Server documentation recommends an Agent SDK over
a WebSocket protocol, with compatibility surfaces alongside it. A new adapter
should not be designed solely from historical MemGPT/shared-block examples.
[^40][^43]

Current agents use **MemFS**, a Git-backed Markdown memory filesystem. Selected
files stay in context; other memory is retrieved through ordinary file tools.
Semantic search is optional rather than inherent. Git gives memory a concrete
history and synchronization boundary, and local agents remain responsible for
their local memory backups. [^41]

Current shared-memory documentation describes organization-owned Git
repositories attached to **cloud-hosted agents**. Agents commit/push and other
agents pull; local agents use their own MemFS and project files. The docs
explicitly recommend migration from legacy shared memory blocks to shared
repositories. This is an excellent model to study, but not a general claim that
an arbitrary local Jcode process gets Letta collective memory through one call.
[^42]

**Decision:** consider Letta when choosing an agent harness or deliberately
adopting its agent runtime. A memory-only Tool should not accidentally introduce
a second owner of identity, conversations, and background agent execution.
The older standalone AI Memory SDK also needs a current compatibility check
before being selected as the bridge. [^60]

### Cognee: document and knowledge-graph infrastructure

Cognee combines document ingestion, graph construction, and vector retrieval.
It exposes CLI/API/SDK surfaces, and recent documentation includes
remember/recall/forget operations alongside the established add/cognify/search
pipeline. This is a wider knowledge-processing system than a small persistent
fact store. [^44][^45][^46]

Its datasets and permission model can support multiple users and collections,
but the chosen SQL/vector/graph providers affect isolation and deployment.
Current permissions documentation explains provider compatibility and per-user
or dataset storage behavior. A basic single-user example is insufficient
evidence for concurrent agents with separate private and shared knowledge.
[^47]

The CLI can ingest named datasets and run graph processing; processing may be
background work. Forgetting and rebuilding also need to follow the provider's
documented lifecycle. Pin the API generation and test deletion of derived data,
rather than equating removal of an input document with removal of every fact
produced from it. [^45][^48]

**Decision:** defer until repository memory includes substantial design documents,
external knowledge, and relationship queries that simpler stores cannot answer.
Its Apache-2.0 license is attractive; model configuration and several storage
roles are the greater operational cost. [^44]

### Graphiti and Zep: temporal knowledge when it earns its complexity

Graphiti constructs a temporal graph from episodes and represents changing
relationships. Its current supported backend choices include Neo4j and FalkorDB;
the README warns that the Kuzu adapter is deprecated. The project currently
declares Apache-2.0. Historical licensing/backend summaries should be rechecked
before using them to select a deployment. [^49][^61]

Its MCP server exposes graph operations and grouping, providing a usable bridge
for agents. Explicit `group_id` selection can map to a repository store. The
integration is nevertheless a graph service/library boundary rather than a
universally documented one-shot JSON query CLI. [^50]

Zep is the managed product in this family. Its official FAQ says the old
self-hosted Community Edition is deprecated; Graphiti is the current open-source
alternative, while Zep Cloud and enterprise BYOC are separate offerings. [^51]

**Decision:** evaluate when queries such as “what was true before this migration,
and which evidence invalidated it?” become important. For a handful of build
facts, a provenance field and explicit supersession are a cheaper starting point.

## Integration shape

### Keep the public API common

The wrapper should expose a few domain-relevant operations and return normalized
results. It should not publish every upstream command as part of Trashtalk's
stable interface. Prefer CLI JSON when available; use a narrow HTTP or MCP
boundary when that is the provider's actual interface. Bash remains the runtime;
it need not implement embedding models or language servers itself.

| Boundary | Proposed responsibilities |
|---|---|
| `Tools::Roam`, `Tools::AstGrep`, selected memory Tool | Version/detection, explicit configuration, invocation, parsing, bounded output, errors |
| Repository/local-copy/worktree objects | Durable identity, checkout selection, allocation, Git lifecycle, index association |
| Memory-store object | Provider endpoint/collection association, owner, scope, permitted operations |
| AgentIdentity | Personal-memory association and participation in shared stores |
| Assignment | Work authority, progress, outcome, provenance references |

Names here are provisional. `Repository`, `LocalRepository`, `Worktree`, and
memory stores are required future domain work, not implemented APIs. An index
is rebuildable derived data; a memory store contains durable knowledge and needs
backup/export. Those lifecycles should remain separate even if one vendor offers
both features.

An illustrative future message surface is:

```bash
# Proposed domain messages, not commands available today.
memory=$(@ "$repository" memory)
@ "$memory" search: 'how do we run the integration suite?'
@ "$memory" remember: 'The integration suite requires TEST_TOKEN.' \
  source: "$assignment"
```

The store derives scope from its persistent association; callers should not
repeat a vendor bank ID in every ordinary message. A proposed memory result
contains its ID, text, owner/scope, author, source references, observed time,
revision applicability, supersession state, and retrieval score if supplied.
Scores from different providers are not comparable probabilities of truth.

Writes need stable IDs, explicit completion status, and a way to inspect failures.
Retries must not multiply facts. Concurrent corrections need version checks or
append-and-supersede semantics. Private-to-shared publication should preserve
attribution and original evidence. Reading a fact does not make it a new
independent observation.

Do not automatically store entire transcripts. First retain selected, evidenced
observations, and provide read, correct, supersede, forget, and export operations.
Treat retrieved text as evidence to inspect, not instructions that grant tool
authority. Provider absence should produce an explicit unavailable result while
delegation and authoritative Assignment state continue to work.

### CLI feasibility

These documented command shapes illustrate why a Bash adapter is feasible;
they are not installation instructions or evidence of local qualification:

```bash
# Run in an explicitly selected checkout; prepare its index deliberately.
roam index
roam --json context 'parse_config'

# Structural search; JSON positions require normalization.
ast-grep run --lang bash --pattern 'source $FILE' --json=stream

# Selected memory bank; recall is distinct from generated reflection.
hindsight memory recall repo-example 'integration test setup' -o json

# An explicit project, avoiding accidental default-project selection.
bm tool search-notes 'integration test setup' --project repo-example
```

The underlying contracts are documented by the respective command references.
Actual adapters should pass argument arrays or correctly quoted arguments,
keep diagnostics off protocol stdout, set timeouts and result limits, and avoid
interactive setup during an agent request. [^2][^12][^19][^24]

## Releases, licenses, and operating cost

### Maintenance snapshot

The following public release tags were current in the inspected GitHub metadata.
Links point to the release being recorded. Licenses refer to the inspected
default-branch files and may require rechecking at the final pinned artifact.
An active release stream is maintenance evidence, not reliability evidence.

| Project | Observed release | Published | Current declared license / qualification |
|---|---|---|---|
| Roam | [v14.1.0](https://github.com/Cranot/roam-code/releases/tag/v14.1.0) | 2026-09-09 | Apache-2.0 [^15] |
| GitNexus | [v1.6.11](https://github.com/abhigyanpatwari/GitNexus/releases/tag/v1.6.11) | 2026-09-04 | PolyForm Noncommercial 1.0.0 [^6] |
| Serena | [v1.7.0](https://github.com/oraios/serena/releases/tag/v1.7.0) | 2026-08-09 | MIT [^7] |
| grepai | [v0.37.0](https://github.com/yoanbernabeu/grepai/releases/tag/v0.37.0) | 2026-09-10 | MIT [^10] |
| ast-grep | [0.45.3](https://github.com/ast-grep/ast-grep/releases/tag/0.45.3) | 2026-08-31 | MIT [^13] |
| Hindsight | [v0.9.2](https://github.com/vectorize-io/hindsight/releases/tag/v0.9.2) | 2026-08-25 | MIT [^18] |
| Basic Memory | [v0.23.2](https://github.com/basicmachines-co/basic-memory/releases/tag/v0.23.2) | 2026-08-25 | AGPL-3.0 [^25] |
| Supermemory | [server-v0.0.8](https://github.com/supermemoryai/supermemory/releases/tag/server-v0.0.8) | 2026-08-17 | MIT repository; hosted features/models differ [^28][^30] |
| OpenViking | [v0.4.19](https://github.com/volcengine/OpenViking/releases/tag/v0.4.19) | 2026-09-08 | AGPL-3.0 server; Apache-2.0 Rust CLI [^58] |
| Cognee | [v1.5.4](https://github.com/topoteretes/cognee/releases/tag/v1.5.4) | 2026-09-04 | Apache-2.0 [^44] |
| Graphiti | [v0.30.2](https://github.com/getzep/graphiti/releases/tag/v0.30.2) | 2026-09-08 | Apache-2.0 [^61] |

Mem0's main Python manifest reports **2.0.20**. The repository's latest-release
entry points to a `pi-agent` integration release, which should not be mistaken
for the core library version. Letta's old `0.16.8` server release likewise should
not be used as the version of its current runtime. [^34][^40]

### Cost model

For local code tools, the initial costs are installation, cold indexing, disk,
RAM, incremental refresh, and maintenance. Roam's static path and ast-grep do
not require inference calls. Semantic indexes introduce embedding computation;
language-server tools introduce project setup and resident processes. These are
different costs from the tokens an agent later spends reading results.

For learned memory, measure ingestion/extraction, embeddings, consolidation,
retrieval, optional reflection/reranking, storage, and operator time separately.
Self-hosted storage does not make remote inference free or local. A hosted API
can be cheaper operationally for small workloads; a local model can reduce
external data transfer while increasing machine requirements. No aggregate
monthly estimate is credible without an ingestion and query volume.

Published hosted prices provide orientation, not equivalent packages:

| Service | Public pricing at the evidence date | Budget implication |
|---|---|---|
| Hindsight Cloud | Retain $10/million tokens; recall $0.75/million tokens; reflect $0.05/call; storage $0.25/million tokens/month after the first 30 days [^52] | Frequent reflection may dominate a small store's bill; track each meter |
| Mem0 Platform | Free tier: 10,000 adds and 1,000 retrievals/month; Starter $19/month for 50,000 adds and 5,000 retrievals; Pro $249/month [^53] | Retrieval allowance and feature tier matter more than stored-user count |
| Supermemory Platform | Free plan includes $5 monthly credits; Pro $19/month includes $20 credits; Max $100 includes $130; usage draws from credits [^54] | Measure the provider's billed units; do not equate them with another provider's token count |

Supermemory's deployment documentation separately offers a free local binary;
its commercial self-hosting/Enterprise language concerns a different product
scope. Do not infer that all self-hosting requires a paid plan, or that the local
binary includes every Enterprise control. [^27][^28][^54]

## The next bounded experiments

Vendor benchmark scores are not a selection result for this project. The
Hindsight and Zep papers provide useful memory designs and evaluations, but
different datasets, model choices, retrieval budgets, and metrics prevent a
simple league table from establishing the best repository-memory provider.
Correcting a branch-specific build fact is a different task from answering a
long conversation benchmark. [^55][^56]

### Code experiment

Start with Roam plus the existing compiler/`rg` baseline. Use small fixed tasks
from a Rust repository, a Go repository, a Zig repository, and Trashtalk's Bash
and `.trash` sources. Record exact commits and manually checked answers for
symbol location, callers, implementation entry points, and likely change impact.

Measure cold index time, one-file refresh time, warm query latency, peak memory,
index size, returned context size, and correct useful results. Report unsupported
languages as unsupported rather than assigning a misleading zero-accuracy score.
Add Serena for language-resolution failures; add GitNexus only if its licensing
and expected graph benefit justify that second evaluation. Test ast-grep on a
few patterns that text search cannot state accurately.

Before exposing an index to multiple agents, require:

1. Two linked worktrees and an independent clone cannot return one another's
   uncommitted changes accidentally.
2. Branch changes and edits produce a refreshed result or an explicit stale
   result; deleted symbols disappear after refresh.
3. Duplicate checkout basenames, spaces in paths, and interrupted indexing do
   not corrupt identity or silently select another repository.
4. No query unexpectedly writes instruction files, launches an interactive
   setup, publishes repository data, or starts unbounded background work.

### Memory experiment

Begin with **Basic Memory or plain notes as the baseline, and Hindsight as the
first learned provider**. Add Supermemory Local only if that comparison leaves
deployment cost or extraction quality unresolved. Use two agent identities,
two logical repositories, independent clones, and a fresh harness conversation
for recall. Store about 30 deliberately selected observations: exact facts,
qualified claims, procedures, investigation outcomes, and several corrections.

| Test | Required behavior |
|---|---|
| Fresh-session recall | A new session can retrieve an earlier observation without reusing the old conversation |
| Shared versus private | An authorized second identity reads repository facts; its access does not imply access to private agent memory |
| Repository isolation | The same question in another repository cannot retrieve the first repository's facts |
| Clone/worktree identity | Shared knowledge follows the logical repository; branch-qualified claims remain qualified |
| Fidelity | Negation, uncertainty, revision, and evidence survive storage and retrieval |
| Correction | New evidence supersedes an outdated claim; the old claim is not returned as current truth |
| Concurrent writes and retry | Two writers retain their contributions; retrying an accepted write does not duplicate it |
| Completion and restart | Pending extraction is visible and eventually completes or fails; persisted data survives restart |
| Forget/export | Deletion removes active retrieval and relevant derived records; export preserves useful content and provenance |
| Provider outage | Explicit unavailable status; Assignment and messaging operations still work |

Measure top-five useful recall, unsupported claims, evidence completeness,
ready-to-query ingestion latency, warm recall latency, and cost per successful
investigation. Run the same extraction model where supported, and report where
it cannot be held constant. Include one human review of the stored facts, not
just an LLM judge of fluent answers.

Proceed with a provider only if it preserves scope and correction behavior and
reduces repeated investigation enough to justify its operation. If explicit
notes perform just as well, ship the note-backed memory slice first. Repository
and worktree objects, persistent shared memory, and Tool adapters remain deferred
implementation work under the [delegation plan](agent-delegation-implementation.md).

## Sources

All undated documentation and default-branch sources below were accessed on
September 10, 2026. Release dates are recorded separately above. Sources are
maintainer documentation, published source, or original papers; product claims
are not independent runtime verification.

[^1]: Cranot. [Roam source and project documentation](https://github.com/Cranot/roam-code), current README: graph architecture, language coverage, and expanded agent features.
[^2]: Cranot. [Roam agent CLI contract](https://github.com/Cranot/roam-code/blob/main/docs/agent-cli.md), current main: JSON output, explicit indexing, and environment controls.
[^3]: Roam. [Command reference](https://www.roam-code.com/docs/command-reference), current documentation.
[^4]: GitNexus maintainers. [GitNexus project documentation](https://github.com/abhigyanpatwari/GitNexus), current README: architecture and language support.
[^5]: GitNexus maintainers. [CLI command definitions](https://github.com/abhigyanpatwari/GitNexus/blob/main/gitnexus/src/cli/index.ts), current main: direct queries, branch/repository flags, and index-only mode.
[^6]: GitNexus maintainers. [LICENSE](https://github.com/abhigyanpatwari/GitNexus/blob/main/LICENSE), PolyForm Noncommercial 1.0.0.
[^7]: Oraios. [Serena](https://github.com/oraios/serena), current project architecture, interfaces, and license.
[^8]: Oraios. [Programming language support](https://oraios.github.io/serena/01-about/020_programming-languages.html), current supported servers and limitations.
[^9]: Oraios. [Serena workflow](https://oraios.github.io/serena/02-usage/040_workflow.html), current project activation and usage documentation.
[^10]: Yoan Bernabeu and contributors. [grepai](https://github.com/yoanbernabeu/grepai), current project overview and license.
[^11]: grepai. [Configuration](https://yoanbernabeu.github.io/grepai/configuration/), current embedding and storage options.
[^12]: ast-grep maintainers. [CLI reference](https://ast-grep.github.io/reference/cli) and [JSON output](https://ast-grep.github.io/guide/tools/json), current command and position contracts.
[^13]: ast-grep maintainers. [Project source](https://github.com/ast-grep/ast-grep) and [rule configuration/languages](https://ast-grep.github.io/reference/yaml), current support and license.
[^14]: Sourcegraph. [SCIP](https://github.com/sourcegraph/scip), protocol, indexers, and license.
[^15]: Cranot. [Roam LICENSE](https://github.com/Cranot/roam-code/blob/main/LICENSE), Apache-2.0.
[^16]: GitNexus maintainers. [CLI package documentation](https://github.com/abhigyanpatwari/GitNexus/blob/main/gitnexus/README.md), current repository registry and index behavior.
[^17]: grepai. [Workspace management](https://yoanbernabeu.github.io/grepai/workspace/), project filtering and JSON examples.
[^18]: Vectorize. [Hindsight](https://github.com/vectorize-io/hindsight), architecture, deployment, memory operations, and license.
[^19]: Vectorize. [Hindsight CLI](https://hindsight.vectorize.io/sdks/cli), output formats and memory/document/operation commands.
[^20]: Vectorize. [Memory banks](https://hindsight.vectorize.io/developer/api/memory-banks), scope and configuration.
[^21]: Vectorize. [Retain API](https://hindsight.vectorize.io/developer/api/retain), extraction, document identity, and asynchronous behavior.
[^22]: Vectorize. [Coding-agent integrations](https://github.com/vectorize-io/hindsight/blob/main/hindsight-integrations/coding-agents/README.md), bank naming and worktree configuration; [storage](https://hindsight.vectorize.io/developer/storage), persistence architecture.
[^23]: Basic Machines. [Basic Memory](https://github.com/basicmachines-co/basic-memory), Markdown persistence, projects, and search.
[^24]: Basic Machines. [CLI reference](https://docs.basicmemory.com/reference/cli-reference), read/write/edit/search, project identity, and output options.
[^25]: Basic Machines. [LICENSE](https://github.com/basicmachines-co/basic-memory/blob/main/LICENSE), AGPL-3.0.
[^26]: Basic Machines. [Knowledge format](https://docs.basicmemory.com/concepts/knowledge-format), notes, observations, and relations.
[^27]: Supermemory. [Supermemory Local overview](https://supermemory.ai/docs/self-hosting/overview), local server, API, embeddings, and deployment.
[^28]: Supermemory. [Local versus Enterprise](https://supermemory.ai/docs/self-hosting/local-vs-enterprise), authentication, models, and organizational features.
[^29]: Supermemory. [Self-hosting configuration](https://supermemory.ai/docs/self-hosting/configuration), persistent paths, model providers, and ingestion queue.
[^30]: Supermemory. [Project source](https://github.com/supermemoryai/supermemory), local/hosted interfaces and declared license.
[^31]: Mem0. [Open-source overview](https://docs.mem0.ai/open-source/overview), package behavior and defaults.
[^32]: Mem0. [Open-source setup](https://docs.mem0.ai/open-source/setup), self-hosted server and storage configuration.
[^33]: Mem0. [Platform CLI](https://docs.mem0.ai/platform/cli), agent mode, output, scope, and explicit storage options.
[^34]: Mem0. [Python package manifest](https://github.com/mem0ai/mem0/blob/main/pyproject.toml) and [project/license](https://github.com/mem0ai/mem0), current core version and Apache-2.0 declaration.
[^35]: Volcengine. [OpenViking](https://github.com/volcengine/OpenViking), context filesystem and architecture.
[^36]: Volcengine. [OpenViking CLI setup](https://github.com/volcengine/OpenViking/blob/main/docs/en/getting-started/05-cli-setup.md), profiles, JSON, and noninteractive setup.
[^37]: Volcengine. [Context types](https://github.com/volcengine/OpenViking/blob/main/docs/en/concepts/02-context-types.md), resource and memory scope.
[^38]: Volcengine. [Viking URI](https://github.com/volcengine/OpenViking/blob/main/docs/en/concepts/04-viking-uri.md), identity-relative addressing.
[^39]: Volcengine. [Session](https://github.com/volcengine/OpenViking/blob/main/docs/en/concepts/08-session.md), archive and extraction lifecycle.
[^40]: Letta. [Legacy server repository](https://github.com/letta-ai/letta) and [current Letta Code runtime](https://github.com/letta-ai/letta-code), development transition.
[^41]: Letta. [MemFS](https://docs.letta.com/concepts/memfs), Git-backed memory, context loading, and synchronization.
[^42]: Letta. [Shared memory](https://docs.letta.com/concepts/shared-memory), cloud requirement, Git repositories, and migration from blocks.
[^43]: Letta. [App Server](https://docs.letta.com/platform/app-server), SDK and protocol interfaces.
[^44]: Cognee. [Project source](https://github.com/topoteretes/cognee), architecture and Apache-2.0 license.
[^45]: Cognee. [CLI overview](https://docs.cognee.ai/cognee-cli/overview), ingestion, processing, and dataset operations.
[^46]: Cognee. [Remember](https://docs.cognee.ai/core-concepts/main-operations/remember) and [recall](https://docs.cognee.ai/core-concepts/main-operations/recall), current memory operations.
[^47]: Cognee. [Permissions](https://docs.cognee.ai/setup-configuration/permissions), users, datasets, provider compatibility, and storage isolation.
[^48]: Cognee. [Forget](https://docs.cognee.ai/core-concepts/main-operations/forget), deletion and related lifecycle.
[^49]: Zep. [Graphiti](https://github.com/getzep/graphiti), temporal graph, backend support, and deployment.
[^50]: Zep. [Graphiti MCP server](https://github.com/getzep/graphiti/blob/main/mcp_server/README.md), transport, grouping, and operations.
[^51]: Zep. [FAQ](https://help.getzep.com/faq), Community Edition deprecation and Cloud/Graphiti/BYOC distinctions.
[^52]: Vectorize. [Hindsight pricing](https://vectorize.io/pricing), rate card accessed September 10, 2026.
[^53]: Mem0. [Pricing](https://mem0.ai/pricing), plan allowances accessed September 10, 2026.
[^54]: Supermemory. [Pricing](https://supermemory.ai/pricing/), plans and metering accessed September 10, 2026.
[^55]: Chris Latimer et al. [Hindsight is 20/20: Building Agent Memory that Retains, Recalls, and Reflects](https://arxiv.org/abs/2512.12818), December 14, 2025.
[^56]: Preston Rasmussen et al. [Zep: A Temporal Knowledge Graph Architecture for Agent Memory](https://arxiv.org/abs/2501.13956), January 20, 2025.
[^57]: Supermemory. [Organizing and filtering memories](https://supermemory.ai/docs/concepts/filtering), container tags and metadata.
[^58]: Volcengine. [OpenViking LICENSE](https://github.com/volcengine/OpenViking/blob/main/LICENSE) and [Rust components LICENSE](https://github.com/volcengine/OpenViking/blob/main/crates/LICENSE), server/CLI license boundary.
[^59]: Volcengine. [VikingMem Python SDK documentation](https://www.volcengine.com/docs/84313/1941747?lang=zh), service-specific SDK and endpoint; Chinese-language documentation.
[^60]: Letta. [AI Memory SDK](https://github.com/letta-ai/ai-memory-sdk), external-agent memory library; latest observed release v0.2.0, November 4, 2025.
[^61]: Zep. [Graphiti LICENSE](https://github.com/getzep/graphiti/blob/main/LICENSE), current Apache-2.0 declaration.
