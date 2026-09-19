# Agent conversation improvements

**Status:** deferred by the user on 2026-09-16; Gusgus conversation creation
was implemented on 2026-09-18. The other two items remain future improvements,
not active implementation work. Sizes include implementation and regression
tests. Reassess them when work resumes.

| Improvement | Size | Main uncertainty |
| --- | --- | --- |
| Intermittent automatic dismissal | M, provisional | Root cause has not been isolated; could shrink to S after reproduction. |
| Start a conversation from `inagent` | Implemented | Gusgus-only empty composer; first direct send creates the global conversation. |
| Inbox notifications for detached conversations | L | Durable unread tracking, focus races, coalescing and restart recovery. |

Suggested order for the remaining work: dismissal, then detached notifications.
The work spans the Innards UI and Trashtalk's session bridge; notification
authority belongs in Trashtalk.

## Intermittent automatic dismissal

The user reports that the conversation view sometimes dismisses itself. First
reproduce the exact interaction and distinguish explicit dismissal from terminal
events, bridge EOF/failure and process lifecycle changes. Do not assume that
the scroll-follow fix resolves this separate symptom.

Acceptance:

- Normal conversation updates, scrolling and run completion keep the view open.
- Intentional detach still works and never stops the agent.
- Bridge or terminal failures preserve a recoverable draft and an actionable
  diagnostic instead of silently discarding the conversation view's state.
- Add a regression at the reproduced failure boundary, with PTY coverage where
  the failure depends on terminal behavior.

## Start a conversation from inagent (implemented)

Option-U and `@ Gusgus focusCurrent` attach to the current global Gusgus
conversation when one exists. With no current conversation, they open `inagent`
with an empty composer showing the canonical starting workspace and configured
profile. No session is created until the user sends the first message with
**C-c C-c**.

This first implementation intentionally supports only Gusgus, whose session
scope is always global. Trashtalk atomically reuses a conversation that appears
during the interaction or creates the single current conversation, then sends
the text through direct session input. It never converts the draft into inbox
mail. The view becomes pinned to the resulting session; choosing specialist
identities or workspace-scoped conversations remains outside this slice.

Acceptance:

- A user with no current conversation can create one and send the first message
  from the UI without going through `@@`.
- Reuse the selected current conversation when policy requires it; do not create
  duplicate identity-scoped sessions or silently replace provider history.
- Cancellation and creation failures retain user input and show a useful result.
- The composer retains direct-input semantics; failure does not silently turn
  the user's text into inbox mail.

Implemented behavior retains the draft on creation or send failure. If creation
succeeds but native input fails, the view attaches to that empty conversation so
the user can inspect the error and retry. Detaching before the first send creates
nothing.

## Inbox notifications for detached conversations

General agent conversation output should be discoverable when the human is not
viewing that conversation. This is separate from the implemented per-Assignment
status Message described in [durable delegation](agent-delegation-flow.md).

Acceptance:

- Output viewed in the focused conversation does not create redundant inbox
  notifications; merely having a window open is not proof that output was seen.
- Unseen output in a detached conversation updates one actionable notification
  per conversation rather than accumulating one item per message or text chunk.
- Opening the notification returns to the same logical/provider conversation.
- Durable unread positions and presentation acknowledgements survive restart,
  reconnect and concurrent focus changes without losing or duplicating notices.
- Reading conversation output or a notification never settles agent deliveries
  or completes an Assignment.

Agree on what counts as viewed before implementation, including scrolling away
from the bottom and multiple attached views. Exercise those cases together with
detach-during-output and worker/UI restart recovery.

## Related implemented work

The [live view guide](agent-session-view.md) documents returning to the bottom to
resume following and acknowledging changed content under a stable message ID.
Those changes were implemented, tested and installed locally in Innards before
this deferral; they are not part of the outstanding work above.
