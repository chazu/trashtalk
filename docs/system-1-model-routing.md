# Jev via OpenRouter proof of concept

## Status

Implemented as a narrow proof of concept. It sends one non-streaming text
completion to `~typesafe/jev-latest` through OpenRouter.

## Goal

Verify that Trashtalk can call Jev through OpenRouter with little added
runtime overhead. This is not a routing layer.

## Scope

The proof of concept contains one class, `OpenRouter`, with one public method:

```trash
@ OpenRouter complete: 'Reply with one word: ready'
```

It calls `https://openrouter.ai/api/v1/chat/completions` directly. The request
uses the fixed model `~typesafe/jev-latest` and a single user message.

`OPENROUTER_API_KEY` must be present in the caller environment. The adapter
writes the authorization header only to a mode-600 temporary curl config file.
It never puts the key in Trashtalk source, shell command text, curl arguments,
or returned results.

The success result is JSON with `outcome: "success"`, the HTTP `status`, the
fixed `model`, and returned `content`. HTTP, transport, malformed-response,
and request-encoding failures return distinct outcomes.

## Manual smoke test

From the repository root, run:

```bash
source lib/trash.bash
@ OpenRouter complete: 'Reply with exactly: ready'
```

This call incurs the provider's normal inference cost.

## Deliberately deferred

This proof of concept does not include:

- LiteLLM, HAProxy, a persistent HTTP helper, or local GPU backends.
- Health checks, failover, retries, load balancing, or model routing.
- Streaming, tool calls, structured output, or multi-turn conversations.
- A `ServiceTransport` trait, capability matrix, model aliases, or a generic service class.

## Next decision

After the direct call is useful in real work, decide whether a second backend
or a second protocol is needed. Only then evaluate LiteLLM and a shared service
transport boundary.
