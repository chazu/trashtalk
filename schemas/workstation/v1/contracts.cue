package workstation
import "strings"
#Text: string & strings.MinRunes(1) & strings.MaxRunes(256) & !~"[\\x00-\\x1f\\x7f]"
#Note: string & strings.MaxRunes(256) & !~"[\\x00-\\x1f\\x7f]"
#Timestamp: string & =~"^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$"
#SubscriptionID: string & =~"^eventsubscription_[A-Za-z0-9-]{1,64}$"
#AttentionID: string & =~"^attention_[A-Za-z0-9-]{1,64}$"
#IdentityID: string & =~"^agentidentity_[A-Za-z0-9-]{1,64}$"
#SessionID: string & =~"^agentsession_[A-Za-z0-9-]{1,64}$"
#RunID: string & =~"^agentrun_[A-Za-z0-9-]{1,64}$"
#Digest: string & =~"^[a-f0-9]{64}$"
// Receipt provenance: presentation and loop-prevention metadata, never routing authority.
#Origin: {
 producer: #Text
 run?: #RunID
}
// Delegation policy: manual delegation needs a target; automatic needs explicit confirmation.
#Delegation: {
 mode?: "manual" | "automatic"
 maxLineageDepth?: int & >=0 & <=8
 confirmation?: #Note
}
#Coordinate: {
 subscription: #SubscriptionID
 streamName: #Text
 partition: string & =~"^[A-Za-z0-9_.-]{1,64}$"
 offset: int & >=0 & <=9007199254740991
}
#EventSubscription: {
 schema_version: 1
 id: #SubscriptionID
 owner: string & =~"^[A-Za-z0-9][A-Za-z0-9_.:-]{0,63}$"
 enabled: bool
 dispatchState: "enabled" | "paused"
 streamName: #Text
 consumerName: #Text
 adapterKind: #Text
 filter: {exitNot?: int & >=0 & <=255}
 debounceSeconds: int & >=0 & <=86400
 targetIdentity: "" | #IdentityID
 grouping: #Text
 revision: int & >=1
 schemaDigest: #Digest
 createdAt: #Timestamp
 updatedAt: #Timestamp
 initialPosition: "from-now" | "from-start"
 auditNote: #Text
 delegation?: #Delegation
}
#Attention: {
 schema_version: 1
 id: #AttentionID
 subscription: #SubscriptionID
 groupKey: #Text
 state: "open" | "acknowledged" | "snoozed" | "resolved" | "suppressed"
 message: "" | (string & =~"^message_[A-Za-z0-9-]{1,64}$")
 firstCoordinate: null | #Coordinate
 lastCoordinate: null | #Coordinate
 eventCount: int & >=0
 snoozeUntil: "" | #Timestamp
 note: #Note
 actor: #Text
 createdAt: #Timestamp
 updatedAt: #Timestamp
 schemaDigest: #Digest
 lastResolvedSession: "" | #SessionID
 // Phase 2 routing metadata. Absent fields mean an unrouted local group.
 workspace?: "" | #Text
 origin?: null | #Origin
 lineageDepth?: int & >=0 & <=64
 delegationRevision?: int & >=0
 delegatedMessage?: "" | (string & =~"^message_[A-Za-z0-9_-]{1,128}$")
 delegatedSession?: "" | #SessionID
 delegatedIdentity?: "" | #IdentityID
 routingNote?: #Note
}
#CommandReceipt: {
 schema_version: 1
 kind: "command-receipt.v1"
 receiptId?: string & =~"^commandreceipt_[A-Za-z0-9-]{1,64}$"
 commandLabel?: #Text
 startedAt?: #Timestamp
 origin?: #Origin
 workspace: #Text
 fingerprint: string & =~"^[a-f0-9]{64}$"
 exitCode: int & >=0 & <=255
 finishedAt: #Timestamp
 display: #SafeDisplay
}
#SafeDisplay: {
 title: #Text
 summary: #Note
}
#Envelope: {
 schema_version: 1
 coordinate: #Coordinate
 groupKey: #Text
 display: #SafeDisplay
 workspace?: #Text
 origin?: #Origin
}
