-- Optional feature installation. No event log or consumer cursor is created.
CREATE UNIQUE INDEX IF NOT EXISTS workstation_subscription_consumer
 ON instances(json_extract(data,'$.consumerName'))
 WHERE class='EventSubscription' AND json_extract(data,'$.schema_version')=1;
CREATE INDEX IF NOT EXISTS workstation_subscription_owner
 ON instances(class,json_extract(data,'$.owner'));
CREATE INDEX IF NOT EXISTS workstation_attention_subscription
 ON instances(class,json_extract(data,'$.subscription'),json_extract(data,'$.state'));
