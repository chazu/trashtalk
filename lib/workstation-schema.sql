BEGIN IMMEDIATE;
-- Optional feature installation. No event log or consumer cursor is created.
CREATE UNIQUE INDEX IF NOT EXISTS workstation_subscription_consumer
 ON instances(json_extract(data,'$.consumerName'))
 WHERE class='EventSubscription' AND json_extract(data,'$.schema_version')=1;
CREATE INDEX IF NOT EXISTS workstation_subscription_owner
 ON instances(class,json_extract(data,'$.owner'));
CREATE INDEX IF NOT EXISTS workstation_attention_subscription
 ON instances(class,json_extract(data,'$.subscription'),json_extract(data,'$.state'));
CREATE TABLE IF NOT EXISTS workstation_coordinates (
 subscription_id TEXT NOT NULL, stream_name TEXT NOT NULL,
 partition TEXT NOT NULL, offset INTEGER NOT NULL CHECK(offset >= 0 AND offset <= 9007199254740991),
 attention_id TEXT NOT NULL,
 PRIMARY KEY(subscription_id,stream_name,partition,offset)
);
CREATE INDEX IF NOT EXISTS workstation_coordinates_attention ON workstation_coordinates(attention_id);
COMMIT;
