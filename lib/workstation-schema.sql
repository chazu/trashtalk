BEGIN IMMEDIATE;
-- The first Workstation package release renamed its two persisted models from
-- top-level classes.  Keep their ids and documents stable while changing the
-- Store discriminator atomically, before rebuilding the dependent indexes.
DROP INDEX IF EXISTS workstation_subscription_consumer;
DROP INDEX IF EXISTS workstation_subscription_owner;
DROP INDEX IF EXISTS workstation_attention_subscription;
UPDATE instances
   SET data=json_set(data,'$.class','Workstation::EventSubscription')
 WHERE class='EventSubscription';
UPDATE instances
   SET data=json_set(data,'$.class','Workstation::Attention')
 WHERE class='Attention';
-- Optional feature installation. No event log or consumer cursor is created.
CREATE UNIQUE INDEX IF NOT EXISTS workstation_subscription_consumer
 ON instances(json_extract(data,'$.consumerName'))
 WHERE class='Workstation::EventSubscription' AND json_extract(data,'$.schema_version')=1;
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
