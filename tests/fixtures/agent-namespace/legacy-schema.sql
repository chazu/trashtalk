-- Membership is maintained in the same SQLite statement as session persistence.
-- Policy revision 0 is legacy, explicitly migrated by the identity owner.
CREATE TABLE IF NOT EXISTS agent_session_memberships (
  identity_id TEXT NOT NULL,
  scope_key TEXT NOT NULL,
  session_id TEXT NOT NULL UNIQUE,
  policy_revision INTEGER NOT NULL,
  PRIMARY KEY(identity_id, scope_key)
);
CREATE INDEX IF NOT EXISTS agent_sessions_identity_lifecycle ON instances(class,json_extract(data,'$.identity'),json_extract(data,'$.lifecycleState'));
CREATE TRIGGER IF NOT EXISTS agent_session_insert_policy BEFORE INSERT ON instances
WHEN NEW.class='AgentSession' AND json_extract(NEW.data,'$.lifecycleState') IN ('open','paused')
AND EXISTS(SELECT 1 FROM instances i WHERE i.id=json_extract(NEW.data,'$.identity')
  AND i.class='AgentIdentity' AND coalesce(json_extract(i.data,'$.sessionPolicyRevision'),0)>0)
BEGIN
  SELECT CASE WHEN NOT EXISTS(SELECT 1 FROM instances i WHERE i.id=json_extract(NEW.data,'$.identity')
    AND json_extract(NEW.data,'$.sessionPolicyRevision')=json_extract(i.data,'$.sessionPolicyRevision'))
    THEN RAISE(ABORT,'Session policy changed; upgrade or reload before writing') END;
END;
CREATE TRIGGER IF NOT EXISTS agent_session_update_policy BEFORE UPDATE ON instances
WHEN NEW.class='AgentSession' AND json_extract(NEW.data,'$.lifecycleState') IN ('open','paused')
AND EXISTS(SELECT 1 FROM instances i WHERE i.id=json_extract(NEW.data,'$.identity')
  AND i.class='AgentIdentity' AND coalesce(json_extract(i.data,'$.sessionPolicyRevision'),0)>0)
BEGIN
  SELECT CASE WHEN NOT EXISTS(SELECT 1 FROM instances i WHERE i.id=json_extract(NEW.data,'$.identity')
    AND json_extract(NEW.data,'$.sessionPolicyRevision')=json_extract(i.data,'$.sessionPolicyRevision'))
    THEN RAISE(ABORT,'Session policy changed; upgrade or reload before writing') END;
END;
CREATE TRIGGER IF NOT EXISTS agent_session_insert_membership AFTER INSERT ON instances
WHEN NEW.class='AgentSession'
BEGIN
  DELETE FROM agent_session_memberships WHERE session_id=NEW.id;
  INSERT INTO agent_session_memberships(identity_id,scope_key,session_id,policy_revision)
    SELECT i.id,CASE json_extract(i.data,'$.sessionScope') WHEN 'identity' THEN '' ELSE json_extract(NEW.data,'$.workspace') END,
      NEW.id,json_extract(i.data,'$.sessionPolicyRevision')
    FROM instances i WHERE i.id=json_extract(NEW.data,'$.identity') AND i.class='AgentIdentity'
      AND coalesce(json_extract(i.data,'$.sessionPolicyRevision'),0)>0
      AND json_extract(NEW.data,'$.lifecycleState') IN ('open','paused');
END;
CREATE TRIGGER IF NOT EXISTS agent_session_update_membership AFTER UPDATE ON instances
WHEN OLD.class='AgentSession' OR NEW.class='AgentSession'
BEGIN
  DELETE FROM agent_session_memberships WHERE session_id=OLD.id;
  INSERT INTO agent_session_memberships(identity_id,scope_key,session_id,policy_revision)
    SELECT i.id,CASE json_extract(i.data,'$.sessionScope') WHEN 'identity' THEN '' ELSE json_extract(NEW.data,'$.workspace') END,
      NEW.id,json_extract(i.data,'$.sessionPolicyRevision')
    FROM instances i WHERE i.id=json_extract(NEW.data,'$.identity') AND i.class='AgentIdentity'
      AND NEW.class='AgentSession' AND coalesce(json_extract(i.data,'$.sessionPolicyRevision'),0)>0
      AND json_extract(NEW.data,'$.lifecycleState') IN ('open','paused');
END;
CREATE TRIGGER IF NOT EXISTS agent_session_delete_membership AFTER DELETE ON instances
WHEN OLD.class='AgentSession'
BEGIN
  DELETE FROM agent_session_memberships WHERE session_id=OLD.id;
END;
-- Old launchers must not bypass current membership after a scope cutover.
CREATE TRIGGER IF NOT EXISTS agent_run_insert_membership BEFORE INSERT ON instances
WHEN NEW.class='AgentRun' AND json_extract(NEW.data,'$.state') IN ('starting','running','recovering')
AND EXISTS(SELECT 1 FROM instances s JOIN instances i ON i.id=json_extract(s.data,'$.identity')
  WHERE s.id=json_extract(NEW.data,'$.session') AND coalesce(json_extract(i.data,'$.sessionPolicyRevision'),0)>0)
BEGIN
  SELECT CASE WHEN NOT EXISTS(SELECT 1 FROM instances s JOIN instances i ON i.id=json_extract(s.data,'$.identity')
    JOIN agent_session_memberships m ON m.session_id=s.id AND m.identity_id=i.id
    WHERE s.id=json_extract(NEW.data,'$.session') AND json_extract(s.data,'$.lifecycleState')='open'
      AND m.policy_revision=json_extract(i.data,'$.sessionPolicyRevision'))
    THEN RAISE(ABORT,'Run requires current open session membership') END;
  SELECT CASE WHEN EXISTS(SELECT 1 FROM instances r WHERE r.class='AgentRun' AND r.id!=NEW.id
    AND json_extract(r.data,'$.session')=json_extract(NEW.data,'$.session')
    AND json_extract(r.data,'$.state') IN ('starting','running','recovering'))
    THEN RAISE(ABORT,'Session already has an active run') END;
END;
CREATE TRIGGER IF NOT EXISTS agent_run_update_membership BEFORE UPDATE ON instances
WHEN NEW.class='AgentRun' AND json_extract(NEW.data,'$.state') IN ('starting','running','recovering')
AND (coalesce(json_extract(OLD.data,'$.session'),'')!=json_extract(NEW.data,'$.session')
  OR coalesce(json_extract(OLD.data,'$.state'),'') NOT IN ('starting','running','recovering'))
AND EXISTS(SELECT 1 FROM instances s JOIN instances i ON i.id=json_extract(s.data,'$.identity')
  WHERE s.id=json_extract(NEW.data,'$.session') AND coalesce(json_extract(i.data,'$.sessionPolicyRevision'),0)>0)
BEGIN
  SELECT CASE WHEN NOT EXISTS(SELECT 1 FROM instances s JOIN instances i ON i.id=json_extract(s.data,'$.identity')
    JOIN agent_session_memberships m ON m.session_id=s.id AND m.identity_id=i.id
    WHERE s.id=json_extract(NEW.data,'$.session') AND json_extract(s.data,'$.lifecycleState')='open'
      AND m.policy_revision=json_extract(i.data,'$.sessionPolicyRevision'))
    THEN RAISE(ABORT,'Run requires current open session membership') END;
  SELECT CASE WHEN EXISTS(SELECT 1 FROM instances r WHERE r.class='AgentRun' AND r.id!=NEW.id
    AND json_extract(r.data,'$.session')=json_extract(NEW.data,'$.session')
    AND json_extract(r.data,'$.state') IN ('starting','running','recovering'))
    THEN RAISE(ABORT,'Session already has an active run') END;
END;
CREATE TRIGGER IF NOT EXISTS agent_delivery_claim_membership BEFORE UPDATE ON instances
WHEN NEW.class='AgentDelivery' AND json_extract(NEW.data,'$.state')='offered'
AND coalesce(json_extract(OLD.data,'$.state'),'')!='offered'
AND EXISTS(SELECT 1 FROM instances s JOIN instances i ON i.id=json_extract(s.data,'$.identity')
  WHERE s.id=json_extract(NEW.data,'$.session') AND coalesce(json_extract(i.data,'$.sessionPolicyRevision'),0)>0)
BEGIN
  SELECT CASE WHEN NOT EXISTS(SELECT 1 FROM instances s JOIN instances i ON i.id=json_extract(s.data,'$.identity')
    JOIN agent_session_memberships m ON m.session_id=s.id AND m.identity_id=i.id
    JOIN instances r ON r.id=json_extract(NEW.data,'$.run') AND r.class='AgentRun'
    WHERE s.id=json_extract(NEW.data,'$.session') AND json_extract(s.data,'$.lifecycleState')='open'
      AND json_extract(r.data,'$.session')=s.id AND json_extract(r.data,'$.state') IN ('starting','running','recovering')
      AND m.policy_revision=json_extract(i.data,'$.sessionPolicyRevision'))
    THEN RAISE(ABORT,'Delivery claim requires the current session and its active run') END;
END;
CREATE TRIGGER IF NOT EXISTS agent_delivery_insert_membership BEFORE INSERT ON instances
WHEN NEW.class='AgentDelivery' AND json_extract(NEW.data,'$.state') IN ('pending','offered')
AND EXISTS(SELECT 1 FROM instances s JOIN instances i ON i.id=json_extract(s.data,'$.identity')
  WHERE s.id=json_extract(NEW.data,'$.session') AND coalesce(json_extract(i.data,'$.sessionPolicyRevision'),0)>0)
BEGIN
  SELECT CASE WHEN NOT EXISTS(SELECT 1 FROM instances s JOIN instances i ON i.id=json_extract(s.data,'$.identity')
    JOIN agent_session_memberships m ON m.session_id=s.id AND m.identity_id=i.id
    WHERE s.id=json_extract(NEW.data,'$.session') AND json_extract(s.data,'$.lifecycleState') IN ('open','paused')
      AND m.policy_revision=json_extract(i.data,'$.sessionPolicyRevision'))
    THEN RAISE(ABORT,'Delivery requires current session membership') END;
END;
CREATE TRIGGER IF NOT EXISTS agent_delivery_requeue_membership BEFORE UPDATE ON instances
WHEN NEW.class='AgentDelivery' AND json_extract(NEW.data,'$.state')='pending'
AND coalesce(json_extract(OLD.data,'$.state'),'')!='pending'
AND EXISTS(SELECT 1 FROM instances s JOIN instances i ON i.id=json_extract(s.data,'$.identity')
  WHERE s.id=json_extract(NEW.data,'$.session') AND coalesce(json_extract(i.data,'$.sessionPolicyRevision'),0)>0)
BEGIN
  SELECT CASE WHEN NOT EXISTS(SELECT 1 FROM instances s JOIN instances i ON i.id=json_extract(s.data,'$.identity')
    JOIN agent_session_memberships m ON m.session_id=s.id AND m.identity_id=i.id
    WHERE s.id=json_extract(NEW.data,'$.session') AND json_extract(s.data,'$.lifecycleState') IN ('open','paused')
      AND m.policy_revision=json_extract(i.data,'$.sessionPolicyRevision'))
    THEN RAISE(ABORT,'Historical delivery cannot be requeued into a superseded session') END;
END;
CREATE TRIGGER IF NOT EXISTS agent_message_insert_membership BEFORE INSERT ON instances
WHEN NEW.class='Message' AND json_extract(NEW.data,'$.to') LIKE 'session:%'
AND EXISTS(SELECT 1 FROM instances s JOIN instances i ON i.id=json_extract(s.data,'$.identity')
  WHERE s.id=substr(json_extract(NEW.data,'$.to'),9) AND coalesce(json_extract(i.data,'$.sessionPolicyRevision'),0)>0)
BEGIN
  SELECT CASE WHEN NOT EXISTS(SELECT 1 FROM instances s JOIN instances i ON i.id=json_extract(s.data,'$.identity')
    JOIN agent_session_memberships m ON m.session_id=s.id AND m.identity_id=i.id
    WHERE s.id=substr(json_extract(NEW.data,'$.to'),9) AND json_extract(s.data,'$.lifecycleState') IN ('open','paused')
      AND m.policy_revision=json_extract(i.data,'$.sessionPolicyRevision'))
    THEN RAISE(ABORT,'This conversation is historical; send to the current session') END;
END;
CREATE TRIGGER IF NOT EXISTS agent_message_update_membership BEFORE UPDATE ON instances
WHEN NEW.class='Message' AND json_extract(NEW.data,'$.to') LIKE 'session:%'
AND (json_extract(OLD.data,'$.to') IS NOT json_extract(NEW.data,'$.to')
  OR json_extract(OLD.data,'$.body') IS NOT json_extract(NEW.data,'$.body'))
AND EXISTS(SELECT 1 FROM instances s JOIN instances i ON i.id=json_extract(s.data,'$.identity')
  WHERE s.id=substr(json_extract(NEW.data,'$.to'),9) AND coalesce(json_extract(i.data,'$.sessionPolicyRevision'),0)>0)
BEGIN
  SELECT CASE WHEN NOT EXISTS(SELECT 1 FROM instances s JOIN instances i ON i.id=json_extract(s.data,'$.identity')
    JOIN agent_session_memberships m ON m.session_id=s.id AND m.identity_id=i.id
    WHERE s.id=substr(json_extract(NEW.data,'$.to'),9) AND json_extract(s.data,'$.lifecycleState') IN ('open','paused')
      AND m.policy_revision=json_extract(i.data,'$.sessionPolicyRevision'))
    THEN RAISE(ABORT,'This conversation is historical; send to the current session') END;
END;
-- Closing does not stop an already admitted run. Require explicit stop or
-- termination, preserving the active session until execution is accounted for.
CREATE TRIGGER IF NOT EXISTS agent_session_close_active BEFORE UPDATE ON instances
WHEN NEW.class='AgentSession' AND json_extract(NEW.data,'$.lifecycleState')='closed'
AND json_extract(OLD.data,'$.lifecycleState')!='closed'
AND EXISTS(SELECT 1 FROM agent_session_memberships m WHERE m.session_id=OLD.id)
AND EXISTS(SELECT 1 FROM instances r WHERE r.class='AgentRun' AND json_extract(r.data,'$.session')=OLD.id
  AND json_extract(r.data,'$.state') IN ('starting','running','recovering'))
BEGIN
  SELECT RAISE(ABORT,'Session has active work; stop it before closing');
END;
-- Only the explicit migration transaction can change an enrolled scope policy.
-- The marker never survives COMMIT and is not an instance-field escape hatch.
CREATE TABLE IF NOT EXISTS agent_session_policy_changes (
  identity_id TEXT PRIMARY KEY, old_revision INTEGER NOT NULL, new_revision INTEGER NOT NULL
);
CREATE TRIGGER IF NOT EXISTS agent_identity_update_policy BEFORE UPDATE ON instances
WHEN OLD.class='AgentIdentity' AND coalesce(json_extract(OLD.data,'$.sessionPolicyRevision'),0)>0
AND EXISTS(SELECT 1 FROM instances s WHERE s.class='AgentSession' AND json_extract(s.data,'$.identity')=OLD.id)
AND (NEW.class IS NOT OLD.class OR json_extract(NEW.data,'$.sessionScope') IS NOT json_extract(OLD.data,'$.sessionScope')
  OR json_extract(NEW.data,'$.sessionPolicyRevision') IS NOT json_extract(OLD.data,'$.sessionPolicyRevision'))
AND NOT EXISTS(SELECT 1 FROM agent_session_policy_changes c WHERE c.identity_id=OLD.id
  AND c.old_revision=json_extract(OLD.data,'$.sessionPolicyRevision') AND c.new_revision=json_extract(NEW.data,'$.sessionPolicyRevision'))
BEGIN
  SELECT RAISE(ABORT,'Session policy changes require explicit owner migration');
END;
CREATE TRIGGER IF NOT EXISTS agent_identity_replace_policy BEFORE INSERT ON instances
WHEN EXISTS(SELECT 1 FROM instances old WHERE old.id=NEW.id AND old.class='AgentIdentity'
  AND coalesce(json_extract(old.data,'$.sessionPolicyRevision'),0)>0
  AND EXISTS(SELECT 1 FROM instances s WHERE s.class='AgentSession' AND json_extract(s.data,'$.identity')=old.id)
  AND (NEW.class IS NOT old.class OR json_extract(NEW.data,'$.sessionScope') IS NOT json_extract(old.data,'$.sessionScope')
    OR json_extract(NEW.data,'$.sessionPolicyRevision') IS NOT json_extract(old.data,'$.sessionPolicyRevision')))
BEGIN
  SELECT RAISE(ABORT,'Stale identity replacement cannot change session policy');
END;
CREATE TRIGGER IF NOT EXISTS agent_session_update_provenance BEFORE UPDATE ON instances
WHEN OLD.class='AgentSession' AND coalesce(json_extract(OLD.data,'$.sessionPolicyRevision'),0)>0
AND (NEW.class IS NOT OLD.class OR json_extract(NEW.data,'$.identity') IS NOT json_extract(OLD.data,'$.identity')
  OR json_extract(NEW.data,'$.workspace') IS NOT json_extract(OLD.data,'$.workspace'))
BEGIN
  SELECT RAISE(ABORT,'An enrolled session retains its identity and creation workspace');
END;
CREATE TRIGGER IF NOT EXISTS agent_session_replace_provenance BEFORE INSERT ON instances
WHEN EXISTS(SELECT 1 FROM instances old WHERE old.id=NEW.id AND old.class='AgentSession'
  AND coalesce(json_extract(old.data,'$.sessionPolicyRevision'),0)>0
  AND (NEW.class IS NOT old.class OR json_extract(NEW.data,'$.identity') IS NOT json_extract(old.data,'$.identity')
    OR json_extract(NEW.data,'$.workspace') IS NOT json_extract(old.data,'$.workspace')))
BEGIN
  SELECT RAISE(ABORT,'An enrolled session retains its identity and creation workspace');
END;
-- A delivery's execution directory is separately authorized from membership.
-- Policy snapshots are compared in the same statement that publishes/claims it.
DROP TRIGGER IF EXISTS agent_delivery_context_insert;
DROP TRIGGER IF EXISTS agent_delivery_context_claim;
CREATE TRIGGER IF NOT EXISTS agent_delivery_context_insert_v1
BEFORE INSERT ON instances
WHEN json_extract(NEW.data,'$.class')='AgentDelivery'
  AND coalesce(json_extract(NEW.data,'$.executionWorkspace'),'')!=''
BEGIN
  SELECT CASE WHEN NOT EXISTS (
    SELECT 1 FROM instances s JOIN instances r ON r.id=json_extract(s.data,'$.role')
    JOIN instances i ON i.id=json_extract(s.data,'$.identity')
    WHERE s.id=json_extract(NEW.data,'$.session') AND s.class='AgentSession' AND r.class='AgentRole'
      AND json_extract(i.data,'$.enabled') IN (1,'true')
      AND r.id=json_extract(NEW.data,'$.authorizationRole')
      AND CAST(json_extract(r.data,'$.revision') AS TEXT)=CAST(json_extract(NEW.data,'$.authorizationRevision') AS TEXT)
      AND json_extract(r.data,'$.workspacePolicy')=json_extract(NEW.data,'$.authorizationWorkspacePolicy')
  ) THEN RAISE(ABORT,'Delivery workspace authorization changed; reload before admission') END;
END;

CREATE TRIGGER IF NOT EXISTS agent_delivery_context_claim_v1
BEFORE UPDATE OF data ON instances
WHEN NEW.class='AgentDelivery' AND json_extract(NEW.data,'$.state')='offered'
  AND coalesce(json_extract(NEW.data,'$.executionWorkspace'),'')!=''
BEGIN
  SELECT CASE WHEN NOT EXISTS (
    SELECT 1 FROM instances s JOIN instances role ON role.id=json_extract(s.data,'$.role')
    JOIN instances i ON i.id=json_extract(s.data,'$.identity')
    JOIN instances run ON run.id=json_extract(NEW.data,'$.run')
    WHERE s.id=json_extract(NEW.data,'$.session') AND json_extract(run.data,'$.session')=s.id
      AND role.class='AgentRole' AND run.class='AgentRun'
      AND json_extract(i.data,'$.enabled') IN (1,'true')
      AND role.id=json_extract(NEW.data,'$.authorizationRole')
      AND CAST(json_extract(role.data,'$.revision') AS TEXT)=CAST(json_extract(NEW.data,'$.authorizationRevision') AS TEXT)
      AND json_extract(role.data,'$.workspacePolicy')=json_extract(NEW.data,'$.authorizationWorkspacePolicy')
      AND json_extract(run.data,'$.executionWorkspace')=json_extract(NEW.data,'$.executionWorkspace')
  ) THEN RAISE(ABORT,'Delivery execution workspace or authorization no longer matches its run') END;
END;

CREATE TRIGGER IF NOT EXISTS agent_run_workspace_immutable
BEFORE UPDATE OF data ON instances
WHEN OLD.class='AgentRun' AND coalesce(json_extract(OLD.data,'$.executionWorkspace'),'')!=''
  AND json_extract(NEW.data,'$.executionWorkspace') IS NOT json_extract(OLD.data,'$.executionWorkspace')
BEGIN
  SELECT RAISE(ABORT,'Run execution workspace is immutable');
END;

CREATE TRIGGER IF NOT EXISTS agent_delivery_workspace_immutable
BEFORE UPDATE OF data ON instances
WHEN OLD.class='AgentDelivery' AND coalesce(json_extract(OLD.data,'$.executionWorkspace'),'')!=''
  AND json_extract(NEW.data,'$.executionWorkspace') IS NOT json_extract(OLD.data,'$.executionWorkspace')
BEGIN
  SELECT RAISE(ABORT,'Delivery execution workspace is immutable');
END;
