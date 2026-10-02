#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail

# Kube resources, snapshots, and diffs against a fixture kubectl.
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
test_dir=$(mktemp -d)
mkdir -p "$test_dir/bin"
trap 'rm -rf "$test_dir"' EXIT

pod_json='{"kind":"Pod","metadata":{"name":"api","namespace":"web","labels":{"app":"api"},"creationTimestamp":"2020-01-01T00:00:00Z"},"spec":{"containers":[{"name":"main"},{"name":"side"}]},"status":{"phase":"Running","containerStatuses":[{"ready":true,"restartCount":2},{"ready":false,"restartCount":1}]}}'
deploy_json='{"kind":"Deployment","metadata":{"name":"api","namespace":"web"},"spec":{"replicas":3,"template":{"spec":{"containers":[{"image":"api:1"}]}}},"status":{"readyReplicas":2,"unavailableReplicas":1,"conditions":[{"type":"Available","status":"True"}]}}'
service_json='{"kind":"Service","metadata":{"name":"api","namespace":"web"},"spec":{"type":"ClusterIP","clusterIP":"10.0.0.1","selector":{"app":"api"},"ports":[{"port":80,"targetPort":8080,"protocol":"TCP"}]}}'
node_json='{"kind":"Node","metadata":{"name":"n1"},"spec":{"unschedulable":true},"status":{"conditions":[{"type":"Ready","status":"True"}],"allocatable":{"cpu":"4"}}}'
config_json='{"kind":"ConfigMap","metadata":{"name":"settings","namespace":"web"},"data":{"a":"1"}}'
export POD_JSON="$pod_json"

cat > "$test_dir/bin/kubectl" <<'SH'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$KUBECTL_LOG"
case "$*" in
  'config current-context') echo fixture-ctx ;;
  'config get-contexts -o name') printf 'one\ntwo\n' ;;
  *'get pods -o json'*) printf '{"items":[%s]}\n' "$POD_JSON" ;;
  *'get pod api -o json'*) printf '%s\n' "$POD_JSON" ;;
  *'logs api'*) echo 'log line' ;;
  *) exit 1 ;;
esac
SH
chmod +x "$test_dir/bin/kubectl"
export PATH="$test_dir/bin:$PATH" KUBECTL_LOG="$test_dir/kubectl.log"
source "$root/lib/trash.bash" 2>/dev/null

check() { if [[ "$2" == "$3" ]]; then printf 'PASS: %s\n' "$1"; else printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$1" "$2" "$3"; exit 1; fi; }

check 'Kubectl name' kubectl "$(@ Kube::Kubectl name)"
check 'Resource indexed columns' 'kube_kind kube_name kube_namespace kube_cluster captured_at' \
    "$(@ Kube::Resource indexedColumns | jq -r '[.[].name] | join(" ")')"
check 'Pod inherits indexed columns' '$.capturedAt' "$(@ Kube::Pod indexedColumns | jq -r '.[4].path')"

pod=$(@ Kube::Pod fromJson: "$pod_json" cluster: prod)
check 'Pod kind' Pod "$(@ "$pod" getKind)"
check 'Pod name' api "$(@ "$pod" getName)"
check 'Pod namespace' web "$(@ "$pod" getNamespace)"
check 'Pod cluster' prod "$(@ "$pod" getCluster)"
check 'Pod identity' 'Pod/web/api' "$(@ "$pod" getIdentity)"
check 'Pod capturedAt is UTC' true "$([[ "$(@ "$pod" getCapturedAt)" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}T.*Z$ ]] && echo true || echo false)"
check 'Pod summary' 'Running 1/2 restarts=3' "$(@ "$pod" summary)"
check 'Pod ready' false "$(@ "$pod" ready)"
check 'Pod containers' '["main","side"]' "$(@ "$pod" containers)"
check 'Pod labels' '{"app":"api"}' "$(@ "$pod" labels | jq -c .)"
check 'Pod get:' Running "$(@ "$pod" get: '.status.phase')"
check 'Pod logs use its context' 'log line' "$(@ "$pod" logs)"
check 'Pod logs argv' '-n web --context prod logs api' "$(tail -1 "$KUBECTL_LOG")"

deploy=$(@ Kube::Deployment fromJson: "$deploy_json" cluster: prod)
check 'Deployment identity' 'Deployment/web/api' "$(@ "$deploy" getIdentity)"
check 'Deployment replicas' 3 "$(@ "$deploy" getReplicas)"
check 'Deployment summary' '2/3 available' "$(@ "$deploy" summary)"
check 'Deployment image' 'api:1' "$(@ "$deploy" image)"

service=$(@ Kube::Service fromJson: "$service_json" cluster: prod)
check 'Service type' ClusterIP "$(@ "$service" getServiceType)"
check 'Service selector' '{"app":"api"}' "$(@ "$service" selector | jq -c .)"
check 'Service summary' 'ClusterIP 10.0.0.1 [80/TCP]' "$(@ "$service" summary)"

node=$(@ Kube::Node fromJson: "$node_json" cluster: prod)
check 'Node identity has no namespace' 'Node//n1' "$(@ "$node" getIdentity)"
check 'Node summary' 'Ready,SchedulingDisabled' "$(@ "$node" summary)"
check 'Node allocatable' '{"cpu":"4"}' "$(@ "$node" allocatable | jq -c .)"

config=$(@ Kube::Resource fromJson: "$config_json" cluster: prod)
check 'Generic resource kind' ConfigMap "$(@ "$config" getKind)"
check 'Generic resource identity' 'ConfigMap/web/settings' "$(@ "$config" getIdentity)"

@ "$pod" save >/dev/null
changed_json=$(jq -c '.status.phase = "Failed"' <<< "$pod_json")
pod2=$(@ Kube::Pod fromJson: "$changed_json" cluster: prod)
@ "$pod2" save >/dev/null
diff=$(@ "$pod" diffWith: "$pod2")
check 'diffWith: finds the changed phase' '[{"path":"status.phase","from":"Running","to":"Failed"}]' "$(@ "$diff" getChanged | jq -c .)"
check 'diff hasChanges' true "$(@ "$diff" hasChanges)"
same=$(@ "$pod" diffWith: "$pod")
check 'identical resources have no changes' false "$(@ "$same" hasChanges)"
@ "$config" save >/dev/null
check 'latestFor: finds a snapshot by identity' true "$(@ Kube::Resource latestFor: 'ConfigMap/web/settings' | grep -q 'kube_resource_' && echo true || echo false)"
check 'history lists saved snapshots' true "$(@ Kube::Resource history | grep -q "$config" && echo true || echo false)"

current=$(@ Kube::Cluster current)
check 'Cluster current uses kubectl context' fixture-ctx "$(@ "$current" getContext)"
all=$(@ Kube::Cluster all)
check 'Cluster all has one per context' 2 "$(@ "$all" size)"
fetched=$(@ "$current" fetch: pods)
check 'Cluster fetch: returns typed resources' Pod "$(@ "$(@ "$fetched" at: 0)" getKind)"
one=$(@ "$current" fetch: pod named: api)
check 'Cluster fetch:named: returns a Pod' 'Pod/web/api' "$(@ "$one" getIdentity)"
check 'fetch:named: passes the cluster namespace' '-n default --context fixture-ctx get pod api -o json' "$(tail -1 "$KUBECTL_LOG")"

snap=$(@ Kube::Snapshot take: daily of: pods onCluster: fixture-ctx)
check 'Snapshot size' 1 "$(@ "$snap" size)"
check 'Snapshot resourceIds' true "$([[ "$(@ "$snap" resourceIds)" == kube_pod_* ]] && echo true || echo false)"
check 'Snapshot resourcesOfKind:' "$(@ "$snap" resourceIds)" "$(@ "$snap" resourcesOfKind: Pod)"
check 'Snapshot resourcesOfKind: filters' '' "$(@ "$snap" resourcesOfKind: Node)"
snap2=$(@ Kube::Snapshot take: daily of: pods onCluster: fixture-ctx)
check 'Snapshot diff without changes' false "$(@ "$(@ Kube::Diff compare: "$snap" with: "$snap2")" hasChanges)"
export POD_JSON="$changed_json"
snap3=$(@ Kube::Snapshot take: daily of: pods onCluster: fixture-ctx)
snapdiff=$(@ Kube::Diff compare: "$snap" with: "$snap3")
check 'Snapshot diff reports the changed pod' 'Pod/web/api status.phase' "$(@ "$snapdiff" getChanged | jq -r '.[] | .identity + " " + .changes[0].path')"
check 'Snapshot diff report' true "$(@ "$snapdiff" report | grep -q 'status.phase: Running' && echo true || echo false)"

# latest:onCluster: and history:onCluster: read saved snapshots by label and
# cluster, newest first; the arguments are data, never SQL.
@ "$snap" setTakenAt: 2020-01-01T00:00:01Z; @ "$snap" save
@ "$snap3" setTakenAt: 2020-01-01T00:00:03Z; @ "$snap3" save
@ "$snap2" setTakenAt: 2020-01-01T00:00:02Z; @ "$snap2" setCluster: other-ctx; @ "$snap2" save
check 'Snapshot latest:onCluster:' "$snap3" "$(@ Kube::Snapshot latest: daily onCluster: fixture-ctx)"
check 'Snapshot latest:onCluster: filters by cluster' "$snap2" "$(@ Kube::Snapshot latest: daily onCluster: other-ctx)"
check 'Snapshot history:onCluster: newest first' "$snap3 $snap" "$(@ Kube::Snapshot history: daily onCluster: fixture-ctx | tr '\n' ' ' | sed 's/ $//')"
check 'Snapshot history:onCluster: unknown label' '' "$(@ Kube::Snapshot history: weekly onCluster: fixture-ctx)"
check 'Snapshot latest:onCluster: quotes its arguments' '' "$(@ Kube::Snapshot latest: "x' OR '1'='1" onCluster: fixture-ctx)"
