#!/usr/bin/env bash
# Builds one project with bonsai in a fresh container and writes its outputs to <out>/<name>.
# usage: run.sh --url <git url or local repository> --ref <ref> --java <major> --ivy <ivy home> [options] [--sbt <command>]...
#   --out DIR        output root (default community-out)
#   --name NAME      output directory name (default derived from the url)
#   --image IMAGE    image to run (default localhost/bonsai-community:latest)
#   --memory SIZE    container memory cap, swap included (default 12g)
#   --heap SIZE      sbt heap (default 6g)
#   --cpus N         cpu cap
#   --timeout SEC    wall-clock limit for the sbt run (default 3600)
#   --cache VOLUME   named volume holding the dependency caches, kept afterwards (default: a fresh volume, removed after)
#   --version V      bonsai version in the ivy repository (default 0.1.0-SNAPSHOT)
#   --bonsai no      build the project without bonsai, as the control of a run
#   --tag TAG        a git tag to fetch beside the ref, or ALL to unshallow with every tag, for builds that read tags
# --ivy is the ivy home the bonsai artefacts were published to with -Dsbt.ivy.home; its local/ is mounted read-only.
# A build that fails, times out or crashes is a result in <out>/<name>/status, not an error of this script.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
engine=${CONTAINER_ENGINE:-podman}
image=localhost/bonsai-community:latest
out=community-out
memory=12g
heap=6g
timeout_seconds=3600
version=0.1.0-SNAPSHOT
url= ref= java= ivy= name= cpus= cache= tag= inject=yes
sbt=()

while (($#)); do
  case $1 in
    --url) url=$2 ;;
    --ref) ref=$2 ;;
    --java) java=$2 ;;
    --ivy) ivy=$2 ;;
    --out) out=$2 ;;
    --name) name=$2 ;;
    --image) image=$2 ;;
    --memory) memory=$2 ;;
    --heap) heap=$2 ;;
    --cpus) cpus=$2 ;;
    --timeout) timeout_seconds=$2 ;;
    --cache) cache=$2 ;;
    --version) version=$2 ;;
    --tag) tag=$2 ;;
    --bonsai) inject=$2 ;;
    --sbt) sbt+=("$2") ;;
    *)
      echo "unknown option: $1" >&2
      exit 2
      ;;
  esac
  shift 2
done
: "${url:?--url is required}" "${ref:?--ref is required}" "${java:?--java is required}" "${ivy:?--ivy is required}"
[[ -d $ivy/local ]] || { echo "no $ivy/local" >&2; exit 2; }
((${#sbt[@]})) || sbt=(bonsaiAnalyse)
[[ -n $name ]] || name=$(sed -E 's#/+$##; s#\.git$##; s#.*[:/]([^/]+/[^/]+)$#\1#; s#/#__#g' <<<"$url")

dir=$(realpath -m "$out")/$name
rm -rf "$dir"
mkdir -p "$dir"
chmod 777 "$dir"

mounts=()
if [[ -d $url ]]; then
  mounts+=(-v "$(realpath "$url"):/src:ro")
  url=file:///src
fi

cname=bonsai-community-$$-$RANDOM
volume=${cache:-$cname}
cleanup() {
  "$engine" rm -f "$cname" >/dev/null 2>&1 || true
  [[ -n $cache ]] || "$engine" volume rm -f "$volume" >/dev/null 2>&1 || true
}
trap cleanup EXIT

limits=(--memory "$memory" --memory-swap "$memory" --pids-limit 8192)
[[ -z $cpus ]] || limits+=(--cpus "$cpus")

set +e
timeout -k 60 $((timeout_seconds + 900)) "$engine" run --name "$cname" \
  --userns=keep-id:uid=1000,gid=1000 \
  --security-opt label=disable --security-opt no-new-privileges --cap-drop=all \
  "${limits[@]}" \
  -v "$volume:/home/bonsai" \
  -v "$(realpath "$ivy")/local:/home/bonsai/.ivy2/local:ro" \
  -v "$dir:/out" \
  -v "$here/build.sh:/opt/community/build.sh:ro" \
  "${mounts[@]}" \
  -e PROJECT_URL="$url" -e PROJECT_REF="$ref" -e JAVA_MAJOR="$java" -e PROJECT_TAG="$tag" -e BONSAI_INJECT="$inject" \
  -e TIMEOUT_SECONDS="$timeout_seconds" -e BONSAI_VERSION="$version" \
  -e SBT_OPTS="-Dsbt.color=false -Dsbt.supershell=false -Xmx$heap" \
  "$image" bash /opt/community/build.sh "${sbt[@]}"
code=$?
set -e

[[ -s $dir/status ]] || echo "runner-error:$code" >"$dir/status"
echo "$name: $(cat "$dir/status")"
