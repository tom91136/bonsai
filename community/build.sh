#!/usr/bin/env bash
# Runs inside the community image: clones one project, injects bonsai, runs sbt and writes the outputs to /out.
# usage: build.sh <sbt command>...
# environment: PROJECT_URL PROJECT_REF JAVA_MAJOR TIMEOUT_SECONDS BONSAI_VERSION, and optionally PROJECT_TAG and BONSAI_INJECT
# BONSAI_INJECT=no builds the project untouched, as the control of a run.
# PROJECT_TAG is a tag to fetch beside the ref, or ALL to unshallow the clone with every tag, for builds that read git tags.
# sbt runs on JAVA_MAJOR, which the build targets; the analysis runs on JDK 25.
set -uo pipefail

out=/out
src=/work/src
url=${PROJECT_URL:?}
ref=${PROJECT_REF:?}
major=${JAVA_MAJOR:?}
limit=${TIMEOUT_SECONDS:?}
version=${BONSAI_VERSION:?}

export JAVA_HOME=/opt/jdk/$major
export PATH=$JAVA_HOME/bin:$PATH
analysis_jdk=/opt/jdk/25

inject() {
  [[ ${BONSAI_INJECT:-yes} == no ]] && return
  mkdir -p project
  echo "addSbtPlugin(\"net.kurobako\" % \"sbt-bonsai\" % \"$version\")" >project/bonsai-community.sbt
  echo "ThisBuild / bonsaiAnalysisJavaHome := file(\"$analysis_jdk\")" >bonsai-community.sbt
}

now() { date +%s.%N; }
elapsed() { awk -v a="$1" -v b="$2" 'BEGIN { printf "%.1f", b - a }'; }

mkdir -p "$out"
: >"$out/timings.tsv"
t0=$(now)
mkdir -p "$src"
cd "$src"
git init -q
git remote add origin "$url"
if git -c safe.directory='*' fetch -q --depth 1 origin "$ref" 2>"$out/clone.log" && git checkout -q FETCH_HEAD 2>>"$out/clone.log"; then
  commit=$(git rev-parse HEAD)
  if [[ ${PROJECT_TAG:-} == ALL ]]; then
    git fetch -q --unshallow --tags origin 2>>"$out/clone.log" || true
  elif [[ -n ${PROJECT_TAG:-} ]]; then
    git fetch -q --depth 1 origin "refs/tags/$PROJECT_TAG:refs/tags/$PROJECT_TAG" 2>>"$out/clone.log" || true
  fi
else
  echo clone-failed >"$out/status"
  exit 0
fi
t1=$(now)
printf 'clone\t%s\n' "$(elapsed "$t0" "$t1")" >>"$out/timings.tsv"

inject

set +e
: >"$out/build.log"
(tail -n +1 -F "$out/build.log" 2>/dev/null | while IFS= read -r line; do printf '%s %s\n' "$EPOCHREALTIME" "$line"; done >"$out/build.ts.log") &
stamper=$!
timeout -k 30 "$limit" sbt --server --batch "$@" >"$out/build.log" 2>&1
code=$?
t2=$(now)
sleep 3
pkill -P "$stamper" 2>/dev/null
kill "$stamper" 2>/dev/null
printf 'build\t%s\n' "$(elapsed "$t1" "$t2")" >>"$out/timings.tsv"

oom=$(sed -n 's/^oom_kill //p' /sys/fs/cgroup/memory.events 2>/dev/null || echo 0)
case $code in
  0) status=ok ;;
  124) status=timeout ;;
  137) status=killed ;;
  *) status=failed ;;
esac
if [[ $status != timeout && ${oom:-0} -gt 0 ]]; then status=oom; fi
echo "$status" >"$out/status"

cat >"$out/meta.tsv" <<EOF
url	$url
ref	$ref
commit	$commit
java	$major
bonsai	$version
sbt	$*
exit	$code
oom_kill	${oom:-0}
EOF
cat /sys/fs/cgroup/memory.peak >"$out/peak-memory-bytes" 2>/dev/null || true

manifest_name() { sed 's#^/work/src/##; s#/#__#g; s#^\.#_#' <<<"$1"; }

sample_peak() {
  local pid=$1 peak=0 now
  while kill -0 "$pid" 2>/dev/null; do
    now=$(awk '/^VmHWM/ { print $2 }' "/proc/$pid/status" 2>/dev/null || echo 0)
    [[ ${now:-0} -gt $peak ]] && peak=$now
    sleep 1
  done
  echo "$peak"
}

mkdir -p "$out/manifests" "$out/info"
find "$src" -path '*/target/*' -path '*/bonsai/manifest.tsv' -print0 |
  while IFS= read -r -d '' file; do
    name=$(manifest_name "$file")
    cp "$file" "$out/manifests/$name"
    [[ -n ${tool_cp:-} ]] || tool_cp=$(cs fetch --classpath -r ivy2Local "net.kurobako:bonsai-analysis_3:$version" 2>"$out/info/cs.err")
    [[ -n $tool_cp ]] || continue
    started=$(now)
    "$analysis_jdk/bin/java" -Xss1g -Xmx6g -cp "$tool_cp" net.kurobako.bonsai.analysis.CLI --manifest "$file" --mode relaxed --info \
      >"$out/info/$name.out" 2>"$out/info/$name.err" &
    pid=$!
    peak=$(sample_peak "$pid")
    wait "$pid"
    exit_code=$?
    printf '%s\t%s\t%s\tpeak_kb=%s\n' "$name" "$exit_code" "$(elapsed "$started" "$(now)")" "$peak" >>"$out/info/runs.tsv"
  done

grep -E $'^(error|warning|info)\t|\\[bonsai:' "$out/build.log" >"$out/report.txt" || true

if [[ $status != ok ]]; then
  {
    find / -xdev -name 'hs_err_pid*.log' -newer "$out/clone.log" -print 2>/dev/null | while IFS= read -r file; do
      echo "== $file"
      cat "$file"
    done
    echo "== tail of build.log"
    tail -n 200 "$out/build.log"
  } >"$out/crash.txt"
fi
exit 0
