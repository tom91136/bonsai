#!/usr/bin/env bash
# Runs the working tree's analysis over a reference build's manifest and, given a copy of that build, its compile
# checks, then compares the findings of each rule with the previous run over the same build and lists the rules whose
# count jumped; the findings of the last run stay beside the counts. The copy is compiled against a snapshot published
# aside, so ~/.ivy2/local is left alone; never pass the build's own working tree, which the compile writes to.
# usage: corpus.sh <manifest> [<copy of the build directory>]
set -euo pipefail
manifest=$(realpath "${1:?usage: corpus.sh <manifest> [<copy of the build directory>]}")
copy=${2:+$(realpath "$2")}
cd "$(dirname "$0")/.."
history="$HOME/.cache/baumkuchen-corpus/$(sha1sum <<<"$manifest" | cut -c1-12)"
mkdir -p "$history"
stage=$(mktemp -d)
trap 'rm -rf "$stage"' EXIT
sbt --client shutdown >/dev/null 2>&1 || true
# One heavy JVM at a time: the host is shared with other builds.
sbt --batch -Dsbt.ivy.home="$stage" -Divy.home="$stage" 'analysis/analysisClasspath' | grep -v '^\[' |
  tail -1 >"$stage/classpath"
java -Xss1g -Xmx4g -cp "$(cat "$stage/classpath")" net.kurobako.baumkuchen.analysis.CLI --manifest "$manifest" \
  --mode dev --info >"$stage/analysis.txt" 2>"$stage/analysis.err" || true
cut -f2 "$stage/analysis.txt" | sort | uniq -c | awk '{ print $2 "\t" $1 }' >"$stage/counts.tsv"
if [[ -n "$copy" ]]; then
  sbt --batch -Dsbt.ivy.home="$stage" -Divy.home="$stage" \
    'publishLocal; ++3.10.0; plugin/publishLocal; analysis/publishLocal' >/dev/null
  (cd "$copy" && sbt --batch -Dsbt.ivy.home="$stage" -Divy.home="$stage" \
    'set Global / localCacheDirectory := file(sys.props("java.io.tmpdir") + "/baumkuchen-corpus-cache")' \
    'Test/compile' >"$stage/compile.txt" 2>&1 || true)
  grep -o '\[baumkuchen:[a-z-]*\]' "$stage/compile.txt" | tr -d '[]' | sed 's/^baumkuchen://' | sort | uniq -c |
    awk '{ print $2 "\t" $1 }' >>"$stage/counts.tsv"
fi
grep '^problem:' "$stage/analysis.err" || true
previous="$history/counts.tsv"
printf '%-32s %8s %8s\n' rule before after
join -t $'\t' -a 1 -a 2 -e 0 -o 0,1.2,2.2 <(sort "$previous" 2>/dev/null || true) <(sort "$stage/counts.tsv") |
  while IFS=$'\t' read -r rule before after; do
    change=$((after - before))
    # A jump of more than half, and of more than a handful, is worth a look before publishing.
    if ((${change#-} > 20 && ${change#-} * 2 > before)); then mark=' <- jump'; else mark=''; fi
    printf '%-32s %8d %8d%s\n' "$rule" "$before" "$after" "$mark"
  done
cp "$stage/counts.tsv" "$previous"
cp "$stage/analysis.txt" "$history/analysis.txt"
