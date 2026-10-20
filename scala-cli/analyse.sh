#!/usr/bin/env bash
# Runs the whole-build analysis over a Scala CLI project on the JVM: compiles it with its tests, writes the manifest the
# sbt plugin would, and runs the analysis with the remaining arguments, such as --mode ci.
# usage: analyse.sh <project directory> [--library] [analysis options...]
set -euo pipefail
project=$(cd "${1:?usage: analyse.sh <project directory> [--library] [analysis options...]}" && pwd)
shift
library=false
if [[ "${1:-}" == "--library" ]]; then
  library=true
  shift
fi
name=$(basename "$project")
classpath=$(cd "$project" && scala-cli compile . --test --platform jvm --server=false --print-class-path)
scala=$(tr ':' '\n' <<<"$classpath" | grep -o 'scala3-library_3/[^/]*' | head -1 | cut -d/ -f2)
manifest="$project/.scala-build/baumkuchen/manifest.tsv"
mkdir -p "$(dirname "$manifest")"
{
  printf 'root\t%s\n' "$project"
  while IFS= read -r entry; do
    case "$entry" in
      */classes/main) printf 'output\t%s\tmain\t%s\n' "$name" "$entry" ;;
      */classes/test) printf 'output\t%s\ttest\t%s\n' "$name" "$entry" ;;
      *) printf 'classpath\t%s\n' "$entry" ;;
    esac
  done < <(tr ':' '\n' <<<"$classpath")
  if $library; then printf 'library\t%s\n' "$name"; fi
} >"$manifest"
# An empty directory keeps the project's own directives out of the run.
runner=$(mktemp -d)
trap 'rm -rf "$runner"' EXIT
cd "$runner"
scala-cli run --server=false --platform jvm -S "$scala" \
  --dep "net.kurobako:baumkuchen-analysis_$scala:0.1.0-SNAPSHOT" --java-opt -Xss1g --java-opt -Xmx4g \
  -M net.kurobako.baumkuchen.analysis.CLI -- --manifest "$manifest" "$@"
