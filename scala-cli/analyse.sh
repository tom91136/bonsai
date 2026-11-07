#!/usr/bin/env bash
# Runs the whole-build analysis over a Scala CLI project on the JVM: compiles it with its tests, writes the manifest the
# sbt plugin would, and runs the analysis with the options of its .bonsai.conf, then the remaining arguments, such as
# --mode ci.
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
# The build server compiles .java sources, which --server=false skips.
classpath=$(cd "$project" && scala-cli compile . --test --platform jvm --print-class-path)
# The analysis runs on the Scala version it is built with, whatever the project is built with.
analysis=${IVY_HOME:-$HOME/.ivy2/local}/net.kurobako/bonsai-analysis_3/0.1.0-SNAPSHOT/jars/bonsai-analysis_3.jar
scala=$(unzip -p "$analysis" META-INF/bonsai/scala.id)
# An empty directory keeps the project's own directives out of the runs.
runner=$(mktemp -d)
trap 'rm -rf "$runner"' EXIT
cd "$runner"
tool() {
  scala-cli run --server=false --platform jvm -S "$scala" \
    --dep "net.kurobako:bonsai-analysis_3:0.1.0-SNAPSHOT" --java-opt -Xss1g --java-opt -Xmx4g "$@"
}
configuration="$runner/configuration.tsv"
tool -M net.kurobako.bonsai.analysis.Configure -- --config "$project/.bonsai.conf" --module "$name" \
  --out "$configuration"
manifest="$project/.scala-build/bonsai/manifest.tsv"
mkdir -p "$(dirname "$manifest")"
if grep -qx $'library\ttrue' "$configuration"; then library=true; fi
java_home=$(java -XshowSettings:properties -version 2>&1 | awk '$1 == "java.home" { print $3 }')
{
  printf 'root\t%s\n' "$project"
  printf 'jdk\t%s\n' "$java_home"
  while IFS= read -r entry; do
    case "$entry" in
      */classes/main) printf 'output\t%s\tmain\t%s\n' "$name" "$entry" ;;
      */classes/test) printf 'output\t%s\ttest\t%s\n' "$name" "$entry" ;;
      *) printf 'classpath\t%s\t%s\n' "$name" "$entry" ;;
    esac
  done < <(tr ':' '\n' <<<"$classpath")
  if $library; then printf 'library\t%s\n' "$name"; fi
  awk -F'\t' '$1 == "text" || $1 == "text-exclude"' "$configuration"
} >"$manifest"
mapfile -t options < <(awk -F'\t' '$1 == "option" { print $2 }' "$configuration")
tool -M net.kurobako.bonsai.analysis.CLI -- --manifest "$manifest" "${options[@]}" "$@"
