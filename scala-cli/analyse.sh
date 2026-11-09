#!/usr/bin/env bash
# Runs the whole-build analysis over a Scala CLI project on the JVM: compiles it with its tests, writes the manifest the
# sbt plugin would, and runs the analysis with the options of its .bonsai.conf, then the remaining arguments, such as
# --mode strict.
# The analysis needs JDK 17 or later and runs on BONSAI_ANALYSIS_JAVA_HOME, else on the java on the PATH, which is also
# the JDK the manifest names as the target.
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
java_home=$(java -XshowSettings:properties -version 2>&1 | awk '$1 == "java.home" { print $3 }')
analysis_home=${BONSAI_ANALYSIS_JAVA_HOME:-$java_home}
analysis_major=$("$analysis_home/bin/java" -version 2>&1 |
  awk -F'"' '/version "/ { split($2, v, "."); print (v[1] + 0 == 1 ? v[2] : v[1]) + 0; exit }')
if ((${analysis_major:-0} < 17)); then
  source=${BONSAI_ANALYSIS_JAVA_HOME:+BONSAI_ANALYSIS_JAVA_HOME}
  echo "bonsai: the analysis needs JDK 17 or later but ${source:-the java on the PATH} is $analysis_home," \
    "JDK ${analysis_major:-unknown}; set BONSAI_ANALYSIS_JAVA_HOME to a JDK 17 or later" >&2
  exit 1
fi
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
  JAVA_HOME="$analysis_home" scala-cli run --server=false --platform jvm --jvm system -S "$scala" \
    --dep "net.kurobako:bonsai-analysis_3:0.1.0-SNAPSHOT" --java-opt -Xss1g --java-opt -Xmx4g "$@"
}
configuration="$runner/configuration.tsv"
tool -M net.kurobako.bonsai.analysis.Configure -- --config "$project/.bonsai.conf" --module "$name" \
  --out "$configuration"
manifest="$project/.scala-build/bonsai/manifest.tsv"
mkdir -p "$(dirname "$manifest")"
if grep -qx $'library\ttrue' "$configuration"; then library=true; fi
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
