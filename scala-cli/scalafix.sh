#!/usr/bin/env bash
# Runs the bonsai scalafix rules over a Scala CLI project, rewriting its sources, or only reporting them with --check.
# The project's .scalafix.conf is used when it has one, otherwise every rule the rules artefact registers is enabled.
# usage: scalafix.sh <project directory> [--check]
set -euo pipefail
project=$(cd "${1:?usage: scalafix.sh <project directory> [--check]}" && pwd)
shift
check=()
if [[ "${1:-}" == "--check" ]]; then
  check=(--check)
  shift
fi
(($# == 0)) || { echo "usage: scalafix.sh <project directory> [--check]" >&2; exit 2; }
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
printf '//> using dep net.kurobako:bonsai-scalafix_3:0.1.0-SNAPSHOT\nobject Resolve\n' >"$work/Resolve.scala"
classpath=$(scala-cli compile "$work" --server=false --platform jvm --print-class-path)
# The rules run on the Scala version their artefact resolves to, whatever the project is built with.
jars=$(tr ':' '\n' <<<"$classpath")
rules=$(grep -E '/bonsai-scalafix_3(-[^/]*)?\.jar$' <<<"$jars")
tool=$(grep -E '/(bonsai-scalafix_3|scala3-library_3|scala-library)(-[^/]*)?\.jar$' <<<"$jars" | paste -sd: -)
{
  echo 'rules = ['
  unzip -p "$rules" META-INF/services/scalafix.v1.Rule | sed -nE 's/^.*\.([^.]+)$/  \1/p'
  echo ']'
} >"$work/.scalafix.conf"
config="$work/.scalafix.conf"
if [[ -f "$project/.scalafix.conf" ]]; then config="$project/.scalafix.conf"; fi
scala-cli fix --power --enable-built-in=false --scalafix-conf="$config" --scalafix-arg=--tool-classpath="$tool" \
  "${check[@]}" "$project"
