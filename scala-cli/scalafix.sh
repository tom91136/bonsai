#!/usr/bin/env bash
# Runs the bonsai scalafix rules over a Scala CLI project, rewriting its sources, or only reporting them with --check.
# The project's .scalafix.conf is used when it has one, otherwise every rule the rules artefact registers is enabled,
# with the rules, bans and settings it publishes for scalafix's own syntax, unused code and result types.
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
published() { unzip -p "$rules" "META-INF/bonsai/$1"; }
property() { published versions.properties | sed -nE "s/^$1=(.*)$/\1/p"; }
# The Scala version of the project, from the library it compiles against; empty when it cannot be told.
scala=$(scala-cli compile "$project" --server=false --print-class-path 2>/dev/null | tr ':' '\n' |
  sed -nE 's|.*/scala3-library_3-([^/]*)\.jar$|\1|p' | head -n 1 || true)
minor=$(sed -nE 's/^3\.([0-9]+)\.[0-9]+.*/\1/p' <<<"$scala")
patch=$(sed -nE 's/^3\.[0-9]+\.([0-9]+).*/\1/p' <<<"$scala")
supported() {
  [[ -z "$minor" ]] && return 0
  case "$1" in
    RemoveUnused)
      local from
      from=$(property removeUnused.from)
      local from_minor=${from#3.}
      from_minor=${from_minor%%.*}
      local from_patch=${from##*.}
      ((minor > from_minor || (minor == from_minor && patch >= from_patch)))
      ;;
    ExplicitResultTypes)
      ! tr ',' '\n' <<<"$(property explicitResultTypes.excluded)" | grep -qxF "3.$minor"
      ;;
    *) return 0 ;;
  esac
}
builtin=$(published builtin.id)
scalac=()
{
  echo 'rules = ['
  unzip -p "$rules" META-INF/services/scalafix.v1.Rule | sed -nE 's/^.*\.([^.]+)$/  \1/p'
  while read -r rule; do
    if supported "$rule"; then echo "  $rule"; fi
  done < <(awk '!/^-/ && !/=/ && !/\./' <<<"$builtin")
  echo ']'
  awk '!/^-/ && /=/' <<<"$builtin"
  awk '!/^-/ && !/=/ && /\./ { print $0 " = true" }' <<<"$builtin"
} >"$work/.scalafix.conf"
# The unused warnings RemoveUnused reads are asked for unless the project asks for its own; the -Wconf demotion that
# keeps fatal warnings from failing a build is left out, since Scala CLI drops the demoted diagnostics on some lines.
if supported RemoveUnused && ! grep -rqE -e '-(Wunused|Ywarn-unused|Wall)' --include='*.scala' --include='*.sc' "$project"; then
  while read -r option; do scalac+=("--scalac-option=$option"); done < <(awk '/^-/ && !/^-Wconf/' <<<"$builtin")
fi
config="$work/.scalafix.conf"
if [[ -f "$project/.scalafix.conf" ]]; then
  config="$project/.scalafix.conf"
  scalac=()
fi
scala-cli fix --power --enable-built-in=false --scalafix-conf="$config" --scalafix-arg=--tool-classpath="$tool" \
  "${scalac[@]}" "${check[@]}" "$project"
