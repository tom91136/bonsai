#!/usr/bin/env bash
# Installs the bonsai tools into the Maven repository, so that a Maven build can resolve them.
#
# The tools have no published release, and their build publishes them to the Ivy repository of sbt, whose layout Maven
# does not read. This copies them across, with the poms their build published, so that the versions and the
# dependencies they declare are the ones Maven resolves.
#
# usage: publish-maven-local.sh [--version <version>]
set -euo pipefail

version=0.1.0-SNAPSHOT
while [[ $# -gt 0 ]]; do
  case "$1" in
    --version) version=${2:?usage: publish-maven-local.sh [--version <version>]}; shift 2 ;;
    *) echo "publish-maven-local.sh: unknown argument: $1" >&2; exit 1 ;;
  esac
done

ivy=${IVY_HOME:-$HOME/.ivy2/local}/net.kurobako
maven=${MAVEN_COMMAND:-mvn}

# The compiler plugin is built for every minor line of Scala 3 it supports, the analysis and the lifting it reads the
# trees with for the newest, the scalafix rules for Scala 3 as a whole, and the javac plugin runs inside the compiler
# of the build, so it is built for Scala 3 too.
shopt -s nullglob
artifacts=()
for source in "$ivy"/bonsai-*_3*/"$version"; do
  [[ -d "$source" ]] && artifacts+=("$(basename "$(dirname "$source")")")
done
shopt -u nullglob
if (( ${#artifacts[@]} == 0 )); then
  echo "bonsai: no tools of version $version in $ivy; build them with their own scripts/publish-local.sh" >&2
  exit 1
fi

plugin=org.apache.maven.plugins:maven-install-plugin:3.1.4
# The installs below run offline, so the plugin itself has to be fetched first.
"$maven" -B -q "$plugin:help" >/dev/null

for artifact in "${artifacts[@]}"; do
  source="$ivy/$artifact/$version"
  "$maven" -q -o "$plugin:install-file" \
    -Dfile="$source/jars/$artifact.jar" \
    -DpomFile="$source/poms/$artifact.pom"
  echo "installed $artifact $version"
done
