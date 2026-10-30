#!/usr/bin/env bash
# Installs the bonsai tools into the Maven repository, so that a Maven build can resolve them.
#
# The tools have no published release, and their build publishes them to the Ivy repository of sbt, whose layout Maven
# does not read. This copies them across, with the poms their build published, so that the versions and the
# dependencies they declare are the ones Maven resolves.
#
# usage: publish-maven-local.sh [--version <version>] [--scala-versions <versions>]
set -euo pipefail

version=0.1.0-SNAPSHOT
scala_versions="3.9.0 3.10.0"
while [[ $# -gt 0 ]]; do
  case "$1" in
    --version) version=${2:?usage: publish-maven-local.sh [--version <version>]}; shift 2 ;;
    --scala-versions) scala_versions=${2:?usage: publish-maven-local.sh [--scala-versions <versions>]}; shift 2 ;;
    *) echo "publish-maven-local.sh: unknown argument: $1" >&2; exit 1 ;;
  esac
done

ivy=${IVY_HOME:-$HOME/.ivy2/local}/net.kurobako
maven=${MAVEN_COMMAND:-mvn}

# The compiler plugin and the analysis are built for every Scala version they support, the scalafix rules are built
# for Scala 3 as a whole, and the javac plugin runs inside the compiler of the build, so it is built for Scala 3 too.
artifacts=()
for scala_version in $scala_versions; do
  artifacts+=("bonsai-plugin_${scala_version}" "bonsai-analysis_${scala_version}")
done
artifacts+=("bonsai-scalafix_3" "bonsai-java-plugin_3")

for artifact in "${artifacts[@]}"; do
  source="$ivy/$artifact/$version"
  if [[ ! -d "$source" ]]; then
    echo "bonsai: $source is missing; build the tools with their own scripts/publish-local.sh" >&2
    exit 1
  fi
  "$maven" -q -o org.apache.maven.plugins:maven-install-plugin:3.1.4:install-file \
    -Dfile="$source/jars/$artifact.jar" \
    -DpomFile="$source/poms/$artifact.pom"
  echo "installed $artifact $version"
done
