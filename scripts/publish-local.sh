#!/usr/bin/env bash
# Publishes the snapshot to ~/.ivy2/local: the compiler plugin for every minor line of Scala 3 it supports, and the
# rest, analysis included, for the newest. Each module's version directory is built aside and
# exchanged with the published one in a single rename, so a build resolving it meanwhile sees the old module or the
# new one, never a half-written one.
set -euo pipefail
cd "$(dirname "$0")/.."
# An exchange is a rename, which stays within one file system.
stage=$(mktemp -d "$HOME/.ivy2/.stage.XXXXXX")
trap 'rm -rf "$stage"' EXIT
# The batch run needs the build to itself.
sbt --client shutdown >/dev/null 2>&1 || true
sbt --batch -Dsbt.ivy.home="$stage" -Divy.home="$stage" \
  publishLocalAll
for version in "$stage"/local/*/*/*/; do
  relative=${version#"$stage/local/"}
  target="$HOME/.ivy2/local/${relative%/}"
  mkdir -p "$(dirname "$target")"
  if [[ -d "$target" ]]; then
    mv --exchange --no-target-directory "${version%/}" "$target"
  else
    mv --no-target-directory "${version%/}" "$target"
  fi
  echo "published $relative"
done
