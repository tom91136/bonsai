#!/usr/bin/env bash
# Removes the SemanticDB files that the build wrote through a link into the store of sbt, which it makes read-only.
#
# The compiler writes the SemanticDB of a source it compiles again, and sbt has replaced the file with a link into its
# content-addressed store, where the file cannot be written. The links are removed so that the compiler writes the
# files anew.
set -euo pipefail

cd "$(dirname "$0")/.."

removed=0
while IFS= read -r link; do
  rm -f "$link"
  removed=$((removed + 1))
done < <(find . -name "*.semanticdb" -type l 2>/dev/null)

echo "removed $removed semanticdb links"
