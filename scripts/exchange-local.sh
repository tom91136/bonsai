#!/usr/bin/env bash
# Moves every version directory published under <staged> to the same path under <target>, exchanging it with the one
# there in a single rename. A version directory is the one that holds the ivys directory, whatever its depth: sbt 1
# plugins publish under <org>/<module>/scala_<scala>/sbt_<sbt>/<version>, everything else under <org>/<module>/<version>.
#
# usage: exchange-local.sh <staged> <target>
set -euo pipefail

staged=${1:?usage: exchange-local.sh <staged> <target>}
target=${2:?usage: exchange-local.sh <staged> <target>}

mapfile -d '' -t ivys < <(find "$staged" -type d -name ivys -print0 | sort -z)
for ivy in "${ivys[@]}"; do
  version=$(dirname "$ivy")
  relative=${version#"$staged/"}
  destination=$target/$relative
  mkdir -p "$(dirname "$destination")"
  if [[ -d "$destination" ]]; then
    mv --exchange --no-target-directory "$version" "$destination"
  else
    mv --no-target-directory "$version" "$destination"
  fi
  echo "published $relative"
done
