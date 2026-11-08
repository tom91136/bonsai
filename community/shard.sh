#!/usr/bin/env bash
# Runs the projects of one shard one after another within a time budget, through run.sh.
# usage: shard.sh <projects.jsonl> <shard> <of> <budget seconds> <per-project timeout seconds> [run.sh options]
# A project is a JSON object per line: {"url": ..., "ref": ..., "java": 17, "sbt": ["bonsaiAnalyse"], "name": ...};
# "sbt", "name" and "fetch_tag" (a tag, or ALL, to fetch) are optional. Project i belongs to shard i mod <of>.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
list=$1 shard=$2 of=$3 budget=$4 per_project=$5
shift 5
out=community-out
for ((i = 1; i <= $#; i++)); do
  if [[ ${!i} == --out ]]; then
    j=$((i + 1))
    out=${!j}
  fi
done

deadline=$((SECONDS + budget))
jq -c --argjson shard "$shard" --argjson of "$of" -s \
  'to_entries | map(select(.key % $of == $shard) | .value) | .[]' "$list" |
  while IFS= read -r project; do
    url=$(jq -r .url <<<"$project")
    ref=$(jq -r .ref <<<"$project")
    java=$(jq -r .java <<<"$project")
    name=$(jq -r '.name // empty' <<<"$project")
    tag=$(jq -r '.fetch_tag // empty' <<<"$project")
    args=(--url "$url" --ref "$ref" --java "$java")
    [[ -z $name ]] || args+=(--name "$name")
    [[ -z $tag ]] || args+=(--tag "$tag")
    while IFS= read -r command; do args+=(--sbt "$command"); done < <(jq -r '(.sbt // [])[]' <<<"$project")
    remaining=$((deadline - SECONDS - 600))
    limit=$((remaining < per_project ? remaining : per_project))
    if ((limit < 300)); then
      derived=${name:-$(sed -E 's#/+$##; s#\.git$##; s#.*[:/]([^/]+/[^/]+)$#\1#; s#/#__#g' <<<"$url")}
      mkdir -p "$out/$derived"
      echo not-run >"$out/$derived/status"
      echo "$derived: not-run (shard budget spent)"
      continue
    fi
    "$here/run.sh" "${args[@]}" --timeout "$limit" "$@" || echo "$url: runner failed" >&2
  done
