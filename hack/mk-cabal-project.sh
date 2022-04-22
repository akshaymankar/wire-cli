#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ROOT_DIR=$(cd -- "${SCRIPT_DIR}/.." &> /dev/null && pwd)

projFile="$ROOT_DIR/cabal.project"

echo "-- Warning: This is a generated file. PLEASE DO NOT EDIT. Run hack/mk-cabal-project.sh to regenerate." > "$projFile"
echo >> "$projFile"

while IFS="\n"; read gitPin; do
    cat >> "$projFile" <<EOF
source-repository-package
  type: git
  location: $(echo "$gitPin" | yq -r '.location')
  tag: $(echo "$gitPin" | yq -r '.commit')
$(echo "$gitPin" | yq -r 'if .subdirs == null then "" else "  subdir: \(.subdirs | join (" "))" end')

EOF
done < <(yq -c '.gitPins[]' "$ROOT_DIR/pins.yaml")

isFirstHackagePin="1"
while IFS="\n"; read hackagePin; do
    if [[ "$isFirstHackagePin" == "1" ]]; then
        echo "constraints: $(echo "$hackagePin" | yq -r '"\(.package) == \(.version)"' )"
        isFirstHackagePin="0"
    else
        echo "           , $(echo "$hackagePin" | yq -r '"\(.package) == \(.version)"' )"
    fi >> "$projFile"
done < <(yq -c '.hackagePins[]' "$ROOT_DIR/pins.yaml")

echo >> "$projFile"

cat "$ROOT_DIR/cabal.project.end" >> "$projFile"
