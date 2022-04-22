#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

overridesDir=$(cd -- "$SCRIPT_DIR/../nix/haskell-overrides" &> /dev/null && pwd)
overridesFile="$overridesDir/overrides.nix"
pinsFile="$SCRIPT_DIR/../pins.yaml"

rm -rf "$overridesDir"
mkdir -p "$overridesDir"

echo "hsuper: hself: {" > "$overridesFile"

mkDrv() {
    local drv name
    drv=$(eval "$1")
    name=$(echo "$drv" | sed -n 's|.*pname = "\(.*\)";|\1|p')
    echo "$drv" > "$overridesDir/$name.nix"
    echo "  $name = hself.callPackage ./$name.nix {};" >> "$overridesFile"
}

gitCabal2Nix=$(cat <<-'EOF'
.gitPins[] |
  "cabal2nix \(.location) --no-check --no-haddock --revision \(.commit)" as $basecmd |
    if .subdirs == null
    then $basecmd
    else (
      .subdirs[] | "\($basecmd) --subpath \(.)"
    )
    end
EOF
            )

while IFS="\n"; read cmd; do
    mkDrv "$cmd"
done < <(yq -r "$gitCabal2Nix" "$pinsFile")

hackageCabal2Nix='.hackagePins[] | "cabal2nix cabal://\(.package)-\(.version) --no-check --no-haddock"'

while IFS="\n"; read cmd; do
    mkDrv "$cmd"
done < <(yq -r "$hackageCabal2Nix" "$pinsFile")

echo "}" >> "$overridesFile"
