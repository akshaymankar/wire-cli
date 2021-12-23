#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

nix build -o "$SCRIPT_DIR/.hls-env" "$SCRIPT_DIR"

eval "$(direnv stdlib)"
env="$SCRIPT_DIR/.hls-env"
load_prefix "${env}"
path_add XDG_DATA_DIRS "${env}/share"
path_add GI_TYPELIB_PATH "${env}/lib/girepository-1.0"

haskell-language-server-wrapper "$@"
