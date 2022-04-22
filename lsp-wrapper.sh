#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

nix build -o "$SCRIPT_DIR/.hls-env" "$SCRIPT_DIR"

eval "$(direnv stdlib)"
env="$SCRIPT_DIR/.hls-env"
load_prefix "${env}"

haskell-language-server-wrapper "$@"
