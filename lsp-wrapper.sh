#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
direnv exec "$SCRIPT_DIR" haskell-language-server-wrapper "$@"
