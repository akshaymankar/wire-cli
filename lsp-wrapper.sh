#!/usr/bin/env bash

env="$(nix-build $PWD/direnv.nix -A devEnv --no-out-link)"
direnv="$(nix-build $PWD/direnv.nix -A direnv --no-out-link)/bin/direnv"
eval "$("$direnv" stdlib)"
load_prefix "${env}"

ulimit -c unlimited
# ~/.cabal/bin/haskell-language-server "$@" 2>/tmp/hls-stdout.log
haskell-language-server "$@" 2>/tmp/hls-stdout.log
# ghcide --lsp --verbose 2>/tmp/hls-stdout.log
