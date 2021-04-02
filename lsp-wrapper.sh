#!/usr/bin/env bash

env="$(nix-build $PWD/direnv.nix -A devEnv --no-out-link)"
direnv="$(nix-build $PWD/direnv.nix -A direnv --no-out-link)/bin/direnv"
eval "$("$direnv" stdlib)"
load_prefix "${env}"

haskell-language-server-wrapper "$@"
