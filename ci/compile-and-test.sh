#!/usr/bin/env bash

set -euo pipefail

readonly repo=${1:?"Please provide path to repository"}

cat >> /etc/nix/nix.conf <<EOF
experimental-features = nix-command flakes
EOF

cachix use akshaymankar
cachix watch-exec akshaymankar -- nix build -L "./$repo#wire-cli"
