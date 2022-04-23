#!/usr/bin/env sh

set -eu

readonly repo=${1:?"Please provide path to repository"}

nix-env -iA nixpkgs.nixFlakes nixpkgs.git nixpkgs.cachix
cat >> /etc/nix/nix.conf <<EOF
experimental-features = nix-command flakes
EOF

cachix use akshaymankar
cachix watch-exec akshaymankar -- nix build -L "./$repo#wire-cli"
