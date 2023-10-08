#!/usr/bin/env bash

set -euo pipefail

readonly repo=${1:?"Please provide path to repository"}
readonly server_id=${2:?"Please provide server-id"}
readonly zone=${3:?"Please provide server zone"}

cat >> /etc/nix/nix.conf <<EOF
experimental-features = nix-command flakes
EOF

cachix watch-exec akshaymankar -- nix build "$repo#ci-env"
export PATH="$PWD/result/bin:$PATH"

echo "Stopping server ${server_id} in ${zone}"
scw instance server stop "$server_id" zone="$zone"

echo "Remote builder stopped"
