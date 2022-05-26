#!/usr/bin/env bash

set -euo pipefail

readonly repo=${1:?"Please provide path to repository"}
readonly server_id=${2:?"Please provide server-id"}
readonly zone=${3:?"Please provide server zone"}
readonly ssh_key=${4:?"Please provide the ssh key"}

cat >> /etc/nix/nix.conf <<EOF
experimental-features = nix-command flakes
EOF

cachix use akshaymankar
cachix watch-exec akshaymankar -- nix build "$repo#ci-env"
export PATH="$PWD/result/bin:$PATH"

echo "Starting server ${server_id} in ${zone}"
scw instance server start "$server_id" zone="$zone"

builder_hostname="$server_id.pub.instances.scw.cloud"

echo "Waiting for AAAA record for $builder_hostname"
while true; do
    record=$(dig +short AAAA "$builder_hostname")
    if ! [[ -z "$record" ]]; then
        echo "$server_id.pub.instances.scw.cloud to $record"
        break;
    fi
    sleep 1;
done

key_file=$(mktemp)
echo "$ssh_key" > "$key_file"
echo "Waiting for SSH to work"
while true; do
    if ssh -o "StrictHostKeyChecking=no" "nix-builder@$builder_hostname" -i "$key_file" "echo Server alive" ; then
        echo "SSH succeeded"
        break;
    fi
done

echo "Remote builder started"
