#!/usr/bin/env bash
set -euo pipefail

exec sudo openvpn --config "$HOME/dev/vpns/granter_gferreira@vpn.ciasc.gov.br.ovpn"
