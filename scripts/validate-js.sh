#! /usr/bin/env nix-shell
#! nix-shell ../nix/dev-env/default.nix -i bash

set -euxo pipefail

sbt ';+js/test;docs;'
