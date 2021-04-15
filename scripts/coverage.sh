#! /usr/bin/env nix-shell
#! nix-shell ../nix/jvm-env/default.nix -i bash

set -euxo pipefail

sbt ';coverage;jvm/test;coverageReport'
