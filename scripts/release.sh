#! /usr/bin/env nix-shell
#! nix-shell ../nix/release-env/default.nix -i bash

set -euxo pipefail

sbt ';ci-release;docs/docusaurusPublishGhpages'
