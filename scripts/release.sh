#! /usr/bin/env nix-shell
#! nix-shell ../nix/release-env/default.nix -i bash

set -euxo pipefail

# Since we are using the quick Nix installation, the gpg agent isn't started by a daemon.
# This may be a bit hacky, but we'll just explicitly load it.
gpgconf --reload gpg-agent

sbt ';ci-release;docs/docusaurusPublishGhpages'
