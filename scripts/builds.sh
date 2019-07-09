#! /usr/bin/env nix-shell
#! nix-shell ../shell.nix -i bash

# adapted from https://github.com/higherkindness/droste/blob/bad7dae60a0c32e66918613068d6713d209cc6b4/scripts/ci-jobs.sh

set -euxo pipefail

case "$1" in
    "docs")
        sbt 'docs'
        ;;
    "lint")
        sbt 'lint'
        ;;
    "validate")
        sbt 'validate'
        ;;
    "testJVM")
        sbt '+jvm/test'
        ;;
    "testJS")
        sbt '+js/test'
        ;;
    "coverage")
        sbt ';coverage;jvm/test;coverageReport'
        bash <(curl -s https://codecov.io/bash)
        ;;
    "release")
        sbt 'ci-release'
        ;;
    "publish-docs")
        sbt 'docs/docusaurusPublishGhpages'
        ;;
    *)
        echo "no command specified!"
        exit 1
        ;;
esac
