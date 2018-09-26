#!/bin/bash

# Taken + modified from typelevel/frameless

export publish_cmd="publishLocal"

sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION -Dfile.encoding=UTF8 -J-XX:ReservedCodeCacheSize=256M"

case "$PHASE" in
  A)
     docs_cmd="$sbt_cmd tut"
     run_cmd="$docs_cmd"
  ;;
  B)
     coverage="$sbt_cmd coverage test && sbt coverageReport && bash <(curl -s https://codecov.io/bash)"
     run_cmd="$coverage"
  ;;
  C)
     run_cmd="$sbt_cmd clean $publish_cmd"
  ;;
esac
eval $run_cmd
