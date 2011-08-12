#!/usr/bin/env bash
set -ueo pipefail
scriptDir=$(scala -e 'println(new java.io.File("'$(dirname $0)'").getAbsolutePath)')

echo >&2 "Building source..."
mkdir -p $scriptDir/bin
find $scriptDir/scala \
  | egrep '\.scala$' \
  | xargs fsc -d $scriptDir/bin/

echo >&2 "Building JAR..."
(cd $scriptDir/bin; zip -qr $scriptDir/akerblad.jar *)
