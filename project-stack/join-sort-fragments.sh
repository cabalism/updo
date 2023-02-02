#!/bin/sh
set -eu

WORKDIR=$(mktemp -d)
trap 'rm -rf "$WORKDIR"' EXIT

cd "$1"

for f in *.yaml
do
    cp "$PWD/${f}" "$WORKDIR/${f}"
done

cd "$WORKDIR"
for f in *.yaml
do
    yq '... comments=""' --inplace --prettyPrint "${f}"
done

yq '.extra-deps |= sort' --inplace constraints.yaml
yq '.packages |= sort_by(sub("\/", ""))' --inplace pkgs.yaml
yq '.extra-deps |= sort_by(.git | sub("(.*\.com:|.*\.com\/)(.+$)", "$2"))' --inplace deps-external.yaml
yq '.extra-deps |= sort_by(.git | sub("(.*\.com:|.*\.com\/)(.+$)", "$2"))' --inplace deps-internal.yaml
yq '.extra-deps |= sort_by(.git | sub("(.*\.com:|.*\.com\/)(.+$)", "$2"))' --inplace forks-external.yaml
yq '.extra-deps |= sort_by(.git | sub("(.*\.com:|.*\.com\/)(.+$)", "$2"))' --inplace forks-internal.yaml
yq eval-all '. as $item ireduce ({}; . *+ $item )' base.yaml pkgs.yaml deps-external.yaml deps-internal.yaml forks-external.yaml forks-internal.yaml constraints.yaml
