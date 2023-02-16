#!/usr/bin/env python3
#
# NOTE: From input-output-hk/iohk-nix, a repository with no license.
# SOURCE: https://github.com/input-output-hk/iohk-nix/blob/master/ci/sha256map-regenerate/sha256map-regenerate.py
#
# This script is intended to be run as
#
#  python3 sha256map.py
#
# And it will iterate over all git references in the given stack project (stack.yaml file)
# and generate a set of sha256 value for them to stdout.

# These can then be imported as:
# haskell-nix.stackProject' { sha256map = import ./sha256map.nix; }
#
# This allows building a project with haskell.nix in a restricted nix setup
# where network access is only permitted if the sha256 of the download is known
# beforehand.

# TODO: accept a cabal.project as input as well.

import re
import subprocess
import json
import sys

# Read stack.yaml from stin
project = sys.stdin.read()

# match looks like this:

#- git: https://github.com/input-output-hk/cardano-base
#  commit: e8a48cf0500b03c744c7fc6f2fedb86e8bdbe055

pattern = r'\- git: (?P<loc>[^ \n]+).*\n' \
        + r'(?P<pad> .*)commit: (?P<tag>[^ \n]+).*\n'

def sha256entry(match):
  dict = match.groupdict()
  prefetchJSON = subprocess.run(
    ["nix-prefetch-git", "--fetch-submodules", "--quiet", dict['loc'], dict['tag']],
    capture_output=True, check=True).stdout
  sha256 = json.loads(prefetchJSON)["sha256"]
  return '"{loc}"."{tag}" = "{sha256}";'.format(**{**dict, **{"sha256": sha256}})

# Write sha256map to stdout
print("{")
for x in sorted([sha256entry(match) for match in re.finditer(pattern, project)]):
  print(" ", x)
print("}")