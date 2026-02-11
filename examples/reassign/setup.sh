#!/usr/bin/env bash
#
# Lightweight setup script for building the reassign escript
# This script only does the essential setup without extras
#

set -e

# Create _checkouts directory with symlink to kafcod
mkdir -p _checkouts
cd _checkouts
ln -sf ../../.. kafcod
cd ..

# Build the escript
rebar3 escriptize

# Create symlink to escript
ln -sf _build/default/bin/reassign .

echo "Build complete: ./reassign"
