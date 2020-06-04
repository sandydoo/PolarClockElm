#!/bin/sh

./build.sh

echo "Deploying to Github Pages..."

gh-pages -d dist
