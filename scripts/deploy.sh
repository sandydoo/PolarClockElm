#!/bin/sh

echo "Building app..."

yarn build

echo "Copying static files..."

cp public/CNAME dist/

echo "Deploying to Github Pages..."

gh-pages -d dist
