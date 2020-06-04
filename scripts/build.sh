#!/bin/sh

echo "Building app..."

yarn parcel build public/index.html

echo "Copying static files..."

cp public/CNAME dist/
