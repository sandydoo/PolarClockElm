#!/bin/sh

if [ -z $1 ]
then
  env="dev"
elif [ -n $1 ]
then
  env=$1
fi

input="src/Main.elm"
output="dist/clock.js"

case $env in
  dev)
    echo "Compiling for development.\n"
    elm make --output=$output $input;;

  prod)
    echo "Compiling for production.\n"
    elm make --optimize --output=$output $input;;
esac
