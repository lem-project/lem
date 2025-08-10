#!/usr/bin/env bash -ex

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
parent_dir="$(dirname "$script_dir")"

cd $parent_dir

qlot install
qlot exec sbcl --eval '(ql:quickload :lem)' --eval '(asdf:make :lem)'
cp resources/lem.png bin/lem.app/Contents/Resources/
