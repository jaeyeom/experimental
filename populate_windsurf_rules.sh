#!/bin/sh
mkdir -p $PWD/.windsurf/rules && for file in ~/.windsurf/rules/*; do ln -sf "$file" $PWD/.windsurf/rules/; done
