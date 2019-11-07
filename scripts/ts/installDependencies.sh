#!/usr/bin/env bash
npm install -g typescript
npm i fs source-map-support yargs @types/node @types/source-map-support @types/typescript @types/yargs
# remember to compile the scripts under scripts/ts using tsc

hash cloc  # we need the cloc command in order to prepare the repos