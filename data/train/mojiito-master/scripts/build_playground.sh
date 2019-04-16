#!/usr/bin/env bash

PWD=`pwd`
SRCDIR=${PWD}/playground
DESTDIR=${PWD}/dist/playground
TSC="node --max-old-space-size=3000 ./node_modules/@angular/tsc-wrapped/src/main"
ROLLUP="../node_modules/.bin/rollup"
SERVER="${PWD}/node_modules/.bin/http-server"
SERVE=false

for ARG in "$@"; do
  case "$ARG" in
    --serve)
      SERVE=true
      ;;
    *)
      echo "Unknown option $ARG."
      echo "Available options: serve"
      exit 1
      ;;
  esac
done

rm -rf ${DESTDIR}

echo "====== COMPILING: ${TSC} -p ${SRCDIR}/tsconfig-build.json ====="
$TSC -p ${SRCDIR}/tsconfig.json

mkdir ${DESTDIR}/bundles
mkdir ${DESTDIR}/vendor

echo "====== LINKING VENDORS ====="
ln -s ${PWD}/node_modules/reflect-metadata/Reflect.js $DESTDIR/vendor/reflect.js
ln -s ${PWD}/node_modules/core-js/client/core.js $DESTDIR/vendor/core.js
ln -s ${PWD}/dist/packages-dist/ $DESTDIR/vendor/mojiito

echo "====== BUNDLING: ${SRCDIR} ====="
cd  ${SRCDIR}
$ROLLUP -c rollup.config.js

echo "====== COPYING HTML & CSS ====="
ln -s ${PWD}/index.html ${DESTDIR}/index.html
ln -s ${PWD}/styles.css ${DESTDIR}/styles.css

if [[ ${SERVE} == true ]]; then
echo "====== STARTING SERVER ====="
  cd ${DESTDIR}
  $SERVER -p 4200
fi
