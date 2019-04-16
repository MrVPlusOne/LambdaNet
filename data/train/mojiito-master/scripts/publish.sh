#!/usr/bin/env bash


PACKAGES=(core
  platform-browser)

for PACKAGE in ${PACKAGES[@]}
do
  echo "====== ${bold}PUBLISHING to NPM: ${green}${PACKAGE}${normal} ====="
  DESTDIR=${PWD}/dist/packages-dist/${PACKAGE}
  cd $DESTDIR
  npm publish
  cd ../../../
done
