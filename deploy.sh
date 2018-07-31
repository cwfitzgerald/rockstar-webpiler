#!/usr/bin/env bash

rm -r bundle
mkdir bundle
sed 's/.\/js\/target\/scala-2.12\//.\//; s/-fastopt.js/.js/; /jsdeps.js/d' test.html > bundle/test.html
sed '/sourceMappingURL/d' js/target/scala-2.12/{rockstar-jsdeps.min.js,rockstar-opt.js} > bundle/rockstar.js
cp bootstrap.min.css bundle/bootstrap.min.css
rsync -az --progress bundle/ rockstar.connorwfitzgerald.com:/mnt/volume-nyc1-webhost/webroot/rockstar.connorwfitzgerald.com/ --delete
