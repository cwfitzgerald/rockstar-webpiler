#!/usr/bin/env bash

rm -r bundle
mkdir -p bundle/static/js
cp -r backend/{static/,templates/,*.py,requirements.txt} bundle/
sed '/sourceMappingURL/d' js/target/scala-2.12/{rockstar-jsdeps.min.js,rockstar-opt.js} | tr -d '\n' > bundle/static/js/rockstar.js
rsync -az --progress --exclude venv --exclude __pycache__ bundle/ rockstar.connorwfitzgerald.com:/mnt/volume-nyc1-webhost/webroot/rockstar.connorwfitzgerald.com/ --delete
ssh -t rockstar.connorwfitzgerald.com 'cd /mnt/volume-nyc1-webhost/webroot/rockstar.connorwfitzgerald.com/; source venv/bin/activate; pip install -r requirements.txt; deactivate; sudo supervisorctl restart rockstar'
