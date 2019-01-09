#!/usr/bin/env bash

rm -r bundle
mkdir -p bundle/static/js
cp -r backend/{static/,templates/,*.py,requirements.txt} bundle/
sbt fullOptJS
sed -i '/sourceMappingURL/d' bundle/static/js/rs.js bundle/static/bootstrap.min.css
rsync -az --progress --exclude venv --exclude __pycache__ bundle/ rockstar.connorwfitzgerald.com:/mnt/volume-nyc1-webhost/webroot/rockstar.connorwfitzgerald.com/ --delete
ssh -t rockstar.connorwfitzgerald.com "bash -c 'cd /mnt/volume-nyc1-webhost/webroot/rockstar.connorwfitzgerald.com/; source venv/bin/activate; pip install -r requirements.txt; deactivate; sudo chmod -R o+rX .; sudo supervisorctl restart rockstar'"
