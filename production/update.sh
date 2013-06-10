#!/bin/bash

set -e

git push origin $(git tag -l v\* | sort -V | tail -1)
ssh -t deploy@$1 "
cd /home/deploy/BitToll
git checkout master
git pull origin
git checkout $(git tag -l v\* | sort -V | tail -1)
sudo /home/deploy/BitToll/production/run_puppet.sh
"
