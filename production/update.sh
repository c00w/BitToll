#!/bin/bash

set -e

ssh -t deploy@$1 "
cd /home/deploy/BitToll
git checkout master
git pull origin
git checkout $(git tag -l v\* | tail -1)
sudo /home/deploy/BitToll/production/run_puppet.sh
"
