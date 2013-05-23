#!/bin/bash

set -e

ssh -t deploy@$1 "
cd /home/deploy/BitToll
git pull origin production
sudo /home/deploy/BitToll/production/run_puppet.sh
"
