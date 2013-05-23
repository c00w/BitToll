#!/bin/bash

set -e

ssh root@$1 "
cd /home/deploy/BitToll
sudo -u deploy git pull origin production
/home/deploy/BitToll/production/run_puppet.sh
"
