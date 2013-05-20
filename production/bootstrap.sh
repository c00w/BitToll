#!/bin/bash

set -e

ssh root@$1 "apt-get -y install git
git clone git://github.com/c00w/BitToll.git -b production
~/BitToll/production/local_boostrap.sh"
