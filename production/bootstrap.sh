#!/bin/bash

set -e

ssh root@$1 "apt-get -y install git
rm -r ~/*
git clone git://github.com/c00w/BitToll.git -b production --recursive
~/BitToll/production/setup_puppet.sh"