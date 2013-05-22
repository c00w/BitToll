#!/bin/bash

set -e

ssh root@$1 "
echo $2 > /etc/hostname
hostname -F /etc/hostname
echo \"127.0.0.1    localhost\" > /etc/hosts
echo \"127.0.1.1    $2.m.bittoll.com $2\" >> /etc/hosts
echo \"$1   $2.m.bittoll.com $2\" >> /etc/hosts
apt-get -y install git
rm -r ~/*
git clone git://github.com/c00w/BitToll.git -b production --recursive
~/BitToll/production/setup_puppet.sh"
