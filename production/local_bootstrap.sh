#!/bin/bash

set -e

echo "# Puppetlabs products
deb http://apt.puppetlabs.com precise main
deb-src http://apt.puppetlabs.com precise main

# Puppetlabs dependencies
deb http://apt.puppetlabs.com precise dependencies
deb-src http://apt.puppetlabs.com precise dependencies
" > /etc/apt/sources.list.d/puppetlabs.list
apt-get -y update
apt-get -y install puppet
