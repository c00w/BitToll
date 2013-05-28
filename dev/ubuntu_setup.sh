#!/bin/bash

set -e

# Not working
#DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#sudo $DIR/../production/setup_puppet.sh
#sudo FACTER_curruser=$(whoami) puppet apply  --modulepath "$DIR/../puppet_modules" $DIR/../puppet/dev.pp --detailed-exitcodes || [ $? -eq 2 ]

wget http://files.vagrantup.com/packages/7e400d00a3c5a0fdf2809c8b5001a035415a607b/vagrant_1.2.2_x86_64.deb
sudo dpkg -i vagrant_1.2.2_x86_64.deb
rm vagrant_1.2.2_x86_64.deb

sudo apt-get -y install python-pip python-requests
sudo pip install fabric pytest

echo "192.168.56.2    vm  " >> /etc/hosts

