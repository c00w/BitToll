#!/bin/bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
sudo $DIR/../production/setup_puppet.sh
FACTER_curruser=$(whoami) puppet apply  --modulepath "$DIR/../puppet_modules" $DIR/../puppet/dev.pp --detailed-exitcodes || [ $? -eq 2 ]
