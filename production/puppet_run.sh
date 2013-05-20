#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

puppet apply  --modulepath '$DIR/../puppet_modules' $DIR/../puppet/base.pp --detailed-exitcodes || [ $? -eq 2 ]
