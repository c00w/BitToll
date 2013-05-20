#!/bin/bash

puppet apply  --modulepath '../puppet_modules' ../puppet/base.pp --detailed-exitcodes || [ $? -eq 2 ]
