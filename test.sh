#!/bin/bash

cabal install
vagrant up
vagrant provision
py.test testsuite/ -s
