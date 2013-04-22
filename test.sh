#!/bin/bash

cabal install
vagrant provision
py.test testsuite/ -s
