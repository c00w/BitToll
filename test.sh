#!/bin/bash
set -e
cabal install
vagrant provision
py.test testsuite/ -s
