#!/bin/bash
set -e
cabal-dev install
vagrant provision
py.test testsuite/ -s
