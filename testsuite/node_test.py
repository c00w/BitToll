#!/usr/bin/env python2
from testlib import *

set_port("80")
set_server("us.bittoll.com")

def test_page_error():
    r = apicall("nonexistant", "hi")
    assert "error" in r
    assert r["error"] != ""
    assert int(r["error_code"]) == 1

def test_login(login):
    info = login
    assert 'username' in info
    assert 'secret' in info

def pytest_funcarg__login(request):
    info = register()
    assert 'username' in info
    assert 'secret' in info
    return info

def test_balance(login):
    info = balance(login['username'], login['secret'])
    assert 'balance' in info
    assert int(info["error_code"]) == 0

def test_request(login):
    info = request(login['username'], login['secret'], amount=1)
    assert 'payment' in info
    assert int(info["error_code"]) == 0

def test_mine(login):
    info = mine(login['username'], login['secret'])
    assert 'result' in info
    assert 'id' in info
    assert 'error' in info
