#!/usr/bin/env python2
import gevent
import requests
import json
import time
import hashlib

ip_address = "vm"
port = "3000"
url = ''.join(['http://', ip_address, ':', port])

def secret(params, secret):
    keys = params.keys()
    keys.sort()
    hash_str = ""
    for key in keys:
        hash_str += (params[key])
    md5 = hashlib.md5()
    md5.update(hash_str)
    return md5.hexdigest()

def test_login(login):
    info = login
    assert 'username' in info
    assert 'secret' in info
    assert len(info.keys()) == 2

def pytest_funcarg__login(request):
    r = requests.get(url + '/register')
    assert r.status_code == 200
    info = json.loads(r.text)
    assert 'username' in info
    assert 'secret' in info
    return info
   
def test_balance(login):
    body = {}
    body['username'] = login['username']
    body['time'] = str(time.time())
    body['sign'] = secret(body, login['secret'])
    r = requests.post(url + '/balance', data=json.dumps(body))
    print r.text
    assert r.status_code == 200
    info = json.loads(r.text)
    assert 'balance' in info

