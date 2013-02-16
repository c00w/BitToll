#!/usr/bin/env python2
import gevent
import requests
import json

ip_address = "192.168.56.101"
port = "3000"
url = ''.join(['http://', ip_address, ':', port])

def test_login():
    r = requests.get(url + '/register')
    assert r.status_code == 200
    info = json.loads(r.text)
    assert 'username' in info
    assert 'secret' in info
    assert len(info.keys()) == 2

