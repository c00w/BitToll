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
    login = info
    return info

def test_balance(login):
    body = {}
    body['username'] = login['username']
    body['time'] = str(time.time())
    body['sign'] = secret(body, login['secret'])
    r = requests.post(url + '/balance', data=json.dumps(body))
    assert r.status_code == 200
    info = json.loads(r.text)
    assert 'balance' in info

def test_request(login):
    body = {}
    body['username'] = login['username']
    body['time'] = str(time.time())
    body['amount'] = "110"
    body['sign'] = secret(body, login['secret'])
    r = requests.post(url + '/request', data=json.dumps(body))
    assert r.status_code == 200
    info = json.loads(r.text)
    assert 'payment' in info

def test_mine(login):
    body = {}
    body['username'] = login['username']
    body['time'] = str(time.time())
    body['sign'] = secret(body, login['secret'])
    r = requests.post(url + '/mine', data=json.dumps(body))
    assert r.status_code == 200
    info = json.loads(r.text)

    r = requests.post(url + '/balance', data=json.dumps(body))
    assert r.status_code == 200
    info = json.loads(r.text)
    assert info['balance'] != u"0"


def pytest_funcarg__paidlogin(request, login):
    body = {}
    body['username'] = login['username']
    body['time'] = str(time.time())
    body['sign'] = secret(body, login['secret'])
    r = requests.post(url + '/deposit', data=json.dumps(body), timeout=1)
    assert r.status_code == 200
    info = json.loads(r.text)
    assert 'address' in info
    r2 = requests.post(url + '/deposit', data=json.dumps(body), timeout=1)
    assert r2.text == r.text

    print
    print 'Please send some bitcoins to %s' % info['address']
    print 'Then Hit Enter'
    raw_input()
    return login


def test_deposit(paidlogin):
    body = {}
    body['username'] = paidlogin['username']
    body['time'] = str(time.time())
    body['sign'] = secret(body, paidlogin['secret'])

    r2 = requests.post(url + '/balance', data=json.dumps(body))
    assert r2.status_code == 200

    info = json.loads(r2.text)
    assert info['balance'] != "0"

def test_payment(paidlogin):
    login = paidlogin
    body = {}
    body['username'] = login['username']
    body['time'] = str(time.time())
    body['amount'] = "0.1"
    body['sign'] = secret(body, login['secret'])
    r = requests.post(url + '/request', data=json.dumps(body))
    assert r.status_code == 200
    info = json.loads(r.text)
    assert 'payment' in info

    paymentid = info['payment']
    body = {}
    body['username'] = login['username']
    body['payment'] = paymentid
    body['sign'] = secret(body, login['secret'])
   
    r = requests.post(url + '/pay', data=json.dumps(body))
    assert r.status_code == 200
    info = json.loads(r.text)
    print info
