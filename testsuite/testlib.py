import requests
import json
import time
import hashlib

ip_address = "vm"
port = "3000"
url = ''.join(['http://', ip_address, ':', port])

def _secret(params, secret):
    keys = params.keys()
    keys.sort()
    hash_str = ""
    for key in keys:
        hash_str += (params[key])
        md5 = hashlib.md5()
        md5.update(hash_str)
    return md5.hexdigest()

def register():
    return apicall('register', {})

def balance(userid, secret):
    body = {}
    body['username'] = userid
    body['time'] = str(time.time())
    body['sign'] = _secret(body, secret)
    return apicall('balance', body)

def mine(userid, secret):
    body = {}
    body['username'] = userid
    body['time'] = str(time.time())
    body['sign'] = _secret(body, secret)
    return apicall('mine', body)

def deposit(userid, secret):
    body = {}
    body['username'] = userid
    body['time'] = str(time.time())
    body['sign'] = _secret(body, secret)
    return apicall('deposit', body)

def pay(userid, secret, paymentid ):
    body = {}
    body['username'] = userid
    body['time'] = str(time.time())
    body['payment'] = paymentid
    body['sign'] = _secret(body, secret)
    return apicall('pay', body)

def request(userid, secret, amount):
    body = {}
    body['username'] = userid
    body['time'] = str(time.time())
    body['amount'] = str(amount)
    body['sign'] = _secret(body, secret)
    return apicall('request', body)

def apicall(name, body):
    r = requests.post(url +'/' + name, data=json.dumps(body))
    assert r.status_code == 200
    info = json.loads(r.text)
    return info
