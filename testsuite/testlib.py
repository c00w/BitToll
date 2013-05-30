import requests
import json
import time
import hashlib
import fabric

ip_address = "vm"
port = "3000"
url = ''.join(['http://', ip_address, ':', port])

from fabric.api import task, run, env
from fabric.tasks import execute

env.user = 'vagrant'
env.password = 'vagrant'

def set_port(new_port):
    global port, url

    port = new_port
    url = ''.join(['http://', ip_address, ':', port])

def set_server(new_server):
    global ip_address, url

    ip_address = new_server
    url = ''.join(['http://', ip_address, ':', port])

@task
def _send_bc(address):
    run("bitcoind sendtoaddress %s 1" % address)

@task
def _get_mine_address():
    return run("RDTSC_FREQUENCY=2500 /usr/bin/PoolWrapper")

@task
def _newblock():
    return run("/usr/bin/NewBlock gobbledygook")

@task
def _mine_user(username):
    return run("/usr/bin/MineUserTest %s" % username)

@task
def _flush_db():
    run("redis-cli FLUSHALL")

def send_1btc(address):
    execute(_send_bc,address, host='vm')

def get_mine_addr():
    return execute(_get_mine_address, host='vm')['vm']

def run_newblock():
    return execute(_newblock, host='vm')['vm']

def mine_user(username):
    return execute(_mine_user, username, host='vm')['vm']

def flushdb():
    execute(_flush_db, host='vm')

def send_to_address():
    env.hosts = [ip_address]

def _secret(params, secret):
    keys = list(params.keys())
    keys.sort()
    hash_str = ""
    for key in keys:
        hash_str += (params[key])
        md5 = hashlib.md5()
        try:
            md5.update(hash_str)
        except:
            md5.update(hash_str.encode('utf-8'))
    md5.update(secret)
    return md5.hexdigest()

def register():
    return apicall('register', {})

def balance(userid, secret):
    body = {}
    body['username'] = userid
    body['time'] = str(time.time())
    body['sign'] = _secret(body, secret)
    return apicall('balance', body)

def withdraw(userid, secret, address, amount):
    body = {}
    body['username'] = userid
    body['time'] = str(time.time())
    body['address'] = address
    body['amount'] = amount
    body['sign'] = _secret(body, secret)
    return apicall('withdraw', body)

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

def apicall(name, body, auth=None):
    r = requests.post(url +'/' + name, data=json.dumps(body), auth=auth)
    assert r.status_code == 200
    info = json.loads(r.text)
    return info

def mine(userid, secret):
    body = {}
    body['params'] = []
    body['method'] = 'getwork'
    body['id'] = 1
    return apicall('mine', body, (userid, ''))

