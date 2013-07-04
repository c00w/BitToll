#!/usr/bin/env python2
from testlib import *
import pytest

localonly = pytest.mark.skipif("config.option.server")

@pytest.fixture
def glob(pytestconfig):
    if (pytestconfig.option.server):
        set_server(pytestconfig.option.server)

def test_page_error():
    r = apicall("nonexistant", "hi")
    assert "error" in r
    assert int(r["error_code"]) == 1
    assert r["error"] != ""

def test_login(login):
    info = login
    assert 'username' in info
    assert 'secret' in info

@pytest.fixture
def login(request, glob):
    info = register()
    assert 'username' in info
    assert 'secret' in info
    return info

def test_balance(login):
    info = balance(login['username'], login['secret'])
    assert 'balance' in info
    assert int(info["error_code"]) == 0

def test_user_exception(login):
    info = balance(login['username'], login['secret']+"1")
    assert int(info["error_code"]) == 1

def test_request(login):
    info = request(login['username'], login['secret'], amount=1)
    assert 'paymentid' in info
    assert int(info["error_code"]) == 0

    info = requestinfo(login['username'], login['secret'], info["paymentid"])
    assert float(info["amount"]) == 1
    assert float(info["balance"]) == 0
    assert int(info["error_code"]) == 0


def test_alias(login):
    info = setalias(login['username'], login['secret'], login['username'], login['username'])
    assert int(info["error_code"]) == 0

    info = alias(login['username'], login['username'])
    assert int(info["error_code"]) == 0
    assert info['username'] == login['username']
    assert info['secret'] == login['secret']

def test_mine(login):
    info = mine(login['username'], login['secret'])
    assert 'result' in info
    assert 'id' in info
    assert 'error' in info
    assert info['error'] is None

@pytest.fixture
def paidlogin(request, glob):
    login = register()
    info = deposit(login['username'], login['secret'])
    assert 'address' in info
    info2 = deposit(login['username'], login['secret'])
    assert info['address'] == info2['address']

    send_1btc(info['address'])

    info = balance(login['username'], login['secret'])
    assert 'balance' in info
    assert int(info["error_code"]) == 0
    assert float(info['balance']) > 0
    return login

@localonly
def test_deposit(paidlogin):
    info = balance(paidlogin['username'], paidlogin['secret'])
    assert info['balance'] != "0"
    assert int(info["error_code"]) == 0

@localonly
def test_payment(paidlogin):
    info = request(paidlogin['username'], paidlogin['secret'], 1)
    assert int(info["error_code"]) == 0
    assert 'paymentid' in info
    paymentid = info['paymentid']

    info = balance(paidlogin['username'], paidlogin['secret'])
    orig_b = info['balance']

    info = pay(paidlogin['username'], paidlogin['secret'], paymentid)
    assert 'code' in info
    assert info['code'].encode('utf-8') == md5(paymentid + paidlogin['secret'])
    assert int(info["error_code"]) == 0

    info = balance(paidlogin['username'], paidlogin['secret'])
    assert int(info["error_code"]) == 0
    assert orig_b == info['balance']

@localonly
def test_withdraw(login, paidlogin):
    info = deposit(login['username'], login['secret'])
    assert 'address' in info
    addr = info['address']

    info = balance(login['username'], login['secret'])
    assert "balance" in info
    orig_balance = float(info["balance"])

    info = balance(paidlogin['username'], paidlogin['secret'])
    assert "balance" in info
    orig_balance_paid = float(info["balance"])

    info = withdraw(paidlogin['username'], paidlogin['secret'], addr, '0.5')
    assert int(info["error_code"]) == 0
    assert "id" in info

    info = balance(login['username'], login['secret'])
    assert "balance" in info
    assert float(info["balance"]) == 0.5 + orig_balance
    info = balance(paidlogin['username'], paidlogin['secret'])
    assert "balance" in info
    assert float(info["balance"]) == orig_balance_paid - 0.5

@localonly
def test_self_withdraw(paidlogin):
    info = deposit(paidlogin['username'], paidlogin['secret'])
    assert 'address' in info
    addr = info['address']

    info = balance(paidlogin['username'], paidlogin['secret'])
    assert "balance" in info
    orig_balance_paid = float(info["balance"])

    info = withdraw(paidlogin['username'], paidlogin['secret'], addr, '0.5')
    assert int(info["error_code"]) == 0

    info = balance(paidlogin['username'], paidlogin['secret'])
    assert "balance" in info
    assert float(info["balance"]) == orig_balance_paid

@localonly
def test_mine_handling(login):

    mine_user(login['username'])

    assert float(balance(login['username'], login['secret'])['balance']) == 1.0

    mine_addr = get_mine_addr()
    send_1btc(mine_addr)

    resp = run_newblock()
    assert login['username'] in resp

