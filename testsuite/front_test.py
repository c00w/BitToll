#!/usr/bin/env python2
from testlib import *

def test_page_error():
    r = apicall("nonexistant", "hi")
    assert "error" in r
    assert int(r["error_code"]) == 1
    assert r["error"] != ""

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
    assert info['error'] is None

def pytest_funcarg__paidlogin(request):
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


def test_deposit(paidlogin):
    info = balance(paidlogin['username'], paidlogin['secret'])
    assert info['balance'] != "0"
    assert int(info["error_code"]) == 0

def test_payment(paidlogin):
    info = request(paidlogin['username'], paidlogin['secret'], 1)
    assert int(info["error_code"]) == 0
    assert 'payment' in info
    paymentid = info['payment']

    info = balance(paidlogin['username'], paidlogin['secret'])
    orig_b = info['balance']

    info = pay(paidlogin['username'], paidlogin['secret'], paymentid)
    assert 'code' in info
    assert info['code'].encode('utf-8') == md5(paymentid + paidlogin['secret'])
    assert int(info["error_code"]) == 0

    info = balance(paidlogin['username'], paidlogin['secret'])
    assert int(info["error_code"]) == 0
    assert orig_b == info['balance']

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

def test_mine_handling(login):

    mine_user(login['username'])

    assert float(balance(login['username'], login['secret'])['balance']) == 1.0

    mine_addr = get_mine_addr()
    send_1btc(mine_addr)

    resp = run_newblock()
    assert login['username'] in resp

