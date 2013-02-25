#!/usr/bin/env python2
from testlib import register, balance, mine, request, deposit, pay

ip_address = "vm"
port = "3000"
url = ''.join(['http://', ip_address, ':', port])

def test_login(login):
    info = login
    assert 'username' in info
    assert 'secret' in info
    assert len(info.keys()) == 2

def pytest_funcarg__login(request):
    info = register()
    assert 'username' in info
    assert 'secret' in info
    return info

def test_balance(login):
    info = balance(login['username'], login['secret'])
    assert 'balance' in info

def test_request(login):
    info = request(login['username'], login['secret'], amount=1)
    assert 'payment' in info

def test_mine(login):
    info = mine(login['username'], login['secret'])
    info = balance(login['username'], login['secret'])
    assert float(info['balance']) > 0

def pytest_funcarg__paidlogin(request, login):
    info = deposit(login['username'], login['secret'])
    assert 'address' in info
    info2 = deposit(login['username'], login['secret'])
    assert info['address'] == info2['address']

    print
    print 'Please send some bitcoins to %s' % info['address']
    print 'Then Hit Enter'
    raw_input()

    info = balance(login['username'], login['secret'])
    assert float(info['balance']) > 0
    return login


def test_deposit(paidlogin):
    info = balance(paidlogin['username'], paidlogin['secret'])
    assert info['balance'] != "0"

def test_payment(paidlogin):
    info = request(paidlogin['username'], paidlogin['secret'], 1)
    assert 'payment' in info
    paymentid = info['payment']

    info = balance(paidlogin['username'], paidlogin['secret'])
    orig_b = info['balance']

    info = pay(paidlogin['username'], paidlogin['secret'], paymentid)
    assert 'code' in info

    info = balance(paidlogin['username'], paidlogin['secret'])
    assert orig_b == info['balance']
