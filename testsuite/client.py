#!/usr/bin/env python2
from __future__ import print_function
from testlib import *
import cmd
import readline

class BittollClient(cmd.Cmd):
    login = None
    prompt = "bt>"
    def do_server(self, arg):
        set_server(arg)
        print('Set server to %s' % arg)

    def do_port(self, arg):
        set_port(arg)
        print ('Set server to %s' % arg)

    def do_register(self, arg):
        print('Registering')
        info = register()
        self.login = (info['username'], info['secret'])
        print('user = %s'% self.login[0])
        print('secret = %s'% self.login[1])
        print( self.login)

    def do_balance(self, arg):
        info = balance(self.login[0], self.login[1])
        print( info['balance'])

    def do_withdraw(self, arg):
        if " " not in arg:
            print ("withdraw address amount")
            return
        address, amount = arg.split(' ', 1)
        info = withdraw(self.login[0], self.login[1], address, amount)
        print( info)

    def do_deposit(self, arg):
        info = deposit(self.login[0], self.login[1])
        print( info['address'])

    def do_request(self, arg):
        amount = int(arg)
        info = request(self.login[0], self.login[1], amount)
        print( info['payment'])

    def do_pay(self, arg):
        pid = arg
        info = pay(self.login[0], self.login[1], pid)
        print( info)

    def do_mine(self, arg):
        info = mine(self.login[0], self.login[1])
        print( info)

BittollClient().cmdloop("Bittoll Interactive Command Line\n")
