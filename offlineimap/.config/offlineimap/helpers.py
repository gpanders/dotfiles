#!/usr/bin/env python
from subprocess import check_output

def get_secret(account, linum):
    return check_output("pass " + account, shell=True).splitlines()[linum]

def get_pass(account):
    return get_secret(account, 0)
