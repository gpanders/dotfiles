#!/usr/bin/env python
from subprocess import check_output

def get_pass(account):
    return check_output("pass " + account, shell=True).splitlines()[0]
