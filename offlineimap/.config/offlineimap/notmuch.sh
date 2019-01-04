#!/bin/sh

notmuch new
notmuch tag +inbox -- folder:/INBOX/
notmuch tag +archive -inbox -- folder:/Archive/
notmuch tag +sent -inbox -- folder:/Sent/
notmuch tag +trash -inbox -archive -- folder:/Trash/

# Remove new tag from anything not in the inbox
notmuch tag -new -- NOT tag:inbox
